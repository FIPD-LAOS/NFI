# NFI III Server ####
function(input, output) { 
#Connect to NFI-Calc ####
  source("NFI-Calc-III.R")

# Sidebar Item 1 (Summary) ####
# Boxes ####
  output$finished <- renderInfoBox({
    infoBox(
      "Calculated plots", 
      paste0(N.prov.new$N_plots_calculated[19], " / ", (N.prov.new$N_Plots_initial[19])), 
      paste0(round((N.prov.new$N_plots_calculated[19]/(N.prov.new$N_Plots_initial[19])*100),0), "%"), 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "olive",
      fill = TRUE
    )
  })
  
  output$remaining <- renderInfoBox({
    infoBox(
      "Remaining plots",
      paste0(N.prov.new$Plots_remaining[19], " / ", N.prov.new$N_Plots_initial[19]), 
      paste0(round((N.prov.new$Plots_remaining_perCent[19]),0), "%"), 
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow",
      fill = TRUE
    )
  })
  
  output$excluded <- renderInfoBox({
    infoBox(
      "Excluded plots", 
      paste0(sum(ex.plots.end$N), " / ", N.prov.new$N_Plots_initial[19]),
      paste0(round((sum(ex.plots.end$N) / N.prov.new$N_Plots_initial[19])*100,0), "%"),  
      icon = icon("warning-sign", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
 
  # Graph ####
  select_var <- reactive({
    c(input$graph.x,input$graph.y)
  })
  
  dat.graph2 <- reactive({
    dat.graph1[,select_var()]
  })
  
  output$figure1 <- renderPlot({
    library(ggplot2)
    library(RColorBrewer)
    
    colfunc <- colorRampPalette(c("darkgreen", "yellow"))
    
  ggplot(dat.graph2(), aes(factor(dat.graph2()[,1]), dat.graph2()[,2]))  + 
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(aes(fill = dat.graph2()[,1])) + scale_fill_manual(values = c(colfunc(length(levels(as.factor(dat.graph2()[,1])))))) +
    ylab(paste0(names(dat.graph2()[2])," carbon t/ha")) +
    xlab(paste0(names(dat.graph2()[1]))) +
    theme_bw() +
    theme(legend.position = "none",
          #axis.ticks = element_blank(),
          axis.title = element_text(size=14, family="Lato", face = "bold"),
          axis.text = element_text(size=10, family="Lato"),
          panel.grid.major = element_line(colour = "grey85"), 
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(colour = "transparent")
    )
  })
  
  # Sidebar Item 2 (Tables) ####
  # Sidebar SubItem 1 (Summary Table) ####
  # Summary Forest type table ####
  var_input <- reactive({
    if(input$var_input1 == 1){
      c("Forest_Type")
    } else if (input$var_input1 == 2){
      c("Province_Name")
    } else if (input$var_input1 == 3){
      c("Forest_Type", "Province_Name")
    } else {c("Province_Name", "Forest_Type")}
  })
  
  # Summary table ####
  actualN <- reactive({
    summarySE(carbon.new, measurevar = "type", groupvars = c(var_input()))$N/8
  })
  
  # with Dead Wood 
  carbon.new.sum.pre <- reactive({
    summarySUMse(carbon.new, measurevar = "carbon", sdvar = "sd",groupvars = c(var_input(), "plot"))
  })
  
  carbon.new.sum <- reactive({
    a <- summarySE(
      carbon.new.sum.pre(),
      measurevar = "sum",
      
      groupvars = c(var_input())
    )
    
    a$ci <- 1.96*a$se
    a$uncert <- (a$ci/a$mean)*100
    return(a)
  })
  
  carbon.min <- reactive({
    ifelse(length(var_input()) == 2 && var_input()[1] == "Province_Name",
           ddply(carbon.mima, .(carbon.mima[,2], carbon.mima[,3]), function(carbon.mima) min(carbon.mima$sum[carbon.mima$sum > 0]))[3],
           ifelse(length(var_input()) == 2 && var_input()[2] == "Province_Name",
                  ddply(carbon.mima, .(carbon.mima[,3], carbon.mima[,2]), function(carbon.mima) min(carbon.mima$sum[carbon.mima$sum > 0]))[3],
                  ifelse(length(var_input()) == 1 && var_input()[1] == "Province_Name",
                         ddply(carbon.mima, .(carbon.mima[,2]), function(carbon.mima) min(carbon.mima$sum[carbon.mima$sum > 0]))[2],
                         ddply(carbon.mima, .(carbon.mima[,3]), function(carbon.mima) min(carbon.mima$sum[carbon.mima$sum > 0]))[2]
                  )
           )
    )
  })
  carbon.max <- reactive({
    ifelse(length(var_input()) == 2 && var_input()[1] == "Province_Name",
           ddply(carbon.mima, .(carbon.mima[,2], carbon.mima[,3]), function(carbon.mima) max(carbon.mima$sum[carbon.mima$sum > 0]))[3],
           ifelse(length(var_input()) == 2 && var_input()[2] == "Province_Name",
                  ddply(carbon.mima, .(carbon.mima[,3], carbon.mima[,2]), function(carbon.mima) max(carbon.mima$sum[carbon.mima$sum > 0]))[3],
                  ifelse(length(var_input()) == 1 && var_input()[1] == "Province_Name",
                         ddply(carbon.mima, .(carbon.mima[,2]), function(carbon.mima) max(carbon.mima$sum[carbon.mima$sum > 0]))[2],
                         ddply(carbon.mima, .(carbon.mima[,3]), function(carbon.mima) max(carbon.mima$sum[carbon.mima$sum > 0]))[2]
                  )
           )
    )
  })
  
  carbon.new.sum.pre90 <- reactive({
    summarySUMse90(carbon.new.90, measurevar = "carbon", sdvar = "sd",groupvars = c(var_input(), "plot"))
  })
  
  carbon.new.sum.90 <- reactive({
    b <- summarySE(
      carbon.new.sum.pre90(),
      measurevar = "sum",
      
      groupvars = c(var_input())
    )
    
    b$ci <- 1.645*b$se
    b$uncert <- (b$ci/b$mean)*100
    return(b)
  })
  
  carbon.new.summary.2 <- reactive({
    c1 <- cbind.data.frame(
      carbon.new.sum()[,1],
      carbon.new.sum()[,2],
      round(carbon.new.sum()$N, 0),
      round(carbon.new.sum()$mean,2),
      round(carbon.new.sum()$sd,2),
      round(carbon.new.sum()$se,2),
      round(carbon.new.sum()$ci,2),
      round(carbon.new.sum()$uncert,2),
      round(carbon.new.sum.90()$ci,2),
      round(carbon.new.sum.90()$uncert,2),
      round(carbon.min()[[1]],2),
      round(carbon.max()[[1]],2)
    )
    names(c1) <- c(names(carbon.new.sum()[1]), names(carbon.new.sum()[2]), "N", "Total Carbon (AGB + BGB + DW; t/ha)", "S.D.", "S.E.", "CI (95%)", "Uncertainty (95%)", "CI (90%)", "Uncertainty (90%)", "C min (t/ha)", "C max (t/ha)")
    return(c1)
  })
  
  carbon.new.summary.1 <- reactive({
    c2 <- cbind.data.frame(
      carbon.new.sum()[,1],
      round(carbon.new.sum()$N, 0),
      round(carbon.new.sum()$mean,2),
      round(carbon.new.sum()$sd,2),
      round(carbon.new.sum()$se,2),
      round(carbon.new.sum()$ci,2),
      round(carbon.new.sum()$uncert,2),
      round(carbon.new.sum.90()$ci,2),
      round(carbon.new.sum.90()$uncert,2),
      round(carbon.min()[[1]],2),
      round(carbon.max()[[1]],2)
    )
    names(c2) <- c(names(carbon.new.sum()[1]), "N", "Total Carbon (AGB + BGB + DW; t/ha)", "S.D.", "S.E.", "CI (95%)", "Uncertainty (95%)", "CI (90%)", "Uncertainty (90%)", "C min (t/ha)", "C max (t/ha)")
    return(c2)
  })
  
  # output table
  output$con_table <- renderText({
    ifelse(length(var_input()) == 2,2,
           ifelse(length(var_input()) == 1,1,
                  0
           )
    )
  })
  outputOptions(output, 'con_table', suspendWhenHidden=FALSE)
  
  
  output$table2 <- renderTable({
    carbon.new.summary.2()
  })
  output$table1 <- renderTable({
    carbon.new.summary.1()
  })
  
  output$table1.text <- renderText({
    paste("Please select at least one variable to sort by!")
  })
  
  output$downloadReport1 <- downloadHandler(  filename = function() {
    paste("NFI_summary_data_", names(carbon.new.sum()[1]), "_",Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(carbon.new.summary.1(), file,  row.names=FALSE)
  }
  )
  
  output$downloadReport2 <- downloadHandler(  filename = function() {
    paste("NFI_summary_data_", names(carbon.new.sum()[1]), "_", names(carbon.new.sum()[2]), "_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(carbon.new.summary.2(), file, row.names=FALSE)
  }
  )
  
  #without Dead Wood
  var_input2 <- reactive({
    if(input$var_input1.noDW == 1){
      c("Forest_Type")
    } else if (input$var_input1.noDW == 2){
      c("Province_Name")
    } else if (input$var_input1.noDW == 3){
      c("Forest_Type", "Province_Name")
    } else {c("Province_Name", "Forest_Type")}
  })
  
  carbon.new.noDW <-   carbon.new[carbon.new$biomass %in% c("AGB", "BGB"),]
  
  carbon.new.sum.pre.noDW <- reactive({
    summarySUMse(carbon.new.noDW, measurevar = "carbon", sdvar = "sd",groupvars = c(var_input2(), "plot"))
  })
  
  carbon.new.sum.noDW <- reactive({
    d <- summarySE(
      carbon.new.sum.pre.noDW(),
      measurevar = "sum",
      
      groupvars = c(var_input2())
    )
    
    d$ci <- 1.96*d$se
    d$uncert <- (d$ci/d$mean)*100
    return(d)
  })
  
  carbon.min.noDW <- reactive({
    ifelse(length(var_input2()) == 2 && var_input2()[1] == "Province_Name",
           ddply(carbon.mima.noDW, .(carbon.mima.noDW[,2], carbon.mima.noDW[,3]), function(carbon.mima.noDW) min(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[3],
           ifelse(length(var_input2()) == 2 && var_input2()[2] == "Province_Name",
                  ddply(carbon.mima.noDW, .(carbon.mima.noDW[,3], carbon.mima.noDW[,2]), function(carbon.mima.noDW) min(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[3],
                  ifelse(length(var_input2()) == 1 && var_input2()[1] == "Province_Name",
                         ddply(carbon.mima.noDW, .(carbon.mima.noDW[,2]), function(carbon.mima.noDW) min(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[2],
                         ddply(carbon.mima.noDW, .(carbon.mima.noDW[,3]), function(carbon.mima.noDW) min(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[2]
                  )
           )
    )
  })
  carbon.max.noDW <- reactive({
    ifelse(length(var_input2()) == 2 && var_input2()[1] == "Province_Name",
           ddply(carbon.mima.noDW, .(carbon.mima.noDW[,2], carbon.mima.noDW[,3]), function(carbon.mima.noDW) max(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[3],
           ifelse(length(var_input2()) == 2 && var_input2()[2] == "Province_Name",
                  ddply(carbon.mima.noDW, .(carbon.mima.noDW[,3], carbon.mima.noDW[,2]), function(carbon.mima.noDW) max(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[3],
                  ifelse(length(var_input2()) == 1 && var_input2()[1] == "Province_Name",
                         ddply(carbon.mima.noDW, .(carbon.mima.noDW[,2]), function(carbon.mima.noDW) max(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[2],
                         ddply(carbon.mima.noDW, .(carbon.mima.noDW[,3]), function(carbon.mima.noDW) max(carbon.mima.noDW$sum[carbon.mima.noDW$sum > 0]))[2]
                  )
           )
    )
  })
  
  carbon.new.90.noDW <-   carbon.new.90[carbon.new.90$biomass %in% c("AGB", "BGB"),]
  
  carbon.new.sum.pre90.noDW <- reactive({
    summarySUMse90(carbon.new.90, measurevar = "carbon", sdvar = "sd",groupvars = c(var_input2(), "plot"))
  })
  
  carbon.new.sum.90.noDW <- reactive({
    c <- summarySE(
      carbon.new.sum.pre90.noDW(),
      measurevar = "sum",
      
      groupvars = c(var_input2())
    )
    
    c$ci <- 1.645*c$se
    c$uncert <- (c$ci/c$mean)*100
    return(c)
  })
  
  carbon.new.summary.2.noDW <- reactive({
    e1 <- cbind.data.frame(
      carbon.new.sum.noDW()[,1],
      carbon.new.sum.noDW()[,2],
      round(carbon.new.sum.noDW()$N, 0),
      round(carbon.new.sum.noDW()$mean,2),
      round(carbon.new.sum.noDW()$sd,2),
      round(carbon.new.sum.noDW()$se,2),
      round(carbon.new.sum.noDW()$ci,2),
      round(carbon.new.sum.noDW()$uncert,2),
      round(carbon.new.sum.90.noDW()$ci,2),
      round(carbon.new.sum.90.noDW()$uncert,2),
      round(carbon.min.noDW()[[1]],2),
      round(carbon.max.noDW()[[1]],2)
    )
    names(e1) <- c(names(carbon.new.sum.noDW()[1]), names(carbon.new.sum.noDW()[2]), "N", "Total Carbon (AGB + BGB; t/ha)", "S.D.", "S.E.", "CI (95%)", "Uncertainty (95%)", "CI (90%)", "Uncertainty (90%)", "C min (t/ha)", "C max (t/ha)")
    return(e1)
  })
  
  
  carbon.new.summary.1.noDW <- reactive({
    e2 <- cbind.data.frame(
      carbon.new.sum.noDW()[,1],
      round(carbon.new.sum.noDW()$N, 0),
      round(carbon.new.sum.noDW()$mean,2),
      round(carbon.new.sum.noDW()$sd,2),
      round(carbon.new.sum.noDW()$se,2),
      round(carbon.new.sum.noDW()$ci,2),
      round(carbon.new.sum.noDW()$uncert,2),
      round(carbon.new.sum.90.noDW()$ci,2),
      round(carbon.new.sum.90.noDW()$uncert,2),
      round(carbon.min.noDW()[[1]],2),
      round(carbon.max.noDW()[[1]],2)
    )
    names(e2) <- c(names(carbon.new.sum.noDW()[1]), "N", "Total Carbon (AGB + BGB; t/ha)", "S.D.", "S.E.", "CI (95%)", "Uncertainty (95%)", "CI (90%)", "Uncertainty (90%)", "C min (t/ha)", "C max (t/ha)")
    return(e2)
  })
  
  # output table
  output$con_tablenoDW <- renderText({
    ifelse(length(var_input2()) == 2,2,
           ifelse(length(var_input2()) == 1,1,
                  0
           )
    )
  })
  outputOptions(output, 'con_tablenoDW', suspendWhenHidden=FALSE)
  
  
  output$table2noDW <- renderTable({
    carbon.new.summary.2.noDW()
  })
  output$table1noDW <- renderTable({
    carbon.new.summary.1.noDW()
  })
  
  output$table1.text.noDW <- renderText({
    paste("Please select at least one variable to sort by!")
  })
  
  output$downloadReport1.noDW <- downloadHandler(  filename = function() {
    paste("NFI_summary_data_noDW_", names(carbon.new.sum.noDW()[1]), "_",Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(carbon.new.summary.1.noDW(), file,  row.names=FALSE)
  }
  )
  
  output$downloadReport2.noDW <- downloadHandler(  filename = function() {
    paste("NFI_summary_data_noDW_", names(carbon.new.sum.noDW()[1]), "_", names(carbon.new.sum.noDW()[2]), "_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(carbon.new.summary.2.noDW(), file, row.names=FALSE)
  }
  )
  
  # Sidebar SubItem 2 (Plots) ####
  output$plotTable <- renderDataTable({
    plot1 <- summarySE(
      summarySUMse(carbon.new.sub, measurevar = "carbon",sdvar = "sd", groupvars = c("plot", "subplot", "type")),
      measurevar = "sum",
      groupvars = c("plot", "type")
    )
    names(plot1) <- c("Plot ID", "Forest Type",  "N","Carbon (t/ha)", "S.D.", "S.E.", "CI (95%)")
    return(plot1)
  })
  
  # Sidebar SubItem 3 (Sub Plots) ####
  output$subplotTable <- renderDataTable({
    sub <- carbon.new.sub[,c(2,3,4,1,6,7,8)]
    names(sub) <- c("Plot ID", "Subplot ID", "Forest Type", "Biomass Component", "Biomass (t/ha)", "Carbon (t/ha)", "Date")
    return(sub)
  })
  
  # Sidebar SubItem 4 (Excluded Plots) ####
  output$explotText0 <- renderText({
    paste0(" The tables below shows the plots excluded from the analysis accoring to the following rules:"
    )
  })
  output$explotText1 <- renderText({
    paste(" 1. Non-Forest, where the majority of the subplot is of non-forest type")
  })
  output$explotText2 <- renderText({
    paste(" 2. Conflicting Forest Type, where not one forest type is prominent, e.g. DD, DD, EF, EF, or MDF, non-forest, EF, DD")
  })
  output$explotText3 <- renderText({
    paste(" 3. Conflicting subplot descriptions, where there are 2 or more of one subplot descriptions, e.g. twice subplot_A")
  })
  
  output$explotText4 <- renderText({
    paste(" 4. Inaccessibility, i.e. Slope was too steep, Accessing area was too dangerous, i.e. UXO, or other safety reason, Distance was too far, not enough supplies, or Ground access was restricted or denied")
  })
  
  output$explotTextIntroexplotTable <- renderText({
    paste0("Summary Table")
  })
  
  output$explotTable <- renderTable({
    ex <- ex.plots.end[,c(1,2)]
    names(ex) <- c("Reason for Exclusion", "Plot ID")
    return(ex)
  }, digits = 0)
  
  
  output$explotTextIntroexplotTableProv <- renderText({
    paste0("Detailed Table")
  })
  
  output$explotTableProv <- renderTable({
    ex2 <- ex.plot.prov[,c(1,3,2)]
    names(ex2) <- c("Plot ID", "Province Name","Reason for Exclusion")
    return(ex2)
  }, digits = 0)

  # Sidebar Item 3 (Raw data) ####
  output$introRawData <- renderText({
    paste("The raw data are not the raw data one would download from the ONA website. Instead, the rules of inclusion and exclusion are applied, and biomass is already calculated.")
  })
  output$download_lt <- downloadHandler(  filename = function() {
    paste("Living_trees_raw", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(living_trees_co, file, row.names = FALSE)
  }
  )
  
  output$download_dt <- downloadHandler(  filename = function() {
    paste("Dead_trees_raw", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dead_trees.co, file, row.names = FALSE)
  }
  )
  
  output$download_stu <- downloadHandler(  filename = function() {
    paste("Stumps_raw", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(stump_co, file, row.names = FALSE)
  }
  )
  
  # Download pre-processed data ####
  output$introPreData <- renderText({
    paste("The pre-processed data include the summary by subplot, carbon content calculations, and a column indicationg the province in which the sub-plots were measured.")
  })
  
  output$download_lt2 <- downloadHandler(  filename = function() {
    paste("Living_trees_processed", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tree_carbon2.subProv, file, row.names = FALSE)
  }
  )
  
  output$download_dt2 <- downloadHandler(  filename = function() {
    paste("Dead_trees_processed", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dead_tree_carbon.subProv, file, row.names = FALSE)
  }
  )
  
  output$download_stu2 <- downloadHandler(  filename = function() {
    paste("Stumps_processed", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(stump_carbon.subProv, file, row.names = FALSE)
  }
  )
  
  # Sidebar Item 4 (Map) ####
  #Load map data ####
  coord <- cbind.data.frame(
    "X" = NFI$plot_GPS._GPS_longitude,
    "Y" = NFI$plot_GPS._GPS_latitude,
    "plot" = NFI$plot_info.plot_code_nmbr,
    "subplot" = NFI$plot_info.sub_plot
  )
  coord <- coord[!is.na(coord$X),]
  
  
  # Create map ####
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM(default)") %>% 
      addProviderTiles("Esri.WorldImagery", group = "ESRI") %>% 
      addLayersControl(baseGroup = c("OpenStreetMap", "ESRI")) %>%
      addCircleMarkers(data = coord, lat = ~ Y, lng = ~ X,
                       color = ~c("green"),
                       popup = ~paste(coord$plot, coord$subplot),
                       clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                                             spiderfyOnMaxZoom = TRUE))
  })

  # Sidebar Item 5 (Photos)
  # create funtion to turn image path to HTML code
  createLink <- function(val) {
    sprintf('<a href="http://classic.ona.io/api/v1/files/%s" target="_blank">Photo</a>',val)
  }
  # create table with Plot Number, Subplot, Tree-Plot Photo, and Canopy Photo 
  output$tablePhoto <- renderDataTable({
    
    my_table <- cbind(NFI[,c(6,8,28,3)])
    names(my_table) <- c("Plot Number", "Sub_Plot", "Land Use Class", "Date")
    my_table$Tree_Plot <- createLink(substr(NFI$plot_GPS.photo, 36, length(NFI$plot_GPS.photo)))
    my_table$Canopy <- createLink(substr(NFI$canopy.photo_can, 36, length(NFI$canopy.photo_can)))
    
    my_table$Tree_Plot <- ifelse(is.na(my_table$'Land Use Class') == T, NA, my_table$Tree_Plot)
    my_table$Canopy <- ifelse(is.na(my_table$'Land Use Class') == T, NA, my_table$Canopy)
    return(my_table)
    
  }, escape = FALSE)
  
  ### end server
}