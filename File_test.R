runApp(
  list(ui = fluidPage(
    uiOutput("tab"),
      sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
          dataTableOutput('tablePhoto')
        )
      )
  ),
  server = function(input, output, session){
    source("NFI-Calc-III.R")
    
    createLink <- function(val) {
      sprintf('<a href="http://classic.ona.io/api/v1/files/%s">Photo</a>',val)
    }

    output$tablePhoto <- renderDataTable({
      
      my_table <- cbind(NFI[,c(6,8,28,3)])
      names(my_table) <- c("Plot Number", "Sub_Plot", "Land Use Class", "Date")
      my_table$Tree_Plot <- createLink(substr(NFI$plot_GPS.photo, 36, length(NFI$plot_GPS.photo)))
      my_table$Canopy <- createLink(substr(NFI$canopy.photo_can, 36, length(NFI$canopy.photo_can)))
      
      my_table$Tree_Plot <- ifelse(is.na(my_table$'Land Use Class') == T, NA, my_table$Tree_Plot)
      my_table$Canopy <- ifelse(is.na(my_table$'Land Use Class') == T, NA, my_table$Canopy)
      return(my_table)
      
    }, escape = FALSE)
    
    output$download_pic <- downloadHandler(  filename = function() {
      paste("Pictures", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(stump_carbon.subProv, file, row.names = FALSE)
    }
    )
  })
)