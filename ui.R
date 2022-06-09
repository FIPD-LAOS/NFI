# NFI III Interface ####
# Load required packages ####
library(shiny)
library(shinydashboard)
library(xtable)
library(leaflet)

#Load inputs for the UI####    
ui.inputs <- read.csv("data/ui-inputs.csv", header=T)

dashboardPage(
  #Appearance ####
  skin = c("black"),
  # Header
  dashboardHeader(title = "Laos NFI III dashboard"),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary",  icon = icon("tree-deciduous", lib = "glyphicon")), # add dashboard menu items here (including comma)
      menuItem("Tables",  icon = icon("list-alt", lib = "glyphicon"),
               menuSubItem("Summary Table", tabName = "table"),
               menuSubItem("Summary Table without DW", tabName = "tablenoDW"),
               menuSubItem("Subplot Table", tabName = "table2"),
               menuSubItem("Plot Table", tabName = "table3"),
               menuSubItem("Excluded Plots", tabName = "table4")
      ),
      menuItem("Raw data", tabName = "raw",  icon = icon("grain", lib = "glyphicon")),
      menuItem("Map", tabName = "map",  icon = icon("screenshot", lib = "glyphicon")),
      menuItem("Photos", tabName = "photos", icon = icon("picture", lib = "glyphicon")
               )
      )
  ),
  #Body content ####
  dashboardBody(
    tabItems(
      # Sidebar item 1 (Summary) ####
      tabItem(tabName = "summary",style = "overflow-y: auto; max-height: 90vh",
              box(width = 12,
                     title = h2("Progress"),
                              fluidRow(
                                infoBoxOutput("finished", width = 4),
                                infoBoxOutput("remaining", width = 4),
                                infoBoxOutput("excluded", width = 4)
                              )
              ),
              box(width = 12, #height = "600px",
                  h2("Graph"),
                  fluidRow(
                    box(width = 6, solidHeader = TRUE,
                        tags$div(align = 'left', 
                                 selectInput("graph.x", "", 
                                             
                                             selected = c(as.character(ui.inputs$graph.x[1])),
                                             c(as.character(ui.inputs$graph.x[ui.inputs$graph.x != c("")]))
                                 )
                        )
                    ),
                    box(width = 6, solidHeader = TRUE,
                        tags$div(align = 'left', 
                                 selectInput("graph.y", "", 
                                             selected = c(as.character(ui.inputs$graph.y[1])),
                                             c(as.character(ui.inputs$graph.y[ui.inputs$graph.y != c("")]))
                                 )
                        )
                    ),
                    box(width = 12, solidHeader = FALSE,
                        plotOutput("figure1")
                        )
                    )
              )
      ),  # Summary tab ends here
      
      # Sidebar item 2 (Table) ####
      # Sidebar SubItem 1 (Summary Table) ####
      tabItem(tabName = "table", style = "overflow-x: auto",
              h1("Summary Table"),
              fluidRow(
                box(width = 12,
                    tags$div(align = 'left', 
                             radioButtons("var_input1", label = h4("Sort by"), 
                                          choices = c("Forest Type" = 1,
                                                      "Province" = 2, 
                                                      "Forest type and Province" = 3,
                                                      "Province and Forest type" = 4
                                          ),
                                          selected = 1,
                                          inline = TRUE)
                    )
                )
              ),
              
              conditionalPanel("output.con_table == 1",
                               downloadButton('downloadReport1', 'Download table')
              ),
              conditionalPanel("output.con_table == 2",
                               downloadButton('downloadReport2', 'Download table')
              ),
              
              tags$head(tags$style("#table1{padding: 3px;
                                   font-size: 12px;
                                   }"
                         )
              ),
              tags$head(tags$style("#table2{padding: 3px;
                                   font-size: 12px;
                                   }"
                         )
              ),
              
              fluidRow(
                conditionalPanel("output.con_table == 1",
                                 tableOutput("table1")
                ),
                conditionalPanel("output.con_table == 2",
                                 tableOutput("table2")
                ),
                conditionalPanel("output.con_table == 0",
                                 strong(em(h3((textOutput("table1.text")))))
                )
              )
      ),
              
              # Sidebar SubItem 1.2 (Summary Table no DW) ####
              tabItem(tabName = "tablenoDW", style = "overflow-x: auto",
                      h1("Summary Table without Dead Wood"),
                      fluidRow(
                        box(width = 12,
                            tags$div(align = 'left', 
                                     radioButtons("var_input1.noDW", label = h4("Sort by"), 
                                                  choices = c("Forest Type" = 1,
                                                              "Province" = 2, 
                                                              "Forest type and Province" = 3,
                                                              "Province and Forest type" = 4
                                                  ),
                                                  selected = 1,
                                                  inline = TRUE)
                            )
                        )
                      ),
              
                      conditionalPanel("output.con_tablenoDW == 1",
                                       downloadButton('downloadReport1.noDW', 'Download table')
                      ),
                      conditionalPanel("output.con_tablenoDW == 2",
                                       downloadButton('downloadReport2.noDW', 'Download table')
                      ),
                      
              tags$head(tags$style("#table1noDW{padding: 3px;
                                   font-size: 12px;
                                   }"
                                   )
              ),
              tags$head(tags$style("#table2noDW{padding: 3px;
                                   font-size: 12px;
                                   }"
              )
              ),
              
              fluidRow(
                
                conditionalPanel("output.con_tablenoDW == 1",
                                 tableOutput("table1noDW")
                ),
                conditionalPanel("output.con_tablenoDW == 2",
                                 tableOutput("table2noDW")
                ),
                conditionalPanel("output.con_tablenoDW == 0",
                                strong(em(h3((textOutput("table1.text.noDW")))))
                )
              )
      ),
      
      # Sidebar SubItem 2 (Subplot plots) ####
      tabItem(tabName = "table2",
              h1("Subplot Table"),
              box(width = 12,
                  h4("The tables below shows the plots including their subplots.")
              ),
              br(),
              box(width = 12,
                  br(),
                  dataTableOutput("subplotTable")
              )
      ), # Table tab ends here
      # Sidebar SubItem 3 (Plot plots) ####
      tabItem(tabName = "table3",
              h1("Plot Table"),
              box(width = 12,
                  h4("The tables below shows the plots as average of the subplots.")
              ),
              br(),
              box(width = 12,
                  br(),
                  dataTableOutput("plotTable")
              )
      ), # Table tab ends here
      # Sidebar SubItem 3 (Excluded plots) ####
      tabItem(tabName = "table4",
              h1("Excluded Plots"),
              box(width = 12,
                  h4(textOutput("explotText0")),
                  textOutput("explotText1"),
                  textOutput("explotText2"),
                  textOutput("explotText3"),
                  textOutput("explotText4")
              ),
              br(),
              box(width = 12,
                  h4(textOutput("explotTextIntroexplotTable")),
                                tableOutput("explotTable")
                  ),
              br(),
              box(width = 12,
                  h4(textOutput("explotTextIntroexplotTableProv")),
                                tableOutput("explotTableProv")
              )
      ), # Table tab ends here
      
      # Sidebar item 3 (Raw data) ####
      tabItem(tabName = "raw",style = "overflow-y: auto; max-height: 90vh",
              box(width = 12,
                  h1("Download raw data"),
                  h5(textOutput("introRawData")),
                  box(width = 4,
                      h4("Living trees"),
                      downloadButton('download_lt', 'Download')
                  ),
                  box(width = 4,
                      h4("Dead trees"),
                      downloadButton('download_dt', 'Download')
                  ),
                  box(width = 4,
                      h4("Stumps"),
                      downloadButton('download_stu', 'Download')
                  )
              ),
              br(),
              box(width = 12,
                  h1("Download pre-processed data"),
                  h5(textOutput("introPreData")),
                  box(width = 4,
                      h4("Living trees"),
                      downloadButton('download_lt2', 'Download')
                  ),
                  box(width = 4,
                      h4("Dead trees"),
                      downloadButton('download_dt2', 'Download')
                  ),
                  box(width = 4,
                      h4("Stumps"),
                      downloadButton('download_stu2', 'Download')
                  )
              )
      ), # Raw data tab end here
      
      # Sidebar item 4 (Map) ####
      tabItem(tabName = "map",
              h2("Map of Tree-Plot Locations"),
              leafletOutput("map", width = "100%", height = "800px")
              ),
      
      # Sidebar item 5 (Photos) ####
      tabItem(tabName = "photos",
              box(width = 12, 
                  h2("Table of all submitted Tree-Plot and Canopy Photos"),
                  br(),
                  dataTableOutput('tablePhoto')
                  )
              )
      ) # tab items end
    ) # dashboardBody end
  ) # dashboardPage end