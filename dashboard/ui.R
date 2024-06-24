# ~~~~~~~~~ user interface (UI) ~~~~~~~~~ #

source("global.R")

ui <- dashboardPage(
  
  dashboardHeader(title = span("NOAA Pacific Biosampling Inventory", style = "font-size: 15px"),
                  titleWidth = 260), 
  
  dashboardSidebar(
    #tags$head(tags$style(".sidebar-menu li { margin-left: 100px; }")),
    width = 260,
    sidebarMenu(id = "tabs",
    menuItem("Start Here", tabName = "landingpage", icon = icon("star")),
    menuItem("Biosampling Inventory", tabName = "Biosampling Inventory", icon = icon("database")), 
    menuItem("Life History", tabName = "Life History", icon = icon("fish-fins"))
    )),
    
  
  dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")), # END header
                tabItems(
                  tabItem(tabName = "landingpage",
                          class = "active",
                          tags$img(class = "banner",
                                   src = "media/ehu.png",
                                   height = "10%",
                                   width = "100%",
                                   alt = "Photo if a snapper being measured."),
                          
                          fluidRow(box(width = 12, includeHTML(rmarkdown::render("text/landing_page_text.Rmd"))))),
                                       # htmltools::includeMarkdown("text/landing_page_text.md")

######### ----------------------------------------------------------
######### Biological Data Inventory Tab
######### ----------------------------------------------------------               
                  
                  tabItem(tabName = "Biosampling Inventory",
                          fluidRow(column(12, h2("Biosampling Inventory"))), 
                          tabPanel(title = "Guam",
                                                    fluidRow(
                                                   box(width = 12, 
                                                       h3("Choose a species:"),
                                                       column(4,
                                                              wellPanel(
                                                                pickerInput("Species", 
                                                                            choices = unique(species_list),
                                                                            selected = c("Aphareus rutilans")))),
                                                    
                                                        fluidRow(
                                                         
                                                         column(width = 12,
                                                                uiOutput("data_text_guam"))),
                                                         column(width = 12,
                                                                plotOutput("inventory_plot")))))),
                                                    
                                        
                                        
######### ----------------------------------------------------------
######### Life History Tab
######### ----------------------------------------------------------
                tabItem(tabName = "Life History",
                        fluidRow(column(12, h2("Life History"))), 
                        tabsetPanel(
                          tabPanel(title = "Life History",
                                   fluidRow(
                                     box(width = 8,
                                         includeHTML(rmarkdown::render(("text/Life_History.Rmd"))))))))
)))
                          

