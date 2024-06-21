# ~~~~~~~~~ user interface (UI) ~~~~~~~~~ #

source("global.R")

ui <- dashboardPage(
  
  dashboardHeader(title = span("American Samoa Data Integration", style = "font-size: 15px"),
                  titleWidth = 260), 
  
  dashboardSidebar(
    #tags$head(tags$style(".sidebar-menu li { margin-left: 100px; }")),
    width = 260,
    sidebarMenu(id = "tabs",
    menuItem("Start Here", tabName = "landingpage", icon = icon("star")),
    menuItem("Data Inventory", tabName = "datainventory", icon = icon("database")), 
    menuItem("Benthic Cover", tabName = "benthic", icon = icon("certificate")),
    menuItem("Fish", tabName = "fish", icon = icon("fish-fins")),
    menuItem("Environmental", tabName = "environmental", icon = icon("water")),
   # menuItem("Satellite Data", tabName = "satellite", icon = icon("satellite")),
    menuItem("Events Timeline", tabName = "eventstimeline", icon = icon("timeline")),
    menuItem("Resources", tabName = "resources", icon = icon("book-open"))
    )),
  
  dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")), # END header
                tabItems(
                  tabItem(tabName = "landingpage",
                          class = "active",
                          tags$img(class = "banner",
                                   src = "media/landing_page_banner.png",
                                   height = "10%",
                                   width = "100%",
                                   alt = "A panoramic photo of a bay in American Samoa viewed from the beach ."),
                          
                          fluidRow(box(width = 12, includeHTML(rmarkdown::render("text/landing_page_text.Rmd"))))),
                                       # htmltools::includeMarkdown("text/landing_page_text.md")

######### ----------------------------------------------------------
######### Biological Data Inventory Tab
######### ----------------------------------------------------------               
                  
                  tabItem(tabName = "datainventory",
                          fluidRow(column(12, h2("Biological and Environmental Data Inventory"))), 
                          tabsetPanel(
                            tabPanel(title = "About Data Collection",
                                       fluidRow(
                                       box(width = 8,
                                           includeHTML(rmarkdown::render(("text/about_data_inventory_text.Rmd")))))),
                            
                            
                            tabPanel(title = "About Data Collectors",
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render(("text/about_data_collectors_text.Rmd"))))),
                                     
                                     fluidRow(
                                       box(width = 12, 
                                           h3("See when data types were collected (only data included in tool is represented)"),
                                       column(4,
                                                     wellPanel(
                                                       pickerInput("pickIslandDataSourceTimeline",
                                                                   "Island",
                                                                   choices = unique(datasource_timeline$island),
                                                                   multiple = FALSE,
                                                                   selected = c("Tutuila")))),
                                              column(4,
                                                     wellPanel(
                                                       pickerInput("pickDTSourceTimeline",
                                                                   "Data Type",
                                                                   choices = unique(datasource_timeline$data_type),
                                                                   multiple = TRUE,
                                                                   selected = c("benthic cover")))),
                                              column(4,
                                                     wellPanel(
                                                       pickerInput("pickTimePeriodDataSourceTimeline",
                                                                   "Time Period",
                                                                   choices = unique(datasource_timeline$time_period),
                                                                   selected = c("Post-2000")))),
                                     fluidRow(
                                       
                                           column(width = 12,
                                                     plotlyOutput("datasource_timeline_scatter_plot")))))),
                            
                            
                            tabPanel(title = "Explore Data Inventory",
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render(("text/data_inventory_text.Rmd")))),
                                       valueBoxOutput("inventory_total_box")),
                                     fluidRow(column(12,
                                                     column(width = 4,
                                                            fluidRow(column(width = 6,
                                                                 wellPanel(
                                                                   radioButtons("pickIslandDI",
                                                                                "Island",
                                                                                choices = unique(data_inventory$island),
                                                                                selected = c("Tutuila")))),
                                                          column(width = 6,
                                                                 wellPanel(
                                                                   checkboxGroupInput("pickDataTypeDI",
                                                                                      "Data Type",
                                                                                      choices = unique(data_inventory$data_type),
                                                                                      selected = c("benthic cover"))))),
                                                 fluidRow(column(width = 12,
                                                                 wellPanel(
                                                                   # setSliderColor(color = c("#002364", "#002364"), sliderId = c(1, 2)),
                                                                   sliderInput("checkYearDI","Year",
                                                                               min = min(data_inventory$year),
                                                                               max = max(data_inventory$year),
                                                                               step = 1,
                                                                               value = c(1917,2021),
                                                                               sep = "")))),
                                                 fluidRow(column(width = 12,
                                                                 wellPanel(
                                                                   pickerInput("chooseVillageDI","Village",
                                                                               choices = sort(unique(data_inventory$nearest_village)),
                                                                               options = list(`actions-box` = TRUE),
                                                                               multiple = T,
                                                                               selected = unique(data_inventory$nearest_village))))),
                                
                                                 
                                                 fluidRow(column(width = 12,
                                                                 wellPanel(
                                                                   checkboxGroupInput("checkProviderDImonitoring",
                                                                                      "Monitoring Data Sources",
                                                                                      choices = source_type_monitoring,
                                                                                      selected = c("CRAG", "NPS AS", "NOAA - NCRMP"),
                                                                                      inline = TRUE)))),
                                                 fluidRow(column(width = 12,
                                                                 wellPanel(
                                                                   checkboxGroupInput("checkProviderDIstudies",
                                                                                      "Studies Data Sources",
                                                                                      choices = source_type_studies,
                                                                                     # selected = c("CRAG", "ASNPS", "NOAA - NCRMP"),
                                                                                      inline = TRUE))))),
                                          column(8,
                                                 wellPanel(leaflet::leafletOutput("inventory_map", height = 700),
                                                           p("When map is blank there is no avaliable data for selected inputs. Choose more or different inputs.")))))))),
              
######### ----------------------------------------------------------
######### Benthic Exploration Tab
######### ----------------------------------------------------------

                  tabItem(tabName = "benthic",
                          fluidRow(column(12,
                                          h2("Benthic Cover Data"))) ,    
                          tabsetPanel(
                            
                            tabPanel(title = "About Benthic Cover",
                                     fluidRow(
                                       box(width = 12,
                                     includeHTML(rmarkdown::render("text/benthic_cover_about_text.Rmd"))))
                                     
                                     ),
            
                            tabPanel(title = "Island Cover Over Time",
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render("text/benthic_cover_island_time_text.Rmd")))),
                                     fluidRow(column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseIslandSumBC","Island",
                                                                   choices = unique(benthic_island_summarized$island),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = unique(benthic_island_summarized$island)))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseDepthStrataBCSum","Depth Strata",
                                                                   choices = sort(unique(benthic_island_summarized$depth_strata_long)),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = "middle (10-20 m)"))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseGroupBCSum","Benthic Category",
                                                                   choices = sort(unique(benthic_island_summarized$group)),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = unique(benthic_island_summarized$group))))),
                                     fluidRow(box(width = 12,
                                                  column(width = 10,
                                                     plotlyOutput("benthic_cover_line_plot", height = "1000")),
                                              column(width = 2,
                                                     tags$img(
                                                       src = "media/benthic_legend_long.png",
                                                       width = "70%"
                                                     ))))),
                            tabPanel(title = "Village Cover Over Time",
                                     fluidRow(box(width = 12,
                                                  includeHTML(rmarkdown::render("text/benthic_cover_village_time_text.Rmd")))),
                                     fluidRow(column(width = 3,
                                                     wellPanel(
                                                       selectInput("inputVillageBC", "Village",
                                                                   choices = unique(benthic_cover_group$nearest_village),
                                                                   selected = "Vatia"))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseYearBoxBC","Year(s)",
                                                                   choices = sort(unique(benthic_cover_group$year)),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = unique(benthic_cover_group$year)))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseBenthicGroup","Benthic Categories",
                                                                   choices = unique(benthic_cover_group$group),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = c("Hard coral", "CCA", "Macroalgae")))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseDepthStrataBC","Depth Strata",
                                                                   choices = unique(benthic_cover_group$depth_strata_long),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = F,
                                                                   selected = c("middle (10-20 m)"))))),
                                     fluidRow(column(width = 12,
                                                     wellPanel(style = "background: white",
                                                               fluidRow(column(width = 12,
                                                                               plotlyOutput("benthic_cover_scatter_plot", height = 600))),
                                                               fluidRow(column(width = 12,
                                                                               tags$img(
                                                                                 src = "media/benthic_legend.png",
                                                                                 width = "70%"
                                                                               ))))))),
                            
                            tabPanel(title = "Island Cover Spatial",
                                     fluidRow(box(width = 12,
                                                  includeHTML(rmarkdown::render("text/benthic_cover_island_spatial_text.Rmd")))),
                                     fluidRow(column(width = 3,
                                                     wellPanel(
                                                       pickerInput("inputIslandBC_spatial", "Island",
                                                                   choices = unique(benthic_cover_group$island),
                                                                   selected = "Tutuila",
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseYearBoxBC_spatial","Year(s)",
                                                                   choices = sort(unique(benthic_cover_group$year)),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = F,
                                                                   selected = c("2015")))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseBenthicGroup_spatial","Benthic Categories",
                                                                   choices = unique(benthic_cover_group$group),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = F,
                                                                   selected = c("Hard coral")))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseDepthStrataBC_spatial","Depth Strata",
                                                                   choices = unique(benthic_cover_group$depth_strata_long),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = F,
                                                                   selected = c("middle (10-20 m)"))))),
                                     fluidRow(column(width = 12,
                                                     wellPanel(
                                                       leaflet::leafletOutput("benthic_villages_map"))))),
                            
                            
                      
                            
                            )),

######### ----------------------------------------------------------
######### Fish Exploration Tab
######### ----------------------------------------------------------

                  tabItem(tabName = "fish",
                          fluidRow(column(12, h2("Fish Data"))),
                          tabsetPanel(
                            tabPanel(title = "About Fish Counts",
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render("text/fish_about_text.Rmd"))))
                                     
                            ),
                            tabPanel(title = "Explore Site Level",
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render("text/fish_site_text.Rmd")))),
                                     fluidRow(column(12,
                                                     column(6,
                                                            fluidRow(column(width = 6,
                                                                            wellPanel(
                                                                              pickerInput("selectFishSource","Data source",
                                                                                          choices = unique(fish_count$source),
                                                                                          options = list(`actions-box` = TRUE),
                                                                                          multiple = F,
                                                                                          selected = "NOAA"))),
                                                                     column(width = 6,
                                                                            wellPanel(
                                                                              pickerInput("chooseYearFish","Year(s)",
                                                                                          choices = sort(unique(fish_count$year)),
                                                                                          options = list(`actions-box` = TRUE),
                                                                                          multiple = T,
                                                                                          selected = c("2015", "2016","2017", "2018"))))),
                                                            fluidRow(column(width = 6,
                                                                            wellPanel(
                                                                              pickerInput("inputVillageFish", "Village",
                                                                                          choices = unique(fish_count$nearest_village),
                                                                                          multiple = T,
                                                                                          selected = tutuila_villages))),
                                                                     column(width = 6,
                                                                            wellPanel(
                                                                              pickerInput("chooseDepthStrataFish","Depth Strata",
                                                                                          choices = unique(fish_count$depth_strata_long),
                                                                                          options = list(`actions-box` = TRUE),
                                                                                          multiple = T,
                                                                                          selected = c("middle (10-20 m)"))))),
                                                            fluidRow(column(width = 6,
                                                                            wellPanel(
                                                                              pickerInput("chooseFishTrophic","Functional Group",
                                                                                          choices = unique(fish_count$trophic),
                                                                                          options = list(`actions-box` = TRUE),
                                                                                          multiple = T,
                                                                                          selected = unique(fish_count$trophic)))),
                                                                     column(width = 6,
                                                                            wellPanel(
                                                                              pickerInput("chooseFishOI","Fish of Interest",
                                                                                          choices = unique(fish_count$consistent_taxon),
                                                                                          multiple = T,
                                                                                          selected = species_oi))))),
                                                     column(width = 6,
                                                            wellPanel(
                                                              leaflet::leafletOutput("fish_villages_map"))))),
                                     fluidRow(box(width = 12,
                                                  collapsible = TRUE,
                                                  plotlyOutput("fish_count_scatter_plot")))),
                            tabPanel(title = "Explore Island Level",
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render("text/fish_summarized_text.Rmd")))),
                                     fluidRow(column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseIslandFishSum","Island",
                                                                   choices = unique(fish_island_summarized$island),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = unique(fish_island_summarized$island)))),
                                              column(width = 3,
                                                     wellPanel(
                                                       pickerInput("chooseDepthStrataFishSum","Depth Strata",
                                                                   choices = unique(fish_island_summarized$depth_strata_long),
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = T,
                                                                   selected = "middle (10-20 m)")))),
                                     fluidRow(column(width = 12,
                                                     wellPanel(
                                                       plotlyOutput("fish_bar_plot"))))))),

######### ----------------------------------------------------------
#########  Environmental Exploration Tab
######### ----------------------------------------------------------

                  tabItem(tabName = "environmental",
                         # fluidRow(column(12, h2("In Situ Environmental Data"))),
                          tabsetPanel(
                            tabPanel(title = "In Situ Temperature",
                                     fluidRow(
                                       column(width = 12, p(" "))),
                                     fluidRow(
                                       column(width = 12, h3("Summary of Datasets"))),
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render("text/environmental_temp_summary_text.Rmd")))),
                                     fluidRow(
                                       column(width = 12, h3("Explore Temperature"))),
                                
                                     
                                     fluidRow(box(width = 12, 
                                                  column(12,
                                                     column(6,
                                                            
                                                            column(12,
                                                                     pickerInput("chooseIslandTemp","Island",
                                                                                 choices = sort(unique(insitu_temperature$island)),
                                                                                 options = list(`actions-box` = TRUE),
                                                                                 multiple = T,
                                                                                 selected = c(unique(insitu_temperature$island)))),
                                                            column(12,
                                                                     pickerInput("chooseDepthStrataTemp","Depth Strata",
                                                                                 choices = unique(insitu_temperature$depth_strata_long),
                                                                                 options = list(`actions-box` = TRUE),
                                                                                 multiple = T,
                                                                                 selected = c("shallow (0-10 m)"))),
                                                            column(12,
                                                                     sliderInput("chooseYearTemp","Year",
                                                                                 min = min(unique(insitu_temperature$year)),
                                                                                 max = max(unique(insitu_temperature$year)),
                                                                                 step = 1,
                                                                                 value = c("2015", "2022"),
                                                                                 sep = "")),
                                                            column(12,
                                                                     pickerInput("chooseVillageTemp","Village",
                                                                                 choices = sort(unique(insitu_temperature$nearest_village)),
                                                                                 options = list(`actions-box` = TRUE),
                                                                                 multiple = T,
                                                                                 selected = c(unique(insitu_temperature$nearest_village))))
                                                            ),
                                                     column(6,
                                                            leaflet::leafletOutput("insitu_temperature_map", height = 313))))),
      
                                     fluidRow(box(width = 12,
                                                  column(width = 12,
                                                         (plotlyOutput("insitu_temperature_plot", height = 500) ))))),
                            

                            
                            tabPanel(title = "In Situ Water Quality",
                                     fluidRow(column(width = 12, p(" "))),
                                     fluidRow(column(width = 12, h3("Summary of Datasets"))),
                                     fluidRow(
                                       box(width = 12,
                                           includeHTML(rmarkdown::render("text/environmental_wq_summary_text.Rmd")))),
                                    
                                     fluidRow(column(width = 12, h3("Explore Water Quality"))),
                                     fluidRow(box(width = 12,
                                                  column(12,
                                                         column(6,
                                                                column(12,
                                                                       radioButtons("pickParameterCommonWQ",
                                                                                          "Common Parameters",
                                                                                          choices = wq_common_parameters,
                                                                                          inline = "true",
                                                                                          selected = "TP")),
                                                                column(12,
                                                                       radioButtons("pickParameterNCommonWQ",
                                                                                          "Less Common Parameters",
                                                                                          choices = wq_less_common_parameters,
                                                                                          inline = "true",
                                                                                    selected = character(0)))),
                                                         
                                              column(width = 6,
                                                     leaflet::leafletOutput("water_quality_map_projects", height = 450))),
                                     fluidRow(
                                       column(12,
                                           plotlyOutput("water_quality_plot", height = 1000)))))),
                            
                     tabPanel(title = "Satellite Spatial",
                              fluidRow(column(width = 12, p(" "))),
                              fluidRow(column(width = 12, h3("Summary of Datasets"))),
                              fluidRow(
                                box(width = 12,
                                    includeHTML(rmarkdown::render("text/environmental_satellite_spatial_summary_text.Rmd")))),
                              
                              fluidRow(column(width = 3,
                                              wellPanel(
                                                radioButtons("env_PickData",
                                                             "Parameter",
                                                             choices = unique(satellite_environmental$parameter),
                                                             inline = "true",
                                                             selected = c("Degree Heating Weeks (CRW)")))),
                                       column(width = 3,
                                              wellPanel(
                                                radioButtons("env_PickIsland",
                                                             "Island",
                                                             choices = unique(satellite_environmental$island),
                                                             selected = c("Tutuila")))),
                                       column(width = 3,
                                              wellPanel(
                                                ## create date range choices
                                                radioButtons("env_PickYearSat",
                                                            "Year",
                                                            inline = TRUE,
                                                            choices = sort(unique(satellite_environmental$year)),
                                                            selected = c("2015")))),
                                      column(width =3, 
                                             wellPanel(pickerInput("chooseSeasonSat","Season",
                                                   choices = sort(unique(satellite_environmental$season)),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = T,
                                                   selected = c(unique(satellite_environmental$season)))))),
                              fluidRow(column(12,
                                              leaflet::leafletOutput("satellite_environmental_map")))),
                     
                     tabPanel(title = "Satellite Temporal",
                              fluidRow(column(width = 12, p(" "))),
                              fluidRow(column(width = 12, h3("Summary of Datasets"))),
                              fluidRow(
                                box(width = 12,
                                    includeHTML(rmarkdown::render("text/environmental_satellite_temporal_summary_text.Rmd")))),
                              fluidRow(column(width = 3,
                                              wellPanel(
                                                radioButtons("env_PickDataTemporal",
                                                             "Environmental Satellite data",
                                                             choices = unique(satellite_environmental$parameter)))),
                                       column(width = 3,
                                              wellPanel(
                                                pickerInput("env_PickIslandTemporal",
                                                            "Island",
                                                            choices = sort(unique(satellite_environmental$island)),
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = F,
                                                            selected = "Tutuila")))),
                              fluidRow(column(width=12,
                                              wellPanel(plotlyOutput("satellite_environmental_plot")))))
                     )),

######### ----------------------------------------------------------
######### Satellite Data Tab
######### ----------------------------------------------------------
# 
#                   tabItem(tabName = "satellite",
#                           fluidRow(column(12,
#                                           h2("Environmental Satellite Data"))),
#                           fluidRow(box(width = 12,
#                                        includeHTML(rmarkdown::render("text/satellite_explore_text.Rmd")))),
#                           tabsetPanel(
#                             tabPanel(title = "Explore Spatial",
#                                      fluidRow(column(width = 3,
#                                                      wellPanel(
#                                                        radioButtons("env_PickData",
#                                                                     "Parameter",
#                                                                     choices = c("SST", "Chl-a", "KdPar", "DHW"),
#                                                                     selected = c("SST")))),
#                                               column(width = 3,
#                                                      wellPanel(
#                                                        radioButtons("env_PickIsland",
#                                                                     "Island",
#                                                                     choices = unique(satellite_environmental_sst$island),
#                                                                     selected = c("Tutuila")))),
#                                               column(width = 3,
#                                                      wellPanel(
#                                                        ## create date range choices
#                                                        sliderInput("env_PickDateSat",
#                                                                    "Date",
#                                                                    min = min(satellite_environmental_sst$date),
#                                                                    max = max(satellite_environmental_sst$date),
#                                                                    value = c(as.Date("2015-01-01"), as.Date("2016-01-01")),
#                                                                    sep = "-")))),
#                                      fluidRow(column(12,
#                                                      wellPanel(leaflet::leafletOutput("satellite_environmental_map"))))),
#                             tabPanel(title = "Explore Temporal",
#                                      fluidRow(column(width = 3,
#                                                      wellPanel(
#                                                        radioButtons("env2_PickData",
#                                                                     "Environmental Satellite data",
#                                                                     choices = c("SST", "KdPar", "DHW"),
#                                                                     selected = c("SST")))),
#                                               column(width = 3,
#                                                      wellPanel(
#                                                        pickerInput("env_PickIslandVillage",
#                                                                    "Island",
#                                                                    choices = sort(unique(satellite_environmental_sst$island_village)),
#                                                                    options = list(`actions-box` = TRUE),
#                                                                    multiple = F,
#                                                                    selected = "Tutuila:Vatia")))),
#                                      fluidRow(column(width=12,
#                                                      wellPanel(plotlyOutput("satellite_environmental_plot"))))))),

######### ----------------------------------------------------------
######### Events Timeline Tab
######### -----------------------------------------------------------

                  tabItem(tabName = "eventstimeline",
                          fluidRow(column(12,
                                          h2("Events Timeline"))),
                          fluidRow(box(width = 12,
                                       includeHTML(rmarkdown::render("text/events_timeline_text.Rmd")))),
                          fluidRow(column(width = 3,
                                          wellPanel(
                                            pickerInput("chooseTimelineCategory",
                                                        "Category",
                                                        choices = sort(unique(events_timeline$category)),
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = T,
                                                        selected = "biophysical"))),
                                   column(width = 3,
                                          wellPanel(
                                            pickerInput("chooseTimePeriod",
                                                        "Time Period",
                                                        choices = sort(unique(events_timeline$time_period)),
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = T,
                                                        selected = "Post-2000")))),
                          fluidRow(column(width=12,
                                          plotlyOutput("events_timeline_plot")))),                                              

######### ----------------------------------------------------------
######### Resources Tab
######### -----------------------------------------------------------

                  tabItem(tabName = "resources",
                          h2("Resources"),
                          fluidRow(
                            box(width = 12,
                                includeHTML(rmarkdown::render("text/resources_text.Rmd"))))
                          # tags$img(class = "banner",
                          #          src = "media/methods_doc_screenshot.png",
                          #          # height = "10%",
                          #          # width = "100%",
                          #          alt = "A screen shot of American Samoa Methods Documnetation document.")
                          # 
                          )
           )))

