# ~~~~~~~~~ global ~~~~~~~~~ #

# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(htmltools)
library(rmarkdown)
library(markdown)
library(tidyverse)
library(here)
library(sf)
library(leaflet)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(viridis)
library(rcartocolor)
library(png)
library(scales)
library(leaflegend)
library(ggrepel)
library(foreach)
library(doParallel)

#library(tibbletime)
#library(fresh) 
#library(dashboardthemes)
#library(mapview)
#remotes::install_github("dreamRs/shinyWidgets")

# IMPORT DATA ----






data <- read.csv("~/Documents/github/BiosamplingDashboard/dashboard/data/all_samples_2_2024_commonname_noSI.csv") 
data<-data[complete.cases(data$ScientificName),] 
unique(data$Month)
data$Month<-as.character(data$Month)
data$Month<-factor(data$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
data <- data[-which(data$ScientificName=="Calotomus carolinus" & data$Length.cm.>80),]

data <- data[-which(data$ScientificName=="Acanthurus lineatus" & data$Length.cm.>28),]

names(data)[names(data) == 'CommonName'] <- 'CN'
str(data)
unique(sort(data$ScientificName))

summ<-data %>% 
  dplyr::group_by(ScientificName, Region, Month) %>%
  summarise(n=length(Length.cm.))
#pull BMUS and priority non-BMUS species
#test first with BMUS only
BMUS<-c("Aphareus rutilans","Aprion virescens", 
        "Caranx ignobilis",  
        "Caranx lugubris",  
        "Etelis carbunculus",  
        "Etelis coruscans",  
        "Lethrinus rubrioperculatus" , 
        "Lutjanus kasmira"  ,
        "Pristipomoides auricilla"  ,
        "Pristipomoides filamentosus"  ,
        "Pristipomoides flavipinnis"  ,
        "Pristipomoides sieboldii"  ,
        "Pristipomoides zonatus"  ,
        "Variola louti")

#Guam ecosystem species >=50n
Gcoral<-c("Caranx melampygus", 
          "Cheilinus undulatus",
          "Epinephelus fasciatus",
          "Lethrinus obsoletus",
          "Siganus punctatus")

#CNMI ecosystem species >=50n
Ccoral<-c("Acanthurus lineatus",
          "Acanthurus nigricauda",
          "Acanthurus triostegus",
          "Calotomus carolinus",
          "Caranx melampygus",
          "Cephalopholis argus",
          "Cheilinus trilobatus",
          "Cheilinus undulatus",
          "Chlorurus sordidus",
          "Kyphosus cinerascens",
          "Lethrinus obsoletus",
          "Monotaxis grandoculis",
          "Mulloidichthus vanicolensis",
          "Naso lituratus",
          "Naso unicornis",
          "Sargocentron spiniferum",
          "Sargocentron tiere",
          "Scarus ruboviolaceus", 
          "Siganus punctatus",
          "Siganus spinus",
          "Zanclus cornutus")

all<-c("Aphareus rutilans","Aprion virescens", 
       "Caranx ignobilis",  
       "Caranx lugubris",  
       "Etelis carbunculus",  
       "Etelis coruscans",  
       "Lethrinus rubrioperculatus" , 
       "Lutjanus kasmira",
       "Pristipomoides auricilla" ,
       "Pristipomoides filamentosus" ,
       "Pristipomoides flavipinnis",
       "Pristipomoides sieboldii" ,
       "Pristipomoides zonatus" ,
       "Variola louti",
       "Caranx melampygus",
       "Cheilinus undulatus",
       "Epinephelus fasciatus",
       "Lethrinus obsoletus",
       "Siganus punctatus",
       "Acanthurus lineatus",
       "Acanthurus nigricauda",
       "Acanthurus triostegus",
       "Calotomus carolinus",
       "Caranx melampygus",
       "Cephalopholis argus",
       "Cheilinus trilobatus",
       "Cheilinus undulatus",
       "Chlorurus sordidus",
       "Kyphosus cinerascens",
       "Monotaxis grandoculis",
       "Mulloidichthys vanicolensis",
       "Naso lituratus",
       "Naso unicornis",
       "Sargocentron spiniferum",
       "Sargocentron tiere",
       "Scarus rubroviolaceus",
       "Siganus spinus",
       "Zanclus cornutus")


#subset data 
data<-data |> subset(ScientificName %in% all)
sort(unique(data$ScientificName))
str(data)
#fix common names to only one per scientific name
#unique(data$CommonName)

cname<-c("SILVERMOUTH", "UKU","GIANT TREVALLY", "BLACK JACK", "EHU SNAPPER", "ONAGA SNAPPER", "REDGILL EMPEROR", "BLUELINED SNAPPER", "YELLOWTAIL KALIKALI", "PINK OPAKAPAKA", "YELLOWEYE OPAKAPAKA","KALIKALI","GINDAI", "LYRETAIL GROUPER")

Gcoral_cn<-c("BLUEFIN TREVALLY","NAPOLEON WRASSE", "BLACKTIP GROUPER", "ORANGE-STRIPED EMPEROR","GOLD-SPOTTED RABBITFISH")

Ccoral_cn<-c("BLUEBANDED SURGEONFISH", "EPAULETTE SURGEONFISH", "CONVICT TANG", "BUCKTOOTH PARROTFISH", "BLUEFIN TREVALLY", "PEACOCK GROUPER", "TRIPLETAIL WRASSE", "NAPOLEON WRASSE","BULLETHEAD PARROTFISH", "HIGHFIN RUDDERFISH", "BIGEYE EMPEROR","YELLOWFIN GOATFISH", "ORANGESPINE UNICORNFISH", "BLUESPINE UNICORNFISH", "SABER SQUIRRELFISH", "TAHITIAN SQUIRRELFISH", "REDLIP PARROTFISH", "SCRIBBLED RABBITFISH", "MOORISH IDOL")

all_cn<-c("SILVERMOUTH",
          "UKU",
          "GIANT TREVALLY", 
          "BLACK JACK",
          "EHU SNAPPER",
          "ONAGA SNAPPER",
          "REDGILL EMPEROR", 
          "BLUELINED SNAPPER", 
          "YELLOWTAIL KALIKALI",
          "PINK OPAKAPAKA", 
          "YELLOWEYE OPAKAPAKA",
          "KALIKALI",
          "GINDAI", 
          "LYRETAIL GROUPER",
          "BLUEFIN TREVALLY",
          "NAPOLEON WRASSE",
          "BLACKTIP GROUPER", 
          "ORANGE-STRIPED EMPEROR",
          "GOLD-SPOTTED RABBITFISH",
          "BLUEBANDED SURGEONFISH",
          "EPAULETTE SURGEONFISH", 
          "CONVICT TANG", 
          "BUCKTOOTH PARROTFISH", 
          "BLUEFIN TREVALLY", 
          "PEACOCK GROUPER", 
          "TRIPLETAIL WRASSE", 
          "NAPOLEON WRASSE",
          "BULLETHEAD PARROTFISH", 
          "HIGHFIN RUDDERFISH", 
          "BIGEYE EMPEROR",
          "YELLOWFIN GOATFISH", 
          "ORANGESPINE UNICORNFISH", 
          "BLUESPINE UNICORNFISH", 
          "SABER SQUIRRELFISH",
          "TAHITIAN SQUIRRELFISH", 
          "REDLIP PARROTFISH", 
          "SCRIBBLED RABBITFISH", 
          "MOORISH IDOL")


temp<-data.frame(ScientificName=all, CommonName=all_cn)
str(temp)
sort(unique(temp$ScientificName))
str(data)
sort(unique(data$ScientificName))


data<-data%>% dplyr::select(ScientificName, Sex, Length.cm., Weight.g., GonWeight, Month, Region)

data<-merge(data,temp, by="ScientificName")

data<-data|> dplyr::arrange(ScientificName)


#set up species and region lists for dropdown menu
region_list <- c("A.Samoa", "A.Samoa_unfished", "Guam", "CNMI", "CNMI_unfished")
#species_list <- na.omit((unique(data$ScientificName)))
#common_list <-all_cn
#common_list <- na.omit((unique(data$CommonName)))

#dictionary of scientific name : common name
#scientific_dict <- species_list
#names(scientific_dict) <- common_list

#and common name : scientific name
#common_dict <- common_list
#names(common_dict) <- species_list



guamsn<-c("Aphareus rutilans","Aprion virescens", 
          "Caranx ignobilis",  
          "Caranx lugubris",  
          "Etelis carbunculus",  
          "Etelis coruscans",  
          "Lethrinus rubrioperculatus" , 
          "Lutjanus kasmira"  ,
          "Pristipomoides auricilla"  ,
          "Pristipomoides filamentosus"  ,
          "Pristipomoides flavipinnis"  ,
          "Pristipomoides sieboldii"  ,
          "Pristipomoides zonatus"  ,
          "Variola louti","Caranx melampygus", 
          "Cheilinus undulatus",
          "Epinephelus fasciatus",
          "Lethrinus obsoletus",
          "Siganus punctatus")

guamcn<-c("SILVERMOUTH", "UKU","GIANT TREVALLY", "BLACK JACK", "EHU SNAPPER", "ONAGA SNAPPER", "REDGILL EMPEROR", "BLUELINED SNAPPER", "YELLOWTAIL KALIKALI", "PINK OPAKAPAKA", "YELLOWEYE OPAKAPAKA","KALIKALI","GINDAI", "LYRETAIL GROUPER","BLUEFIN TREVALLY","NAPOLEON WRASSE", "BLACKTIP GROUPER", "ORANGE-STRIPED EMPEROR","GOLD-SPOTTED RABBITFISH")
temp<-data.frame(ScientificName=guamsn, CommonName=guamcn)
str(temp)
sort(unique(temp$ScientificName))
str(data)
sort(unique(data$ScientificName))

gdata<-data %>% dplyr::filter(Region =="Guam")

gdata<-gdata%>% dplyr::select(ScientificName, Sex, Length.cm., Weight.g., GonWeight, Month, Region)

gdata<-merge(gdata,temp, by="ScientificName")

gdata<-gdata|> dplyr::arrange(ScientificName)


#set up species and region lists for dropdown menu
#region_list <- c("A.Samoa", "A.Samoa_unfished", "Guam", "CNMI", "CNMI_unfished")
species_list <- na.omit((unique(gdata$ScientificName)))
common_list <-guamcn
common_list <- na.omit((unique(guamcn)))

#dictionary of scientific name : common name
scientific_dict <- species_list
names(scientific_dict) <- common_list

#and common name : scientific name
common_dict <- common_list
names(common_dict) <- species_list

#CNMI
cnmisn<-c("Aphareus rutilans",
          "Aprion virescens", 
          "Caranx ignobilis",  
          "Caranx lugubris",  
          "Etelis carbunculus",  
          "Etelis coruscans",  
          "Lethrinus rubrioperculatus" , 
          "Lutjanus kasmira"  ,
          "Pristipomoides auricilla"  ,
          "Pristipomoides filamentosus"  ,
          "Pristipomoides flavipinnis"  ,
          "Pristipomoides sieboldii"  ,
          "Pristipomoides zonatus"  ,
          "Variola louti", 
          "Caranx melampygus",
          "Acanthurus lineatus",
          "Acanthurus nigricauda",
          "Acanthurus triostegus",
          "Calotomus carolinus",
          "Cephalopholis argus",
          "Cheilinus trilobatus",
          "Cheilinus undulatus",
          "Chlorurus sordidus",
          "Kyphosus cinerascens",
          "Lethrinus obsoletus",
          "Monotaxis grandoculis",
          "Mulloidichthus vanicolensis",
          "Naso lituratus",
          "Naso unicornis",
          "Sargocentron spiniferum",
          "Sargocentron tiere",
          "Scarus ruboviolaceus", 
          "Siganus spinus",
          "Zanclus cornutus")


cnmicn<-c("SILVERMOUTH",
          "UKU",
          "GIANT TREVALLY", 
          "BLACK JACK",
          "EHU SNAPPER",
          "ONAGA SNAPPER",
          "REDGILL EMPEROR", 
          "BLUELINED SNAPPER", 
          "YELLOWTAIL KALIKALI",
          "PINK OPAKAPAKA", 
          "YELLOWEYE OPAKAPAKA",
          "KALIKALI",
          "GINDAI", 
          "LYRETAIL GROUPER",
          "BLUEFIN TREVALLY",
          "BLUEBANDED SURGEONFISH", 
          "EPAULETTE SURGEONFISH",
          "CONVICT TANG",
          "BUCKTOOTH PARROTFISH", 
          "PEACOCK GROUPER", 
          "TRIPLETAIL WRASSE", 
          "NAPOLEON WRASSE",
          "BULLETHEAD PARROTFISH",
          "HIGHFIN RUDDERFISH",
          "ORANGE-STRIPED EMPEROR",
          "BIGEYE EMPEROR",
          "YELLOWFIN GOATFISH", 
          "ORANGESPINE UNICORNFISH",
          "BLUESPINE UNICORNFISH", 
          "SABER SQUIRRELFISH",
          "TAHITIAN SQUIRRELFISH",
          "REDLIP PARROTFISH", 
          "SCRIBBLED RABBITFISH",
          "MOORISH IDOL")

temp<-data.frame(ScientificName=cnmisn, CommonName=cnmicn)
str(temp)
sort(unique(temp$ScientificName))
str(data)


cdata<-data%>% dplyr::filter(Region=="CNMI")

cdata<-cdata%>% dplyr::select(ScientificName, Sex, Length.cm., Weight.g., GonWeight, Month, Region)

cdata<-merge(cdata,temp, by="ScientificName")

cdata<-cdata|> dplyr::arrange(ScientificName)


#set up species and region lists for dropdown menu
#region_list <- c("A.Samoa", "A.Samoa_unfished", "Guam", "CNMI", "CNMI_unfished")
species_list <- na.omit((unique(cnmisn)))
common_list <-cnmicn
common_list <- na.omit((unique(cnmicn)))

#dictionary of scientific name : common name
scientific_dict <- species_list
names(scientific_dict) <- common_list

#and common name : scientific name
common_dict <- common_list
names(common_dict) <- species_list



