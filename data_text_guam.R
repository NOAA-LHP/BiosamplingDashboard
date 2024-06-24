# Data Inventory Plot -----------------------------------------------------------

data_text_guam <- function(input){
  guam <-reactive(
      gdata %>%
        filter(ScientificName %in% c(input$Species),
               complete.cases(gdata$Length.cm.) ))
  #temp$Month<-month(temp$Date, label=TRUE) #keeps month data in date format for plotting in order
  tempm<-temp%>% 
    na.omit(guam)
  Months<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  Mn=c(1:10, 1)
  Months<-as.data.frame(Months)
  Months<-as.factor(Months$Months)
  #Months <- ordered(Months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))
  tempm<-guam %>%
    dplyr::group_by(Month)%>%
    dplyr::summarize(N_month=length(Length.cm.))
  maxm<-max(tempm$N_month, na.rm=TRUE)
  maxm<-if (maxm >=20) maxm else 20
  tempm<-tidyr::complete(tempm, Month, fill=list(N_Month=0))
  gsi<-guam %>% 
    dplyr::mutate(GSI=(GonWeight/Weight.g.)*100)
  
  #join into list
  build_list<-list(guam, tempm, gsi)
  names(build_list)<- c('guam','tempm','gsi')
  final_list<-build_list
  sample_size=length(final_list$guam$Length.cm.)
  length_min<-min(final_list$guam$Length.cm., na.rm=TRUE)
  length_max<-max(final_list$guam$Length.cm., na.rm=TRUE)
  length_median<-median(final_list$guam$Length.cm., na.rm=TRUE)
  gender<-final_list$guam %>%
    dplyr::group_by(Sex)%>%
    dplyr::summarize(n_females=length(Sex=="F"))
  female=gender[1,2]
  male=gender[2,2]
  unknown=gender[3,2]
  
  maxm<-max(final_list$tempm$N_month, na.rm=TRUE)
  maxm<-if (maxm >=20) maxm else 20
  
  #unpretty formatting but I could not get inline variables (line below) to work
  paste0("A total of ", sample_size, " samples (females=", female, ", males=", male,
         ", unknown/na=", unknown, ") have been collected to date. Median fork length is ",
         length_median, " cm (min=", length_min, "cm, max=", length_max, " cm).")
  
  #markdown("A total of `r sample_size` samples (females=`r female`, males=`r male`, unknown/na=`r unknown`) have been collected to date. Median fork length is `r median` cm (min=`r min` cm, max=`r max` cm).")
  
}

