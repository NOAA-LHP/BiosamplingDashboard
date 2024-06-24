# Data Inventory Plot -----------------------------------------------------------

data_inventory_plot <- function(input){
  guam <-reactive(
    gdata %>%
      filter(ScientificName %in% c(input$Species),
             complete.cases(gdata$Length.cm.) ))
  tempm<-guam 

    Months<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    Mn=c(1:10, 1)
    Months<-as.data.frame(Months)
    Months<-as.factor(Months$Months)
    tempm<-tempm %>%
      dplyr::group_by(Month)%>%
      dplyr::summarize(N_month=length(Length.cm.))
    maxm<-max(tempm$N_month, na.rm=TRUE)
    maxm<-if (maxm >=20) maxm else 20
    tempm<-complete(tempm, Month, fill=list(N_Month=0))
    gsi<-guam %>% 
      dplyr::mutate(GSI=(GonWeight/Weight.g.)*100)
    #join into list
    build_list<-list(guam, tempm, gsi)
    names(build_list)<- c('temp','tempm','gsi')
    final_list<-build_list
    length_min<-min(final_list$guam$Length.cm., na.rm=TRUE)
    length_max<-max(final_list$guam$Length.cm., na.rm=TRUE)
    
    bin_width=2
    nbins <- seq(length_min - bin_width,
                 length_max + bin_width,
                 by = bin_width)
    
    p1 <- ggplot(data=guam, aes(x=Length.cm.))+geom_histogram(binwidth=2,breaks=nbins, color="black", aes(fill=Sex)) + 
      xlab("Fork Length (cm)") +  ylab("Frequency") + ggtitle("Size Distribution")+
      scale_fill_manual(values=c("red", "blue", "white", "black"))+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")+scale_x_continuous(limits = c(length_min-2, length_max+2))
    
    
    #monthly samples
    level_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    p2<-ggplot(tempm, aes(x=factor( Month, level=(level_order)), y=tempm$N_month)) + 
      geom_bar(stat = "identity")+xlab("") +  ylab("Frequency") + ggtitle("Monthly Sample Distribution")+
      geom_hline(yintercept = 20, colour="red", linetype = "dashed")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
    
    #GSI
    p3<-ggplot(gsi, aes(x=Length.cm., y=GSI, color=Sex)) +  xlab("Fork Length (cm)") +  ylab("GSI") + ggtitle("GSI & Fish Length")+
      geom_point(aes(colour=Sex), size=2) +
      geom_point(shape = 1,size=2, colour = "black") + scale_color_manual(values=c("red", "blue", "white", "black"))+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
    
    
    #Spawning Season
    gsi_f<-gsi %>% 
      subset(Sex=="F")
    #gsi_f$Month<-month(gsi_f$Date, label=TRUE)
    
    p4<-ggplot(gsi_f, aes(x=as.factor( Month), y=GSI)) + 
      geom_boxplot(fill="red") +
      xlab("") +  ylab("GSI") + ggtitle("Spawning Season")+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")+scale_x_discrete(limits = month.abb)
    
    grid.arrange(p1,p2,p3,p4 , nrow = 2)
  }
