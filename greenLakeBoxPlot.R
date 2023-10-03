library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(purrr)
library(cowplot)

data<-read_excel('inputs/Green Lake water quality database_through202307.xlsx',skip=3,
                 col_names = c('Source','Period','Year','Month','Date','Station','Depth',
                               'TP','Chl-a','Secchi','Temp','TN','NO23','NH3','Inorg N','Org N',
                               'TKN','SRP','N to P Ratio','TP to Chl-a Ratio'),
                 col_types = c('text','text','numeric','numeric','numeric','text','text',
                               rep('numeric',13))) %>%
  filter(!is.na(Source)) %>%
  pivot_longer(cols=-(Source:Depth),names_to='Parameter',values_to='Result') %>%
  mutate(Period=ifelse(Year>=2017,'Posttreat3',Period)) %>%
  mutate(AlumPeriod=ifelse(grepl('1',Period)|Year==1995,'Alum 1',
                            ifelse(grepl('2',Period)|Period=='Pretreat3','Alum 2',
                                   'Alum 3')))
  



plots_out<-data %>%
  filter(!is.na(Period)&Period!='NA') %>%
  mutate(xGroup=ifelse(Year==1959,'1959',ifelse(Year==1981,'1981','Other'))) %>%
  group_by(Parameter) %>%
  nest() %>%
  mutate(plot=map2(.x=data,.y=Parameter,.f=~{
    
    ymax=max(.x$Result,na.rm=T)
    
    pre1991<-.x %>%
      filter(Year<1989) %>%
      ggplot(aes(x=factor(Year),y=Result,group=Year))+
      geom_boxplot()+
      geom_point(data=~.x %>% group_by(Year) %>% summarise(Mean=mean(Result,na.rm=T)),
                 aes(x=factor(Year),y=Mean),shape=8)+
      xlab('')+
#      scale_y_continuous(paste(.y,ifelse(.y=='Temp','(deg C)',ifelse(grepl('Ratio',.y),'','(ug/L)'))),
 #                        limits = c(0,ymax))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5,size=10,face='bold'),
            axis.title.y=element_text(size=14,face='bold'),
            axis.text.y=element_text(size=10,face='bold'),
            legend.position='none')
    
    post1991<-.x %>%
      filter(Year>=1989) %>%
      ggplot(aes(x=Year,y=Result,group=Year))+
      geom_boxplot(aes(fill=AlumPeriod))+
      geom_point(data=~.x %>% group_by(Year) %>% summarise(Mean=mean(Result,na.rm=T)),
                 aes(x=Year,y=Mean),shape=8)+
      geom_vline(xintercept = c(1991,2003.5,2015.5),linewidth=2)+
      scale_x_continuous('',breaks=1989:2023,minor_breaks=NULL)+
    #  scale_y_continuous(limits = c(0,ymax))+
      ylab('')+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5,size=10,face='bold'),
            axis.text.y=element_blank(),
            legend.position = 'none')
    
    # post1991_95<-.x %>%
    #   filter(Year>=1989&Year<=1995) %>%
    #   ggplot(aes(x=Year,y=Result,group=Year))+
    #   geom_boxplot()+
    #   theme_bw()+
    #   #ggtitle('Pre-1991')+
    #   theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5),
    #         axis.text.y=element_blank())+
    #   geom_vline(xintercept = c(1991,2003.5,2015.5),linewidth=2)+
    #   xlab('')+
    #   scale_y_continuous(limits = c(0,ymax))+
    #   ylab('')
    
    # post2004<-.x %>%
    #   filter(Year>=2004) %>%
    #   ggplot(aes(x=Year,y=Result,group=Year))+
    #   geom_boxplot()+
    #   theme_bw()+
    #   #ggtitle('Pre-1991')+
    #   theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5),
    #         axis.text.y=element_blank())+
    #   geom_vline(xintercept = c(1991,2003.5,2015.5),linewidth=2)+
    #   xlab('')+
    #   scale_y_continuous(limits = c(0,ymax))+
    #   ylab('')
    
    
    if(.y=='Secchi'){
      pre1991<-pre1991+
        geom_hline(yintercept = 2.5,col='red')+
        scale_y_reverse('Secchi Depth (m)',limits=c(7,0),breaks=0:7)
      post1991<-post1991+
        geom_hline(yintercept = 2.5,col='red')+
        scale_y_reverse('',limits=c(7,0),breaks=0:7)+
        annotate('text',x=2000,y=2.5,label='Goal \n>2.5 m',vjust=1)+
        annotate('text',x=1992,y=0,label='Alum 1',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2005,y=0,label='Alum 2',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2016,y=0,label='Alum 3',fontface='bold',hjust=0,vjust=1,size=8)
    }
    if(.y=='Chl-a'){
      pre1991<-pre1991+
        geom_hline(yintercept = 7.3,col='blue')+
        scale_y_continuous('Chlorophyll-a (µg/L)',limits = c(0,100),breaks=seq(0,100,10))
      post1991<-post1991+
        geom_hline(yintercept = 7.3,col='blue')+
        scale_y_continuous('',limits = c(0,100),breaks=seq(0,100,10))+
        annotate('text',x=2000,y=7.3,label='Eutrophic \n>7.3 µg/L',vjust=-0.5)+
        annotate('text',x=1992,y=95,label='Alum 1',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2005,y=95,label='Alum 2',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2016,y=95,label='Alum 3',fontface='bold',hjust=0,vjust=1,size=8)
    }
    if(.y=='TP'){
      pre1991<-pre1991+
        geom_hline(yintercept = 20,col='blue')+
        scale_y_continuous('Total Phosphorus (µg/L)',limits = c(0,100),breaks=seq(0,100,10))
      post1991<-post1991+
        geom_hline(yintercept = 20,col='blue')+
        scale_y_continuous('',limits = c(0,100),breaks=seq(0,100,10))+
        annotate('text',x=2000,y=20,label='Goal \n <20 µg/L',vjust=0.5)+
        annotate('text',x=1992,y=95,label='Alum 1',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2005,y=95,label='Alum 2',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2016,y=95,label='Alum 3',fontface='bold',hjust=0,vjust=1,size=8)
    }
    if(.y=='TN'){
      pre1991<-pre1991+
        geom_hline(yintercept = 650,col='blue')+
        scale_y_continuous('Total Nitrogen (µg/L)',limits = c(0,1200),breaks=seq(0,1200,100))
      post1991<-post1991+
        geom_hline(yintercept = 650,col='blue')+
        scale_y_continuous('',limits = c(0,1200),breaks=seq(0,2100,100))+
        annotate('text',x=2000,y=650,label='Eutrophic \n>650 µg/L',vjust=0.5)+
        annotate('text',x=1992,y=1200,label='Alum 1',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2005,y=1200,label='Alum 2',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2016,y=1200,label='Alum 3',fontface='bold',hjust=0,vjust=1,size=8)
    }
    if(.y=='N to P Ratio'){
      pre1991<-pre1991+
        geom_hline(yintercept = c(9,22),col='blue')+
        scale_y_continuous('N to P Ratio',limits = c(0,50),breaks=seq(0,50,5))
      post1991<-post1991+
        geom_hline(yintercept = c(9,22),col='blue')+
        scale_y_continuous('',limits = c(0,50),breaks=seq(0,50,5))+
        annotate('text',x=2000,y=22,label='P Limited >22',vjust=-0.5)+
        annotate('text',x=2000,y=9,label='N Limited <9',vjust=1)+
        annotate('text',x=1992,y=50,label='Alum 1',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2005,y=50,label='Alum 2',fontface='bold',hjust=0,vjust=1,size=8)+
        annotate('text',x=2016,y=50,label='Alum 3',fontface='bold',hjust=0,vjust=1,size=8)
    }
    plot<-plot_grid(pre1991,post1991,rel_widths = c(0.15,.85))
    plot
  }))
  
plots_out$plot[1]

map2(plots_out$plot,plots_out$Parameter,.f=~ggsave(paste0('outputs/',.y,'.png'),.x,scale=1.2))


data %>%
  filter(!is.na(Period)&Period!='NA') %>%
  mutate(xGroup=ifelse(Year==1959,'1959',ifelse(Year==1981,'1981','Other'))) %>%
  filter(Month>=5&Month<=10) %>%
  group_by(Parameter,Year) %>%
  summarise(SummerMean=round(mean(Result,na.rm=T),1)) %>%
  pivot_wider(names_from = Parameter,values_from = SummerMean) %>%
  left_join(MC_GeoMean_All) %>%
  writexl::write_xlsx('outputs/GreenLake_summerMeans_2023.xlsx')
  