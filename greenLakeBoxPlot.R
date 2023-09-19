library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(purrr)

data<-read_excel('inputs/Green Lake water quality database_through202307.xlsx',skip=3,
                 col_names = c('Source','Period','Year','Month','Date','Station','Depth',
                               'TP','Chl-a','Secchi','Temp','TN','NO23','NH3','Inorg N','Org N',
                               'TKN','SRP','N to P Ratio','TP to Chl-a Ratio'),
                 col_types = c('text','text','numeric','numeric','numeric','text','text',
                               rep('numeric',13))) %>%
  filter(!is.na(Source)) %>%
  pivot_longer(cols=-(Source:Depth),names_to='Parameter',values_to='Result') %>%
  mutate(Period=ifelse(Year>=2017,'Posttreat3',Period)) 
  

library(cowplot)

plots_out<-data %>%
  filter(!is.na(Period)&Period!='NA') %>%
  mutate(xGroup=ifelse(Year==1959,'1959',ifelse(Year==1981,'1981','Other'))) %>%
  group_by(Parameter) %>%
  nest() %>%
  mutate(plot=map2(.x=data,.y=Parameter,.f=~{
    
    ymax=max(.x$Result,na.rm=T)
    
    pre1981<-.x %>%
      filter(Year<1989) %>%
      ggplot(aes(x=factor(Year),y=Result,group=Year))+
        geom_boxplot()+
        theme_bw()+
      #  ggtitle('Pre-1991')+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5))+
      xlab('')+
      scale_y_continuous(paste(.y,ifelse(.y=='Temp','(deg C)',ifelse(grepl('Ratio',.y),'','(ug/L)'))),
                         limits = c(0,ymax))
    
    post1991_95<-.x %>%
      filter(Year>=1989&Year<=1995) %>%
      ggplot(aes(x=Year,y=Result,group=Year,fill=Period))+
      geom_boxplot()+
      theme_bw()+
      #ggtitle('Pre-1991')+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5),
            axis.text.y=element_blank())+
      geom_vline(xintercept = c(1991,2003.5,2015.5),linewidth=2)+
      xlab('')+
      scale_y_continuous(limits = c(0,ymax))+
      ylab('')
    
    post2004<-.x %>%
      filter(Year>=2004) %>%
      ggplot(aes(x=Year,y=Result,group=Year,fill=Period))+
      geom_boxplot()+
      theme_bw()+
      #ggtitle('Pre-1991')+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust = 0.5),
            axis.text.y=element_blank())+
      geom_vline(xintercept = c(1991,2003.5,2015.5),linewidth=2)+
      xlab('')+
      scale_y_continuous(limits = c(0,ymax))+
      ylab('')
    
    
    if(.y=='Secchi'){
      pre1991<-pre1991+
        geom_hline(yintercept = 2.5,col='red')+
        scale_y_reverse('Secchi Depth (m)',limits=c(ymax,0))
      post1991<-post1991+
        geom_hline(yintercept = 2.5,col='red')+
        scale_y_reverse('',limits=c(ymax,0))
    }
    if(.y=='Chl-a'){
      pre1991<-pre1991+
        geom_hline(yintercept = 7,col='red')
      post1991<-post1991+
        geom_hline(yintercept = 7,col='red')
    }
    if(.y=='TP'){
      pre1991<-pre1991+
        geom_hline(yintercept = 25,col='red')
      post1991<-post1991+
        geom_hline(yintercept = 25,col='red')
    }
    if(.y=='N:P Ratio'){
      pre1991<-pre1991+
        geom_hline(yintercept = c(9,22),col='red')
      post1991<-post1991+
        geom_hline(yintercept = c(9,22),col='red')
    }
    plot<-plot_grid(pre1991,post1991,rel_widths = c(0.15,.85))
    plot
  }))
  
plots_out$plot[1]

map2(plots_out$plot,plots_out$Parameter,.f=~ggsave(paste0('outputs/',.y,'.png'),.x,scale=1.2))
