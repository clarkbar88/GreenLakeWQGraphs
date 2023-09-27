#algal txoin data
library(readxl)
library(dplyr)
library(ggplot2)

historic_excel_file<-'inputs/Green Lake Toxin Data 10-31-14.xlsx'

excel_sheets(historic_excel_file)

beach_microcystin<-read_excel(historic_excel_file,sheet='Beach Microcystin Database')

scum_microcystin<-read_excel(historic_excel_file,sheet='Scum Microcystin Database',skip=1) %>%
  mutate(CollectDate=as.Date(as.numeric(CollectDate),origin='1899-12-30'))


newer_excel_file<-'inputs/GreenLakeToxins-9-24-2023.xlsx'

excel_sheets(newer_excel_file)

all_mc_data<-bind_rows(
  read_excel(newer_excel_file,'EastBeach'),
  read_excel(newer_excel_file,'WestBeach'),
  read_excel(newer_excel_file,'DuckLaunch'),
  read_excel(newer_excel_file,'Lake')
) %>%
  filter(Parameter=='Microcystin') %>%
  mutate(`Microcystin Concentration (µg/L)`=as.numeric(`Toxin Concentration (µg/L)`)) %>%
  select(Site,LabSampleNum,CollectDate,`Microcystin Concentration (µg/L)`,`MDL (µg/L)`,ScumInd) %>%
  arrange(CollectDate,Site) %>%
  tidyr::separate(LabSampleNum,into=c('LabPrefix','LabSuffix'),remove=F) %>%
  mutate(Date=as.Date(CollectDate)) %>%
  group_by(Site,Date,LabPrefix) %>%
  mutate(SampleType=ifelse(n()==1&ScumInd=='Yes','Scum',
                         #  ifelse(ScumInd=='Yes',ifelse(LabSuffix==min(LabSuffix),'Routine','Scum'),
                           ifelse(`Microcystin Concentration (µg/L)`==min(`Microcystin Concentration (µg/L)`),'Routine','Scum')#,
                                #  'Routine')
                                  )) %>%
  ungroup() %>%
  mutate(SampleType=ifelse(is.na(SampleType),'Routine',SampleType),
         nonDetectFlag=is.na(`Microcystin Concentration (µg/L)`),
         `Microcystin Concentration (µg/L)`=ifelse(is.na(`Microcystin Concentration (µg/L)`),`MDL (µg/L)`,
                                                         `Microcystin Concentration (µg/L)`),
         Detection=ifelse(nonDetectFlag,'Not Detected','Detected'),
         AboveCriteria=ifelse(`Microcystin Concentration (µg/L)`>8,'Exceeds','Under'))

library(ggplot2)
for(i in c('all_sites',unique(all_mc_data$Site))){
  
  if(i=='all_sites') {data_for_plot<-all_mc_data }else data_for_plot<-all_mc_data %>% filter(Site==i)
  
mc_plot<-data_for_plot %>%
  ggplot(aes(x=Date,y=`Microcystin Concentration (µg/L)`))+
  geom_point(size=3,aes(fill=AboveCriteria,shape=Detection),col='black')+
  geom_hline(yintercept = 8)+
  theme_bw()+
  scale_y_log10(breaks=c(0.05,1,10,100,1000,10000),
                minor_breaks=c(1:9*.1,1:9,1:9*10,1:9*100,1:9*1000,1:9*10^4),
                labels=scales::comma)+
  facet_wrap(~SampleType,ncol=1)+
  scale_shape_manual('Detection',values=c('Detected'=21,'Not Detected'=25))+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle=45,hjust=1))+
  #scale_fill_manual('Monitoring',values=c('Routine'='lightblue','Scum'='red'))+
  scale_fill_manual('Criteria',values=c('Under'='lightblue','Exceeds'='red'))+
  guides(fill=guide_legend(override.aes = list(shape=21)))+
  scale_x_date('',date_breaks = 'year',date_labels = '%Y')

ggsave(paste0('outputs/mc_plot_',gsub('[()]','',i),'.png'),mc_plot,scale=1.2)
}


