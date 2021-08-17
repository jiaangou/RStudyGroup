ssd <- read.csv('ssd_across_year.csv')

#libraries
library(dplyr)

#Data summaries
length(unique(ssd$Taxonomy)) #95 species
table(ssd$Taxonomy) #number of populations per species

table(ssd$SSD_1965) #FBSSD = 87; MBSSD = 23; NSSSD = 26
table(ssd$SSD_1965,ssd$Altitude) #by altitude by bias type


#dplyr alternative to counts
SSD_count <- ssd%>%
  group_by(Altitude,SSD_1965)%>%
  summarise(n())


#Transformation
library(tidyr)

SSD_long <- gather(ssd,key='Year',value='Gender_bias',SSD_1965,SSD_2007)%>%
  mutate(Year=substr(Year,5,8))

#Get counts by altitude by year by gender bias
#.drop=FALSE argument ensures all unique combination is present, which adds 0s to groups when they are not present
SSD_summary <- SSD_long%>%
  group_by(factor(Altitude),factor(Year),factor(`Gender_bias`),.drop=FALSE)%>%
  summarise(Count=n())%>%
  rename(Altitude=`factor(Altitude)`)%>%
  rename(Year=`factor(Year)`)%>%
  rename(`Gender_bias`=`factor(Gender_bias)`)

View(SSD_summary)

## Grouped barplot with ggplot
library(ggplot2)
ggplot(SSD_summary,aes(x=factor(Altitude),y=Count))+
  geom_bar(aes(fill=Gender_bias),position = 'dodge',stat='identity')+
  facet_wrap(~Year)+theme_classic()

ggsave('SSD_count.png')






