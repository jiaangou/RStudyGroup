#R Study Group
#October 1, 2019
#Author: William Ou

#Libraries
library(tidyverse) #loads all libraries (but you can load individuals ones too)
#install.packages('tidyverse')


#Zooplankton data
raw_data <- read.csv("https://www.dropbox.com/s/k3xvoi2vxg9p64k/zooplankton_clean.csv?dl=1")
raw_chla <- read.csv("https://www.dropbox.com/s/7wxnvp67nawazor/chla.csv?dl=1")

View(raw_data)
View(raw_chla)
# Data manipulation w/ tidyverse-------------------------------------------------------
# 1. Renaming & correcting spelling variables

#data inspection
unique(raw_data$genus)
names(raw_data)

#Option 1 using mapvalues() from plyr
clean_data <- raw_data %>% 
  select(Lake,Sample,genus,size)%>% #select essential columns
  rename(lake = `Lake`) %>%   #rename variables names (if you want)
  rename(sample = `Sample`) %>% 
  mutate(genus=plyr::mapvalues(genus,from=c('Daphnia','daphniaa'),to=c('daphnia','daphnia')))%>% #plyr has conflict with dplyr when loaded together
  mutate(genus=plyr::mapvalues(genus,from=c('copepoda','cyclopoidae','Cyclopoida'),to=c('cyclopoida','cyclopoida','cyclopoida')))%>%
  mutate(genus=plyr::mapvalues(genus,from=c('callanoid','calanoidd'),to=c('calanoid','calanoid')))%>%
  mutate(genus=plyr::mapvalues(genus,from=c('Bosmina','bosminaa'),to=c('bosmina','bosmina')))

View(clean_data)
unique(clean_data$genus)
names(clean_data)

#Option 2 using str_replace() from stringr
clean_data <- raw_data%>%
  select(Lake,Sample,genus,size)%>% #select essential columns
  rename(lake = `Lake`) %>%   #rename variables names (if you want)
  rename(sample = `Sample`) %>% 
  mutate(genus = str_replace(genus, pattern = c('Daphnia|daphniaa'), replacement = c('daphnia')))%>% #replaces strings
  mutate(genus = str_replace(genus, pattern = c('copepoda|cyclopoidae|Cyclopoida'), replacement = c('cyclopoida')))%>%
  mutate(genus = str_replace(genus, pattern = c('callanoid|calanoidd'), replacement = c('calanoid')))%>%
  mutate(genus = str_replace(genus, pattern = c('Bosmina|bosminaa'), replacement = c('bosmina')))

#check again
names(clean_data)
unique(clean_data$genus)


#Chl-a 
unique(raw_chla$Lake)

clean_chla <- raw_chla%>%
  select(Lake,Depth,AvgChl)%>%
  rename(lake = `Lake`)%>%
  rename(depth = `Depth`)%>%
  mutate(lake = str_replace(lake,pattern=c('Lost Lake'),replacement=c('Lost')))%>%
  mutate(lake = str_replace(lake,pattern=c('Green Lake'),replacement=c('Green')))%>%
  mutate(lake = str_replace(lake,pattern=c('One Mile Lake'),replacement=c('One Mile')))%>%
  mutate(lake = str_replace(lake,pattern=c('Lillooet Lake'),replacement=c('Lillooet')))

#View(clean_chla)

# 2. Create new variables from variable values
hist(clean_data$size,breaks=50)
mean(clean_data$size) #mean is 0.65

clean_data <- clean_data%>%
  mutate(size_category=ifelse(size>0.65,'large','small'))

# 3. Summary statistics
chla_summ <- clean_chla%>%
  group_by(lake)%>%
  summarise(chla=mean(AvgChl),depth=mean(depth))

View(chla_summ)
chla_summ

# 4. Data joining
# general syntax: join_type(firstTable, secondTable, by=columnTojoinOn)

combined_data <- clean_data%>%
  left_join(chla_summ,by='lake')

str(combined_data)
#see cheatsheet for other join functions

# 5. Reshape: transposing wide-to-long & long-to-wide
# this is commonly used for community ecology data (ie. multispecies data)

# i. Use summary to get counts/abundance of each species for each lake
abundance_data <- clean_data%>%
  group_by(lake,sample,genus)%>%
  summarise(abundance=n())

View(abundance_data)

# ii. Long --> Wide: Species vs Lake and sample  (spread)
species_matrix <- abundance_data %>% 
  spread(key = genus, value = abundance)

View(species_matrix)

species_matrix[is.na(species_matrix)] <- 0  ## fills NA with 0

# iii. Wide --> Long (gather)
long_data <- species_matrix%>%
  gather(key=genus,value=abundance,-c(lake,sample)) #- sign specifies which to exclude

#check if abundances are the same
sum(as.numeric(long_data$abundance))
sum(abundance_data$abundance)



# Plotting w/ ggplot2 -----------------------------------------------------
library(ggplot2)
#1. Basic syntax: ggplot(data,aes(x=x,y=x))+geom_point()+geom_line etc...
ggplot(iris,aes(x=Species,y=Sepal.Length))+
  geom_point()+geom_boxplot()
  
ggplot(iris,aes(x=Species,y=Sepal.Length))+
  geom_boxplot()+geom_point(col='red')

ggplot(iris,aes(x=Species,y=Sepal.Length,col=Species))+geom_point()+geom_boxplot() #order matters

box_p <- ggplot(iris,aes(x=Species,y=Sepal.Length))+geom_point()+geom_boxplot() #can be assigned to a variable (unlike base R plots)
box_p

box_p+geom_jitter()


#2. Customizing
#Many built-in themes available
box_p + theme_bw()
box_p + theme_classic()
box_p + theme_gray()

#But is still customizable

box_p + theme(axis.text.x=element_text(size=13),
              axis.text.y=element_text(size=13),
              panel.background=element_blank())

#because ggplot is "function" based, you can assign your desired theme to a variable
my_theme <- theme(axis.title=element_text(size=15,face='bold'),
                  axis.text.x=element_text(size=13),
                  axis.text.y=element_text(size=13),
                  panel.background=element_blank())


ggplot(clean_data,aes(x=lake,y=size,fill=genus))+geom_boxplot()+my_theme

ggplot(clean_data,aes(x=size))+geom_histogram(binwidth = .1)+
  facet_wrap(~lake)+my_theme
?facet_grid
?facet_wrap

combined_data
ggplot(combined_data,aes(x=lake,y=size,fill=genus,col=genus))+geom_boxplot()+geom_jitter()

ggplot(combined_data,aes(x=depth,y=size,col=genus,shape=lake))+
  geom_jitter(size=scales::rescale(combined_data$chla,to=c(0,1)))+theme_classic()

ggplot(combined_data,aes(x=lake,y=size,fill=genus,col=genus))+geom_boxplot()+
  geom_jitter(size=scales::rescale(combined_data$chla,to=c(0,1)))



?scale
#a lot developments
devtools::install_github('erocoar/gghalves')
library(gghalves)

ggplot(clean_data,aes(x=lake,y=size))+geom_point()+geom_half_violin(side='r')+geom_half_boxplot(side='l')

#3. Animations
fake_data <- data.frame(y=rep(runif(45)),group=rep(c('a','b','c'),each=15),time=rep(seq(15),3))


#visualize
fake_p <-ggplot(fake_data,aes(x=time,y=y,group=group,col=group))+geom_point()+geom_line()
fake_p
#animate
library(gganimate)
fake_p + transition_reveal(time)


install.packages('devtools')





