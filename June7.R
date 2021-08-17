# June 7 R Srcipt

x <- c(TRUE,FALSE,TRUE)
y <- c(FALSE,FALSE,TRUE)
x


x&y

xor(x,y)

clean_data
abundance_data

species_matrix


#Delay flight
delay_flights <- flights%>%
  filter(arr_delay>0&dep_delay>0)

#mutate new column based on distance
distance_class <- delay_flights%>%
  mutate(dist_class = ifelse(distance>mean(distance),'long','short'))%>%
  group_by(dist_class)%>%
  summarise(arrival_delay = mean(arr_delay),departure_delay = mean(dep_delay),
            arrival_sd = sd(arr_delay), departure_sd = sd(dep_delay))


##Ggplot

iris
library(ggplot2)
ggplot(iris,aes(x=Species,y=Sepal.Length,
                col=Species,shape=Species))+
  geom_boxplot()+geom_point()+ylab('Sepal length')+
  theme_classic()


#generate fake data
fake_data <- data.frame(y=rep(runif(45)),group=rep(c('a','b','c'),each=15),time=rep(seq(15),3))
#visualize
fake_p <-ggplot(fake_data,aes(x=time,y=y,group=group,col=group))+geom_point()+geom_line()
#animate
library(gganimate)
fake_p + transition_reveal(time)


#NYC flights

#distance classes
library(tidyr)
dist_df <- distance_class%>%
pivot_longer(cols=c('arrival_delay','departure_delay'),
             values_to='delay',
             names_to='Delay_Type')%>%
pivot_longer(cols=c('arrival_sd','departure_sd'),
             values_to='sd')

ggplot(dist_df,aes(x=dist_class,y=delay,group=Delay_Type,
                   col=Delay_Type))+
  geom_point(position=position_dodge(width=0.1))+
  geom_errorbar(aes(ymin=delay-sd,ymax=delay+sd),
                width=0.1,position=position_dodge(width=0.1))+theme_classic()



