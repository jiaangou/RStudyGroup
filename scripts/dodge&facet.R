#Dodging points of another layer-------


library(ggplot2)
library(tidyr)
library(dplyr)

#Prep data
#transform 
iris_long <- pivot_longer(iris,cols=1:4,names_to='Trait',values_to = 'Measurement (mm)')
iris_summ <- iris_long%>%
  group_by(Species,Trait)%>%
  summarise(`Measurement (mm)`=mean(`Measurement (mm)`),sd=sd(`Measurement (mm)`))


#position_dodge needs a discrete/categorical variable in order for it to dodge (ie. It needs to know how to dodge/or dodge by what variable)
#In the argument inside ggplot(), it takes the grouping/reference variable  as the variable 
#defined by color (in this case its Trait). Which is why the trait data points are dodged from one another within each level of x
#When we use color='black' in the 2nd geom_point() call, it's "tricked" to believe that the new defined color variable is the
# discrete reference variable. Since 'black' has only 1 level, it doesn't dodge from one another. 


#Original
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+
  geom_point(position=position_dodge(width=0.5))+
  geom_point(data=iris_summ,aes(x=Species,y=`Measurement (mm)`),
             position=position_dodge(width=0.5),col='black')

#This can be resovled by multiple ways:
#Debugged #1
# explicitly set a grouping variable within 2nd geom_point() call
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+
  geom_point(position=position_dodge(width=0.5))+
  geom_point(data=iris_summ,aes(x=Species,y=`Measurement (mm)`,group=Trait), # <-- HERE 
             position=position_dodge(width=0.5),col='black')

#Debugged #2
# Use a non-discrete method to differentiate you mean point from raw data points.
# In this case, point size is not a discrete grouping variable, so position_dodge will not be 
# "tricked" into thinking that it is
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+
  geom_point(position=position_dodge(width=0.5))+
  geom_point(data=iris_summ,aes(x=Species,y=`Measurement (mm)`), # <-- HERE 
             position=position_dodge(width=0.5),size=5)



#facet_wrap() VS facet_grid()----------
#Briefly, facet_wrap is actually just a quick easy way to split up your data into n number of panels. 
#It automatically chooses for you the dimension in which your panels are arragned. For example, when using
#the Trait as splitting variable in facet_wrap, it automatically creates a 2x2 panel plot. 
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+geom_point()+
  facet_wrap(~Trait)+theme_classic()

#If you add another variable it just creates a 1-dimensional vector out of those 2 variable (a vector of their unique combinations),
#and automatically creates 2D panel plot according to what it seems fitting
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+geom_point()+
  facet_wrap(~Trait+Species)+theme_classic()


#A feature that facet_wrap has, is you can constrain the number of rows (or column) so that it 
#just do it automatically
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+geom_point()+
  facet_wrap(~Trait,nrow=1)+theme_classic() #nrow =1 


#facet_grid() is more formal and "mathematical" if you will. This is originally what I explained facet_wrap() as but I was wrong.
#In facet_grid() each splitting variable represented 1 dimension. So in this case, setting Trait as the only splitting
#variable, it creates panels in a single dimension (identical to facet_wrap() with nrow=1)
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+geom_point()+
  facet_grid(~Trait)+theme_classic() #nrow =1 

#Now you can add another variable so that each represent a specific dimension.
#In this case, "Trait" is column and "Species" is row. Its a little bit similar to, facet_wrap() 
#with 2 variables, but you will notice that the labels do not get repeated and are placed at the edges
#of the plot
ggplot(iris_long,aes(x=Species,y=`Measurement (mm)`,col=Trait))+geom_point()+
  facet_grid(Species~Trait)+theme_classic() #nrow =1 


