#R Study Group 
#September 24, 2019
#Author: William Ou

### Part 1 #### 
a <- 1 # 1 is assigned to the variable 'a'
class(a) # the value 1 is of the numeric class
#####

#Arithmetic operations
1+1
1+4/4

#logical operations: work for both numeric and charcaters
1==1
a>2
'cat'=='dog'

#TRUE = 1; FALSE = 0
(5>2)+(1==1)

#built-in dataset: iris, mtcars, cars, attitude, PlantGrowth,  etc...
iris$Sepal.Length 
View(iris)
sum(iris$Sepal.Length>5)


#vectors
vec <- c(1,2,4,2)  
vec  [3] #gives the 3rd element of the vector vec


#Matrix (a multidimensional array)
c(1,'2',4,1)
matrix(1:10,nrow=5,byrow=T)




#data frames
df <- data.frame(x=c(1,2,4,3),y=c('a','f','d','e'))
#df [r,c]
df[1,1]
df$x #by variable/column name
df[,1] #by index

#Excerice: Shifting the elements to the right
v <- round(runif(5),2)

length(v) #tells you how many elements are in v
seq(v) #seq(v) creates a sequence from 1 to length of v; this sequence is essentially another vector that can be indexed

#SOLUTION

#create a vector of indices and use that to index the vector v
#the first one should the last i which is equal to the length of v
#the following sequence should 1 to the index just before length

#first i = 5
length(v)

#following that should be a sequence from 1 to 4 (ie. sequence to length(v)-1)
seq(length(v)-1)

#put them all together
v[c(length(v),seq(length(v)-1))]


#Practice data set
#read from drobox
zooplankton <- read.csv("https://www.dropbox.com/s/k3xvoi2vxg9p64k/zooplankton_clean.csv?dl=1")

#useful functions that help you inspect the data set
str(zooplankton)
head(zooplankton)
View(zooplankton)
unique(zooplankton$genus)
#visualize data 
hist(zooplankton$size,breaks=30) #creates a histogram of the size values; Just using different values for the breaks argument to see what it does
boxplot(zooplankton$size~zooplankton$genus) #creates box plot where y is the size ~ x is categorical genus variable






