#Writing functions in R; Author: William Ou; Date: June 28,2020 --------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(magick)
library(deSolve)

#Functions

#General structure

# function(p1, p2, p3){
#  step1  <-.....
#  step2 <- .....
#  step3 <- ....
#  out <- ....
#  return(out)
# }

#Example
custom_mean <- function(x){
  out <- sum(x)/length(x)
  return(out)
}
random <- rnorm(10)
mean(random)
custom_mean(random)


#Apply function
mat <- matrix(1:100,nrow=10)
apply(X = mat, MARGIN = 2,FUN = mean)


#For loop
for(i in 1:ncol(mat)) {
  #print(i)
  print(mean(mat[,i]))
}


#While loop
x <- 20
while(x>10){
  print(x-1)
  x <- x-1
}



x <- c(1,4,1)

y<- c(3,2,6)

x
y
x>y

#Sort algorithms
#for simplicity, sort from increasing order 
set.seed(1)
rand_i <- runif(10)

#Super slow sorting algorithm -------
#Put MAX at the end (or min at front) OR MAX at the front (or min at the back)
super_slow_sort <- function(random_sequence,track=TRUE){

  df <- data.frame('iteration1'=random_sequence)  
  random <- random_sequence #conserve original input
  sorted <- random_sequence #this will iteratively become sorted
  
  #initial conditions
  front <- sorted[-length(sorted)]
  back <- sorted[-1]
  first_val <- 1
  
  while(sum(front<=back)!=(length(sorted)-1)){
    
    min_i <- which(random==min(random)) #find the min value
    sorted[first_val] <- random[min_i] #replace first value with min value
    
    #Update conditions
    random <- random[-min_i] #remove minimum value
    first_val <- first_val +1
    front <- sorted[-length(sorted)]
    back <- sorted[-1]
    
    #update dataframe (optional)
    df[paste0('iteration',first_val)] <- sorted
  }

    #return(df)
  if(track==TRUE){
    return(df)
  }else{
    return(sorted)
  }
  
}

#Test and visualize
sorted_df <- super_slow_sort(rand_i,track=TRUE)
super_slow_sort(rand_i,track=FALSE)


sort_p  <- sorted_df %>%
  tibble::rownames_to_column(var='sequence')%>%
  pivot_longer(cols = -1,names_to='iteration',values_to ='Value')%>%
  mutate(sequence=as.numeric(sequence))%>%
  mutate(iteration_no = stringr::str_split(iteration,"iteration")%>%sapply(function(x)x[2]))%>%
  mutate(iteration_no = as.numeric(iteration_no))%>%
  ggplot(aes(x=factor(sequence),y=Value))+
  geom_bar(stat='identity')+labs(x='Sequence index')+theme_classic()
  
sort_anim <- sort_p + 
  transition_states(iteration_no,transition_length = 2,
                           state_length=0.5)+
  labs(title='Iteration: {closest_state}')



#Mathematical functions ---------

#Trigonometry
trig_fun <- function(x){
  y <- sin(x)*cos(x)
  return(y)
}

trig_fun(x=seq(from=-10,to=10,length.out = 100))

#Test and visualize
data.frame(x=seq(-10,10,length.out = 100))%>%
  mutate(y=trig_fun(x))%>%
  ggplot(aes(x=x,y=y))+geom_line()+theme_classic()

#Quadratic function
quad_fun <- function(x){
  y <- (x^2 + 1/x)/(3*x)
}

#Test and visualize
data.frame(x=seq(-10,10,length.out = 101))%>%
  mutate(y=quad_fun(x))%>%
  ggplot(aes(x=x,y=y))+geom_point()+theme_classic()
    

#Logistic growth-------

#I. Continuous time
#dN/dt = rN (1 - N/K)
#This ODE has an analytical solution:
#N(t) =  KN0e^(rt) / K + N0(e^(rt)-1)

#Anlytical
logistic_analytical <- function(t,N0,r,K){
  #Parameters: N0, r, and K
  y <- (K*N0*exp(r*t)) / (K + N0*(exp(r*t)-1))
  
  df <- data.frame('Nt'=y, 't'=t)
  return(df)
}
  
#Test and visualize
logistic_analytical(t=seq(1,50,length.out =1000),N0=100,r=0.4,K=700)%>%
  ggplot(aes(y=Nt,x=t))+geom_line()+theme_classic()

#Numerical solution with solver from deSolve
logistic_numerical <- function(N0,r,K,time){
  require(deSolve)
  parms <- c(r=r,K=K)
  y <- c(N=N0)
  
  model <- function (time, y, parms) {
    with(as.list(c(y, parms)), {
      dN <- r * N * (1 - N / K)
      list(dN)
    })
  }
  out <- ode(y, time, model, parms)
  
  return(as.data.frame(out))
}
?ode
#Test and visualize
#N0=100,r=0.4,K=700

logistic_analytical(t=seq(0,100,0.1),N0=100,r=0.4,K=1000)%>%
  ggplot(aes(y=Nt,x=t))+geom_line()+theme_classic()


logistic_numerical(N0 = 100,r = 0.4,K = 1000,time=seq(0,100,0.1))%>%
  ggplot(aes(x=time,y=N))+geom_line()+theme_classic()



#II. Discrete-time population growth
# Nt+1 = Nt + rNt ( 1 - Nt/K)

dlogistic <- function(K = 200, r = 1, N0 = 2, t = 15) {
  N <- c(N0, numeric(t))
  for (i in 1:t) {
    N[i+1] <- N[i] + r * N[i] * (1 - N[i]/K)
  }
  df <- data.frame(time = 0:t, N = N)
  
  return(df)
}

#Test and visualize
dlogistic(r=3,N0=20)%>%
  ggplot(aes(x=time,y=N))+geom_point()+geom_line()+theme_classic()


#Apply to iterative the values of r 

#candidate r values 
new_r <- c(1.5,2,2.5,3)

sapply(new_r,FUN=function(x)cbind(dlogistic(K=500,N0=10,t=20,r=x),r=x),
       simplify = FALSE)

lapply(out,function(x)x$r)
bind_rows(out)

dlogistic(K=1000,N0=10,t=20,r=1.5)%>%
  ggplot(aes(x=time,y=N))+geom_point()+geom_line()+theme_classic()



?sapply

########################
#Exercise: Use a for loop to see how dynamics change when parameter r varies-----
########################
  
#Details: The discrete-population growth model shows some interesting dynamics that you wouldn't
#otherwise see in its continous counterpart. Though the equation is relatively simple,
#at specific parameter spaces, the dynamics of the system may appear unpredictable and "random" but 
#its actually not! It's chaos (ie. fluctuations are non-random) ! The chaotic behavior stems from 
#the effect of initial conditions and the value of r. 

#To visualize the complex behavior of this system, write a function that calculates population size 
#from the discrete-growth population growth model at varying conditions of r. For simplicity, let's
#keep N0 and K the constant. 

#Tips: Consider writing a for loop that takes a new value of r at every iteration. Of course, 
#there are solutions in which you don't need a loop. Do whatever works for you!




#"Numerical" mean ------
#Random normally-distributed variable
rand <- rnorm(20)
hist(rand,breaks=10)
rand_mean <- mean(rand) # Analytical mean
rand_range <- range(rand) # range of values

#Create a sequence of "X's" in which to calculate Sum of Squares from
rand_x <- seq(from=rand_range[1],to=rand_range[2],length.out=100)

#Calculate Sum of Squares
ss <- sapply(rand_x,FUN=function(x)sum((x-rand)^2))

#sum((rand_x[1]-rand)^2)
#rand

#Store data dataframe
ss_mean <- data.frame(SS=ss,rand_x = rand_x,time=1:100)
rand_df <- data.frame(randomX = rand, index = 1:20)


#Visualize
ggplot(rand_df,aes(x=index,y=randomX))+geom_point(size=4)+
  geom_hline(yintercept=mean(rand)+0.5)+
  geom_segment(aes(x=index,y=randomX,xend=index,yend=mean(rand)+0.5))+
  theme_minimal()

#Plots for animating
NewX_df <-  data.frame(NewX=rep(rand_x,each=20),
                       randomX = rep(rand,times=100),
                       index = rep(1:20,times=100),
                       frame = rep(1:20,each=100))

SSplot <- ggplot(NewX_df,aes(x=index,y=randomX))+geom_point(size=3)+
  #geom_point(rand_df,aes(x=index,y=randomX))
  geom_hline(aes(yintercept=NewX))+
  geom_segment(aes(x=index,y=randomX,xend=index,yend=NewX))+
  theme_minimal()

SSmini <- ggplot(ss_mean,aes(x=rand_x,y=ss))+geom_point()+geom_line()+
  labs(x='X',y='Sum of Squares')+geom_vline(xintercept=rand_mean,col='red')+
  geom_text(x=0.3,y=50,label='Mean = 0.23',size=5)+coord_flip()+
  ggtitle('Minimize Sum of Squares')+
  theme_classic()


#Animate side by side

SSanim <- SSplot + labs(title = 'X: {frame_time}', x = 'Index', y = 'X') +
  transition_time(frame)

SSmini_anim <- SSmini+transition_reveal(time)

#patch together
gifA  <- animate(SSanim,width=260,height=260)
gifB <- animate(SSmini_anim,width=260,height=260)

a_mgif <- image_read(gifA)
b_mgif <- image_read(gifB)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif






















