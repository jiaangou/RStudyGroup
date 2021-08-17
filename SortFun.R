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
    
    #update dataframe
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

plot(sorted_df$random_sequence)
plot(sorted_df$random_sequence)

library(gganimate)

sort_p <- sorted_df %>%
  tibble::rownames_to_column(var='sequence')%>%
  pivot_longer(cols = -1,names_to='iteration')%>%
  mutate(iteration_no = substr(iteration,10,10))%>%
  mutate(iteration_no = as.numeric(iteration_no))%>%
  ggplot(aes(x=sequence,y=value))+
  geom_bar(stat = 'identity')

fake_p + transition_states(
  iteration_no,
  transition_length = 2,
  state_length = 1
)

?transition_reveal
 
plot(sorted)







#Mathematical functions


trig_fun <- function(x){
  y <- sin(x)*cos(x)
  return(y)
}

trig_fun(x=seq(-10,10,length.out=100))%>%plot(type='l')

quad_fun <- function(x){
  y <- (x^2 + 1/x)/(3*x)
}

quad_fun(x=seq(-10,10,length.out=100))%>%plot(type='l')

seq(10,length.out=100,by = 0.001)


#Logistic growth-------


#I. Continuous time
#dN/dt = rN (1 - N/K)
#This ODE has an analytical solution:
#N(t) =  KN0e^(rt) / K + N0(e^(rt)-1)

logistic_analytical <- function(t,N0,r,K){
  #Parameters: N0, r, and K
  y <- (K*N0*exp(r*t)) / (K + N0*(exp(r*t)-1))
  
  df <- data.frame('Nt'=y, 't'=t)
  return(df)
}
  
  
logistic_analytical(t=seq(1,50,length.out =1000),N0=100,r=0.4,K=3000)%>%
  ggplot(aes(y=Nt,x=t))+geom_line()+theme_classic()


logistic_numerical <- function(init,r,K,times){
  require(deSolve)
  model <- function (time, y, parms) {
    with(as.list(c(y, parms)), {
      dN <- r * N * (1 - N / K)
      list(dN)
    })
  }
  
  y=init
  parms <- data.frame(r=r,K=K)
  
  out <- ode(y,times,model,parms)
  return(ode)
}


logistic_numerical(init = 0.1,r = 0.3,K = 1000,times=seq(0,100,0.1))

model <- function (time, y, parms) {
  with(as.list(c(y, parms)), {
    dN <- r * N * (1 - N / K)
    list(dN)
  })
}
y <- c(N = 0.1)
parms <- c(r = 0.1, K = 10)
times <- seq(0, 100, 1)

out <- ode(y, times, model, parms)


library(deSolve)
example(deSolve)



discrete_logistic <- function(r,N0,K,range,increment=0.001){
  #range of x from 0 
  #increment = X's increment of increase
  
  pop <- N0
  Xs <- seq(from=1,to=range,by=increment)
  
  for (i in 2:length(Xs)){
    N0 <- pop[i-1]
    N_plus1 <- r*N0*(1-N0)
    pop <- append(pop,N_plus1)
  }
  
  df <- data.frame('Nt' = pop, 't' = Xs)
  return(df)
}


seq(from=1,to=10,by=0.01)

discrete_logistic(r=0.9,N0=0.2,range=30,increment = 0.1)%>%
  ggplot(aes(x=t,y=Nt))+geom_line()+theme_classic()


# Xn+1 = rXn ( 1 - Xn)


#Numerical mean ------

#Random normally-distributed variable
rand <- rnorm(20)
rand_mean <- mean(rand) # Analytical mean
rand_range <- range(rand) # range of values

#Create a sequence of "X's" in which to calculate Sum of Squares from
rand_x <- seq(from=rand_range[1],to=rand_range[2],length.out=100)

#Calculate Sum of Squares
ss <- sapply(rand_x,function(x)sum((x-rand)^2))

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
library(gganimate)
SSanim <- SSplot + labs(title = 'X: {frame_time}', x = 'Index', y = 'X') +
  transition_time(frame)

SSmini_anim <- SSmini+transition_reveal(time)

#patch together
library(magick)

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






















