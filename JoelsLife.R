#Joel's life plan
library(dplyr)
library(ggplot2)

periodic_interest <- function(P0, r, t, salary, retirement_t, allowance){
  P <- c(P0,numeric(t)) #initiate vector with length = time + 1
  
  for (i in 1:t){
    if(i<retirement_t){
      P[i+1] <- P[i]+salary+P[i]*r
    }
    else{
      P[i+1] <- P[i]+(P[i]*r)-allowance
    }
  }
  df <- data.frame(P = P,t = 1:(t+1))
  return(df)

}

#Simulate data given certain conditions 
retire <- seq(0,40,by=4)

simulated_data <- sapply(retire,function(x)periodic_interest(P0=100000,r=0.03,t=80, #applies the function to each value of the "retire" vector 
                    salary=60000,retirement_t=x,allowance=60000),simplify = FALSE)%>%
  bind_rows()%>% #compress lists elements into a single dataframe by binding rows
  mutate(retirement = rep(retire,each=81)) #manually add retirement variable and its values
  
#Plots curves in the same panels
simulated_data%>%
ggplot(aes(x=t,y=P,group=retirement,col=retirement))+
  geom_point()+geom_line()+theme_classic()

#Split retirement values into separate panels using facet_wrap() function
simulated_data%>%
  ggplot(aes(x=t,y=P,group=retirement,col=retirement))+
  geom_point()+geom_line()+theme_classic()+facet_wrap(~retirement) 







