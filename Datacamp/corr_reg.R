library(ggplot2)
library(dplyr)


mtcars

ggplot(mtcars,aes(x=cut(wt,breaks=5),y=mpg))+geom_point()

ggplot(mtcars,aes(x=cut(wt,breaks=5),y=mpg))+geom_boxplot()


## Using Factors 
ggplot(mtcars, aes(x=hp,y=mpg, color=factor(gear)))+geom_point()


## Scaling the co-ordinates to Log of x and Y 
ggplot(mtcars, aes(x=hp,y=mpg, color=factor(gear)))+geom_point() + 
  coord_trans(x = "log10", y = "log10")

## Same  as above 
ggplot(mtcars, aes(x=hp,y=mpg, color=factor(gear)))+geom_point() + 
  scale_x_log10() + scale_y_log10()

############### co-rrelation Function ############ 
################################################# 

# Finding Basic co-relation between two Numbers

cor(mtcars$disp,mtcars$hp)

mtcars %>% 
  summarize(N=n(),r=cor(disp,hp))

#N = n() -- Counts the data values
#r = gives the correlation 

mtcars %>% 
  summarize(No_Rows = n(), correlation = cor(disp,hp))

mtcars %>% 
  summarize(No_Rows = n(), correlation = cor(disp,hp,use = "pairwise.complete.obs"))


# use = pairwise.complete.ob, removes the NA values and performs the calculations

#################################################
## Anscombe Dataset 
#################################################

setwd("D:\\Users\\shash\\Documents\\GitHub\\Tutorials_R_PY\\R\\Datacamp")

Anscmbe <- read.csv("Anscombe.csv")

Anscmbe <- data.frame(Anscmbe)

head(Anscmbe)


Anscmbe %>% 
  group_by(group) %>% 
  summarize( N = n(),    ##### Counts the Number of Observations 
             mean_of_x = mean(x),
             std_dev_of_x = sd(x),
             mean_of_y = mean(y),
             std_dev_of_y = sd(y),
             correlation_x_y = cor(x,y)
  )

# Normal Correlation vs Logarithmic correlation.

cor(Anscmbe$x, Anscmbe$y)
cor(log(Anscmbe$x),log(Anscmbe$y))

#################################################
## Visualizing Linear model
#################################################

ggplot(data = mtcars,aes(x=disp,y=mpg)) + 
  geom_point() + geom_smooth(method = "lm",se=FALSE)

## se = False turns off the Standard error Region . 


ggplot(data = mtcars,aes(x=disp,y=mpg)) + 
  geom_point() + geom_smooth(method = "lm")





