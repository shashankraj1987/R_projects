library(gapminder)
library(dplyr)
library(ggplot2)

gapminder
dim(gapminder)

gapminder %>% 
  filter(continent == "Asia")

# Arrange function 

gapminder %>% 
  arrange(desc(pop)) %>% ## Descending Order 
  arrange(lifeExp) ## Arrange in Ascending Order 

# Combining Two Filters 
gapminder %>% 
  arrange(desc(gdpPercap)) %>%  
  filter(year == 2007)  

## Mutate 
############################

gapminder %>% 
  mutate(pop = pop/1000000) %>% 
  arrange(desc(pop)) %>% 
  filter(year == 2007) %>% 
  mutate(gdp = (gdpPercap * pop)/1000000)


### GGPLOT2 ### 
###########################

gapminder_2k7 <- gapminder %>%
                    filter(year==2007)

ggplot(gapminder_2k7,aes(x=gdpPercap,y=lifeExp))+geom_point()

# Logarithmic plot of x 

ggplot(gapminder_2k7,aes(x=pop,y=lifeExp)) + geom_point() + scale_x_log10()

ggplot(gapminder_2k7,aes(x=pop,y=lifeExp, color = continent, size = pop)) + geom_point() + scale_x_log10() + scale_y_log10()



### Facetting -- Divide a plot into sub-plots by some feature 

ggplot(gapminder_2k7,aes(x=pop,y=lifeExp, color = continent, size = pop)) + geom_point() + scale_x_log10() + scale_y_log10() + facet_wrap(~ continent)

ggplot(gapminder,aes(x=gdpPercap,y=lifeExp, color= continent, size=pop))+geom_point()+scale_x_log10()+scale_y_log10()+ facet_wrap(~ year)


## Summarizing the Data
#Filter for the year 1957, then use the median() function within a summarize() to calculate the median life expectancy into a column called medianLifeExp.

gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp))

gapminder %>% filter(year==1957) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))


## Visualizing the Summarized Data

gapminder_1957 <- gapminder %>% group_by(year,continent) %>% summarize(medianLifeExp = median(lifeExp), meanpop = mean(pop)/1000000)

ggplot(gapminder_1957,aes(x=year,y=meanpop, color = continent)) + geom_point() + expand_limits(y=0)


# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
by_continent_2007 %>% ggplot(aes(x=medianGdpPercap,y=medianLifeExp, color= continent)) + geom_point()+expand_limits(y=0)

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>% group_by(year) %>% summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year,aes(x=year,y=medianGdpPercap)) + geom_line() + expand_limits(y=0)

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>% group_by(year,continent) %>% summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent,aes(x=year,y=medianGdpPercap, color=continent))+geom_line()+expand_limits(y=0)

# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>% filter(year==1952) %>% group_by(continent) %>% summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
by_continent %>% ggplot(aes(x=continent,y=medianGdpPercap))+geom_col()

## geom_col == Bar Graph 
gapminder_1952 <- gapminder %>% filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952,aes(pop))+geom_histogram()+scale_x_log10()

# Histogram is used to find the Distribution of one variable across time. The bins = is used to determine the range for which 
# we want the distribution to run 

## Box Plots 
gapminder_1952 <- gapminder %>% filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952,aes(x=continent,y=gdpPercap))+geom_boxplot()+scale_y_log10()+ggtitle("Hisplot")



gapminder %>% summarize(gdpPercap)
