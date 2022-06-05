#install.packages("d3r")
#install.packages("googleVis")
#install.packages("htmltools")

#library(d3r)
#library(googleVis)
#library(dplyr)
#library(htmltools)

library(tidyr)
library(plotly)

df1 <- read.csv("D:/Users/shash/Documents/Learning/Visualizations/Dataset/covid-19-data-resource-hub-covid-19-case-counts/data/covid_19_cases.csv")

head(df1)
colnames(df1)

Ind_cases <- df1 %>% filter(cases >0 & country_region == "India") %>% group_by(date)
head(Ind_cases)
colnames(Ind_cases)

Ind_cases$cases

#data(gapminder, package = "gapminder")
#gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
 # geom_point(aes(size = pop, frame = year, ids = country)) +
  #scale_x_log10()
#ggplotly(gg)

###### Function to accumulate Dataset ########

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


#gg <- ggplot(Ind_cases,aes(y=cases,x=date,color=case_type)) + geom_point(aes(frame = date))
#gg %>% animation_opts(1000, easing = "elastic", redraw = FALSE)
#base %>%  add_markers(data = gapminder, frame = ~continent) %>% hide_legend() %>% animation_opts(frame = 1000, transition = 0, redraw = FALSE)

gg <- Ind_cases %>% accumulate_by(~date)

gg <- gg %>% 
  plot_ly(x = ~date, y = ~cases, split = ~case_type, frame = ~date, type = 'scatter',  mode = 'lines',  line  = list(simplyfy = F))

gg <- gg %>% layout(
  xaxis = list(title = "Date",zeroline = F),
  yaxis = list(title = "Cases", zeroline = F)
)

gg <- gg %>% animation_opts(frame = 100, transition = 0, redraw = FALSE)

gg <- gg %>% animation_slider(hide = F)

gg <- gg %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom")

gg

########### Sample PLotly Code for Cumulative Line Chart Animation ####################
#######################################################################################

library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- txhousing 
fig <- df %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area"))

fig <- fig %>% accumulate_by(~date)


fig <- fig %>%
  plot_ly(x = ~date,y = ~median,split = ~city,frame = ~frame,type = 'scatter',mode = 'lines',line = list(simplyfy = F))

fig <- fig %>% layout(
  xaxis = list(title = "Date",zeroline = F),
  yaxis = list(title = "Median",zeroline = F)
) 

fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig
#######################################################################################

txhousing


