library(plotly)

days <- c(1:30)
cumm_sales_cy <- rnorm(30,mean=30000,sd=3)
cumm_sales_py <- rnorm(30,mean=30000,sd=3)

df1 <- data.frame(days,cumm_sales_cy,cumm_sales_py)


#################### Plotly Charting Functions ###########################


####### Basic Scatter plot #######

fig <- df1 %>% plot_ly(x = ~days, y = ~cumm_sales_py)
fig

####### Styling the Markers 

mrk_styl = list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 2))

fig <- df1 %>% plot_ly(x = ~days, y = ~cumm_sales_py, marker = mrk_styl)
fig

####### Plotting Markers and lines

# To add a chart with multiple y-axes values, add the X-axis first. 
# Then add multiple y-axes as a separate trace value. 

fig <- plot_ly(df1, x = ~days, name = "Days")
fig <- fig %>% add_trace(y = ~cumm_sales_cy, name = "Cummulative Sales Current Year",mode = "lines")
fig <- fig %>% add_trace(y = ~cumm_sales_py, name = 'Cummulative Sales Prev Year', mode = 'lines+markers', marker = mrk_styl)
fig 


####### Adding Colors 

fig <- plot_ly(df1, x = ~days, y = ~cumm_sales_cy,
               color = ~cumm_sales_cy, size = ~cumm_sales_cy, mode = 'markers')

fig

####### Adding Data Labels to Hover Text 


#rounded_sales <- format(round(cumm_sales_cy/1000, digits=2),nsmall = 2)

fig <- df1 %>% plot_ly(x= ~days, y = ~cumm_sales_cy,
                       #Adding the Hover Text now
                       text = ~paste("Sales: ",round(cumm_sales_cy/1000,2),"K",'$USD<br>Day num: ',days)
                       ,marker=mrk_styl, mode='lines+markers')
fig


####### Adding Ticks to X-Axis in R 

# This will show all the x-axis values 

fig <- plot_ly(df1,x=~days,y=~cumm_sales_py,mode='lines+markers')
fig <- fig %>% layout(xaxis = list(tickmode="linear"))

fig

## If ticks are in an array : 
##  xaxis = list(ticktext = list("One", "Three", "Five", "Seven", "Nine", "Eleven"), 
##    tickvals = list(1, 3, 5, 7, 9, 11),
##    tickmode = "array"
##  )


####### Applying labels to AXES


fig <- plot_ly(df1, x=~days)
fig <- fig %>% add_trace(y= ~cumm_sales_py,name= "Sales PY", mode = "lines+markers",
                         text = ~paste("Sales: ",round(cumm_sales_py/1000,2),"K",'$USD<br>Day num: ',days)
                         ,marker=mrk_styl)

fig <- fig %>% add_trace(y = ~cumm_sales_cy, name = "Sales CY", mode = "lines+markers",
                         text = ~paste("Sales: ",round(cumm_sales_cy/1000,2),"K",'$USD<br>Day num: ',days)
                         ,marker=mrk_styl)

fig <- fig %>% layout(xaxis = list(tickmode="linear",title="Days"),yaxis=list(title="Sales"))
fig
