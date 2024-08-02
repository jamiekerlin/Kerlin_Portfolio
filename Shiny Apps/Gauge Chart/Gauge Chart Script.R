# Gauge chart template ####################################################
# Created by Jamie Kerlin
# Created on 1/30/2024

# Following plotly code here: #https://plotly.com/r/gauge-charts/

### Load libraries ########################################################
library(plotly)


### Load data #############################################################

# Save CSUN Colors as Objects for easier usage
red <- "#D2202A"
med_gray <- "#ABA9A9"
iris <- "#5B4C93"
cool_gray <- "#898989"
azure <- "#2CA5DA"
periwinkle <- "#92A1CD"
yellow <- "#FDBF56"
beige <- "#D8D2C4"


value <- 


### Create plot ###########################################################

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)), # creates the background plot
  value = 14.59, #sets the value that you want to use as the current value
  title = list(text = "Percent of Students, Fall 2023"), #gives a title above the chart
  type = "indicator", #tells us what type of chart we are using
  mode = "gauge+number+delta", # here, we can change this to not include the number or delta by removing that portion
  delta = list(reference = 10.68, increasing = list(color = periwinkle)), #if we are including delta, add reference and other changes here
  gauge = list( #aesthetics within the gauge chart
    axis =list(range = list(NULL, 20)), #range of the gauge chart e.g. here is 0 to 20
    bar = list(color = yellow), #color of the bar for the current value shown on gauge chart
    steps = list( #if you want different color backgrounds to show "steps" within the gauge chart like previous year
      list(range = c(0, 10.68), color = "lightgrey"), #the range in the c(,) argument gives the portion of the axis you are referencing and the color argument gives the color you want the background to be
      list(range = c(10.68, 14.59), color = periwinkle), #same as previous line
      list(range = c(14.59, 20), color = "white")), #same as previous line
    threshold = list( #threshold adds a line at a value along the axis
      line = list(color = "black", width = 2), #what color and thickness you would like the line
      thickness = 1, #1 if you would like the line all the way across the gauge, <1 if you would like it to be shorter
      value = 14.59))) #where you would like the line
fig <- fig %>% #call back the figure object
  layout(margin = list(l=20,r=30), #change the margins
         font = list(family = "Arial", size = 18)) #change the font

fig #print the figure
