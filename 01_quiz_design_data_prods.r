
# Quizz #1 coursera on Developing Data Products

# https://class.coursera.org/devdataprod-002/quiz/attempt?quiz_id=23

setwd("C:/Users/pedroc/Desktop/courseras 2014/coursera design data products")

# 1

# This requires  manipulate library

library(manipulate)

data(cars)
summary(cars)

library(manipulate)

# Basic code to include a slider

## Create a plot with a manipulator
manipulate(plot(1:x), x = slider(5, 10))


myPlot <- function(s) {
  plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
  abline(0, s)
}

# This function plots distance versus speed, each de-meaned and an associated line of slope s. 
# Which of the following code will make a manipulate plot that creates a slider for the slope?
manipulate(myPlot(s), s = slider(0, 2, step = 0.1))
manipulate(myPlot(s), x.s = slider(0, 2, step = 0.1))
manipulate(myPlot, s = slider(0, 2, step = 0.1))
manipulate(myPlot(s), slider = x(0, 2, step = 0.1))

# 2

# Which of the following code uses the rCharts package to create a 
# sortable and searchable data table for the airquality data set? 
# Assume the rCharts package and the airquality data set have already been loaded into R.

require(devtools)
install_github('rCharts', 'ramnathv')

library(rCharts)

data(airquality)
summary(airquality)

dTable(airquality, sPaginationType = "full_numbers")
d
airquality
head(airquality)


# 4

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Data science FTW!"),
  sidebarPanel(
    h2('Big text'),
    h3('Sidebar')
  ),
  mainPanel(
    h3('Main Panel text')
  )
))
