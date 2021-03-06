#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)

vars <- c(
  "Acumulados"="acumlds",
  "Activos"="activos",
  "Muertes"="muertes"
)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Number of cases", min(points$acumlds), max(points$acumlds),
                #             value = range(points$acumlds), step = 0.1
                # ),
                selectInput("size", "Variable", vars, selected = "acumlds"))
 
  )
)
