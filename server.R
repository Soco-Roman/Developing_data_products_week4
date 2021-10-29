#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

zipdata <- read.csv("_data/acumulados_2.csv", fileEncoding = 'UTF-8')
zipdata$pct_muertes <- (zipdata$muertes /zipdata$acumlds)*100
zipdata$pct_activos <- (zipdata$activos /zipdata$acumlds)*100

popup_column <- function(x, r, my_variables, pretty_names, digits = 0){
  # select row r from x dataframe
  x <- x[r, c(my_variables)]
  # round those variables which are numeric
  for(v in 1:length(my_variables)){
    if(is.numeric(x[,v])){
      x[,v] <- round(x = x[,v], digits = digits)
    } else {}
  }
  # traspose
  x = data.frame(t(x))
  # use pretty names
  row.names(x) <- pretty_names
  # create html popup
  x = paste0("<b> ", row.names(x), ": </b>", x[,1])
  paste0(x, collapse = "<br/>")
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      setView(lng = -100.85, lat = 23.45, zoom = 5)
  })
  
  observe({
    sizeBy <- input$size
    
    radius <- zipdata[[sizeBy]]/max(zipdata[[sizeBy]])*20
    data <- zipdata
  
    data$popup <- sapply(X = 1:nrow(data),
                         FUN = popup_column,
                         x = data,
                         my_variables = c("estado", "NOMGEO","acumlds", "activos", "muertes", "pct_muertes", "pct_activos"),
                         pretty_names = c("State", "Municipality", "Total cases", "Active cases", "Deaths", "Percentage of deaths", "Percentage of active cases"))
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(~X, ~Y,
                       radius = radius, stroke = F, fillOpacity = 0.4, 
                       color = ifelse(sizeBy == "acumlds","#fc8961",ifelse(sizeBy == "activos", "#51127c","red")),
                       popup =~popup)
      #addLegendCustom(colors = c("#fc8961", "#51127c", "red"), labels = c("Total cases", "Active", "Deaths"), sizes = c(10, 20, 40))
      # addLegend("bottomleft", pal=c("red", "blue", "green"), values=radius, title=sizeBy,
      #           layerId="colorLegend")
    # addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #           layerId="colorLegend"
    
  })
})
