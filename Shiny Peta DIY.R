library(shiny)
ui <- fluidPage(
  plotOutput("plot")
)
server <- function(input, output, session) {
  library(GADMTools)
  library(sp)
  library(tidyverse)
  
  mapINA_Prov <- gadm_sp_loadCountries("IDN", level = 2, basefile = "./")
  Prov <- listNames(mapINA_Prov, level = 2)
  
  View(Prov)
  length(Prov)
  pop <- floor(runif(length(Prov), min=40, max=80))
  DATTT <- data.frame(Prov, pop)
  DIY <- gadm.subset(mapINA_Prov, regions = c("Bantul", "Gunung Kidul", "Kota Yogyakarta", "Kulon Progo", "Sleman"))
  output$plot <- renderPlot(
    choropleth(DIY, DATTT,
               adm.join = "Prov",
               value = "pop",
               breaks = "sd",
               palette = "Greens",
               legend = "Produktivitas",
               title = "Daerah Istimewa Yogyakarta")   
    
  )
}
shinyApp(ui, server)