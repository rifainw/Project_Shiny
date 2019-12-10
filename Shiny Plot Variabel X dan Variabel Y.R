library(shiny)
library(plotly)
ui <- fluidPage(
  plotlyOutput("plotRainfall"),
  plotlyOutput("plotTemperature")
)
server <- function(input, output, session) {
  library(openxlsx)
  
  # input data Productivity pada file Data_Project (sheet 1) ke R
  Productivity <- read.xlsx("C:/Rifai/PROJECT/Data_Project.xlsx",sheet=1,startRow=1,colNames = TRUE)
  View(Productivity)
  
  # input data Rainfall pada file Data_Project (sheet 2) ke R
  Rainfall <- read.xlsx("C:/Rifai/PROJECT/Data_Project.xlsx",sheet=2,startRow=1,colNames = TRUE)
  View(Rainfall)
  
  # input data Temperature pada file Data_Project (sheet 3) ke R
  Temperature <- read.xlsx("C:/Rifai/PROJECT/Data_Project.xlsx",sheet=3,startRow=1,colNames = TRUE)
  View(Temperature)
  
  # mengambil data kolom pertama yaitu variabel District
  district <- Productivity[,1]
  head(district)
  
  # membuat pengulangan variabel district
  district_panel <- c()
  for (i in 1:5) # karena terdapat 5 district
  {x = rep(district[i],12)
  district_panel = append(district_panel,x)
  }  
  View(district_panel)
  
  # membuat vektor replikasi dari tahun CaturWulan pertama tahun 2013 - CaturWulan ketiga tahun 2016
  time_panel <- rep(1:12, 5)
  time_panel
  
  # panel untuk Productivity
  Productivity_panel <- c()
  for (i in 1:5)
  { x = Productivity[i,]
  x = x[-1]
  x = t(x)
  Productivity_panel = append(Productivity_panel,x)
  }  
  head(Productivity_panel)
  
  # panel untuk Rainfall
  Rainfall_panel <- c()
  for (i in 1:5)
  { x = Rainfall[i,]
  x = x[-1]
  x = t(x)
  Rainfall_panel = append(Rainfall_panel,x)
  }  
  head(Rainfall_panel)
  
  # panel untuk Temperature
  Temperature_panel <- c()
  for (i in 1:5)
  { x = Temperature[i,]
  x = x[-1]
  x = t(x)
  Temperature_panel = append(Temperature_panel,x)
  }  
  head(Temperature_panel)
  
  # membuat data frame dari ke-lima variabel panel di atas
  Project_frame <- data.frame(district_panel, time_panel, Productivity_panel, Rainfall_panel, Temperature_panel)
  View(Project_frame)
  
  library(plotly)
  library(ggplot2)
  output$plotRainfall <- renderPlotly({
    ggplotly(ggplot(Project_frame, aes(x=Rainfall_panel,y=Productivity_panel,color=district_panel)) + geom_point(aes(shape=district_panel,frame=time_panel)))
  })
  output$plotTemperature <- renderPlotly({
    ggplotly(ggplot(Project_frame, aes(x=Temperature_panel,y=Productivity_panel,color=district_panel)) + geom_point(aes(shape=district_panel,frame=time_panel)))
  })
}
shinyApp(ui, server)
shiny::runApp()