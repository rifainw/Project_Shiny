
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Linear Regression"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Rainfall",
                  "Select rainfall:",
                  min = 40,
                  max = 2500,
                  value = 100),
      sliderInput("Temperature",
                  "Select temperature:",
                  min = 20,
                  max = 40,
                  value = 50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("distPlot"),
      plotOutput("plotRain"),
      plotOutput("plotTemp"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderTable({
    setwd("C:/Rifai/PROJECT/")
    
    RegLin <- read.csv("Data_Projectt.csv");
    DF <- data.frame(Prod = RegLin$Productivity, Rain = RegLin$Rainfall, Temp = RegLin$Temperature);
    DF
    
    Model_lm <- lm(Prod ~ Rain+Temp, data=DF)
    summary(Model_lm)
    
    NewValue1 <- data.frame(Rain = input$Rainfall, Temp = input$Temperature)
    NewValue1
    
    
    Spend_Value1 <- predict(Model_lm, NewValue1)
  })
  output$plotRain <- renderPlot({
    setwd("C:/Rifai/PROJECT/")
    
    RegLin <- read.csv("Data_Projectt.csv");
    DF <- data.frame(Prod = RegLin$Productivity, Rain = RegLin$Rainfall, Temp = RegLin$Temperature);
    DF
    
    Model_lm <- lm(Prod ~ Rain+Temp, data=DF)
    summary(Model_lm)
    
    plot(RegLin$Rainfall, RegLin$Productivity)
    abline(Model_lm, col = "blue")
    
  })
  output$plotTemp <- renderPlot({
    setwd("C:/Rifai/PROJECT/")
    
    RegLin <- read.csv("Data_Projectt.csv");
    DF <- data.frame(Prod = RegLin$Productivity, Rain = RegLin$Rainfall, Temp = RegLin$Temperature);
    DF
    
    Model_lm <- lm(Prod ~ Rain+Temp, data=DF)
    summary(Model_lm)
    
    plot(RegLin$Temperature, RegLin$Productivity)
    abline(Model_lm, col = "red")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

