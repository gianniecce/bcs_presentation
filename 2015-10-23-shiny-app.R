


shinyApp(
  ui = fluidPage(
    sliderInput("time",
                "Choose the time of the day",
                min = 1,
                max = 1440,
                value = c(1,1440)),
    selectInput("day", "days:", choices = levels(factor(seqDay5$diaryday)) ),
    plotOutput("sequence")
  ),
  
  server = function(input, output) {
    output$sequence <- renderPlot({
      x = seq(from = min(input$time), to = max(input$time), by = 1)
      p = ggplot( filter(seqDay5, diaryday == input$day)[seqDay5$Time %in% c(x), ] , aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal()
      print(p)
    })
  }, 
  options = list(height = 1000)
)




filter(seqDay5, diaryday == 1)
filter(seqDay5, diaryday == 1)[seqDay5$Time %in% c(1:19), ]


library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of
  # the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Gender", 
               "Day",
               "Range"),
      Value = as.character(c(input$integer, 
                             input$decimal,
                             paste(input$range, collapse=' ')), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
})





library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      # Simple integer interval
      sliderInput("integer", "Integer:", 
                  min=0, max=1000, value=500),
      
      # Decimal interval with step value
      sliderInput("decimal", "Decimal:", 
                  min = 0, max = 1, value = 0.5, step= 0.1),
      
      # Specification of range within an interval
      sliderInput("range", "Range:",
                  min = 1, max = 1000, value = c(200,500)),
      
      # Provide a custom currency format for value display, 
      # with basic animation
      sliderInput("format", "Custom Format:", 
                  min = 0, max = 10000, value = 0, step = 2500,
                  format="$#,##0", locale="us", animate=TRUE),
      
      # Animation with custom interval (in ms) to control speed,
      # plus looping
      sliderInput("animation", "Looping Animation:", 1, 2000, 1,
                  step = 10, animate=
                    animationOptions(interval=300, loop=TRUE))
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("values")
    )
  )
))


