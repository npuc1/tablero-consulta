# Load required libraries
library(shiny)
library(ggplot2)
library(rintrojs)

# Define UI for the app
ui <- fluidPage(
  # Initialize rintrojs
  introjsUI(),
  
  # App title
  titlePanel("Shiny App with Two Tabs and Conditional Tours"),
  
  # Sidebar layout with tabs
  sidebarLayout(
    sidebarPanel(
      # Tab selector
      tabsetPanel(id = "tabs",
                  tabPanel("Tab 1", value = "tab1",
                           # Numeric input for the number of observations
                           introBox(
                             numericInput("obs1", "Number of observations:", 50, min = 1, max = 1000),
                             data.step = 1, data.intro = "This is the numeric input for Tab 1."
                           ),
                           # Description for the plot
                           p("This plot shows a simple histogram of random data.")
                  ),
                  tabPanel("Tab 2", value = "tab2",
                           # Slider input for another kind of data
                           sliderInput("obs2", "Number of bins:", 10, min = 5, max = 50, width = "100%"),
                           p("This plot shows a histogram with custom bin sizes.")
                  )
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      conditionalPanel(
        condition = "input.tabs == 'tab1'",
        introBox(
          plotOutput("distPlot1"),
          data.step = 2, data.intro = "This is where the graph for Tab 1 will be displayed."
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'tab2'",
        introBox(
          plotOutput("distPlot2"),
          data.step = 2, data.intro = "This is where the graph for Tab 2 will be displayed."
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Generate a random distribution based on user input for Tab 1
  output$distPlot1 <- renderPlot({
    data <- rnorm(input$obs1)
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(binwidth = 0.2, fill = "blue", color = "white") +
      labs(title = "Histogram of Random Data (Tab 1)", x = "Value", y = "Frequency")
  })
  
  # Generate a random distribution with custom bins for Tab 2
  output$distPlot2 <- renderPlot({
    data <- rnorm(100)
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(bins = input$obs2, fill = "green", color = "black") +
      labs(title = "Histogram with Custom Bins (Tab 2)", x = "Value", y = "Frequency")
  })
  
  # Start the appropriate tour based on the selected tab
  observe({
    if (input$tabs == "tab1") {
      introjs(session, options = list(steps = list(
        list(element = "#obs1", intro = "This is the numeric input for Tab 1."),
        list(element = "#distPlot1", intro = "This is the plot for Tab 1.")
      )))
    } else if (input$tabs == "tab2") {
      introjs(session, options = list(steps = list(
        list(element = ".irs-with-grid", intro = "This is the slider input for Tab 2."),
        list(element = "#distPlot2", intro = "This is the plot for Tab 2.")
      )))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
