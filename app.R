
library(shiny)
library(ggplot2)


# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Customize the body background color */
      body {
        background-color: #90e0ef;
      }
      
      /* Adjust the panel colors */
      .well {
        background-color: #fff;
        border: 1px solid #ccc;
      }
     
      
     
      
      
      /* Style the title */
      h1 {
        text-align: center;
        font-size: 24px;
        color: white;
      }
      
      /* Style the plot output */
      .shiny-plot-output {
        margin-top: 20px;
        box-shadow: 0px 0px 6px rgba(0, 0, 0, 0.1);
      }
      
      /* Style the sidebar */
      .sidebar {
        background-color: #fff;
        border-right: 1px solid #ccc;
        padding: 15px;
      }
    "))
  ),
  titlePanel("The Visualization of Iris Dataset Using Different Charts"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X Variable", choices = names(iris)),
      selectInput("y_var", "Y Variable", choices = names(iris)),
      selectInput("plot_type", "Plot Type",
                  choices = c("Scatter Plot", "Box Plot", "Bar Plot", "Histogram", "Density Plot"))
    ),
    mainPanel(
      plotOutput("plot", height = "400px")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create the plot based on user inputs
  output$plot <- renderPlot({
    data <- iris %>% select(input$x_var, input$y_var, Species)
    
    # Scatter plot for different species
    if (input$plot_type == "Scatter Plot") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var, color = "Species")) +
        geom_point() +
        labs(title = "Scatter Plot for Iris dataset for different species") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF"),
                           labels = c("Setosa", "Versicolor", "Virginica"))
    }
    
    # Box plot
    else if (input$plot_type == "Box Plot") {
      ggplot(data, aes_string(x = "Species", y = input$y_var, fill = "Species")) +
        geom_boxplot() +
        labs(title = "Box Plot for Iris dataset for different species") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF"),
                          labels = c("Setosa", "Versicolor", "Virginica"))
    }
    
    # Bar plot
    else if (input$plot_type == "Bar Plot") {
      ggplot(data, aes_string(x = "Species", fill = "Species")) +
        geom_bar() +
        labs(title = "Bar Plot for Iris dataset for different species") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF"),
                          labels = c("Setosa", "Versicolor", "Virginica"))
    }
    
    # Histogram
    else if (input$plot_type == "Histogram") {
      ggplot(data, aes_string(x = input$x_var, fill = "Species")) +
        geom_histogram() +
        labs(title = "Histogram for Iris dataset for different species") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF"),
                          labels = c("Setosa", "Versicolor", "Virginica"))
    }
    
    # Density plot
    else if (input$plot_type == "Density Plot") {
      ggplot(data, aes_string(x = input$x_var, fill = "Species")) +
        geom_density() +
        labs(title = "Density Plot for Iris dataset for different species") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
        scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF"),
                          labels = c("Setosa", "Versicolor", "Virginica"))
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
