# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(datateachr)
library(DT)
library(colourpicker)

# Load your dataset
trees_data <- vancouver_trees

# Define UI
ui <- fluidPage(
  titlePanel("Tree Diameter Categories by Genus"),
  sidebarLayout(
    sidebarPanel(
      #Feature 0: The code below is a slider that allows the user to define what tree diameter counts as thin, medium, or thick trees.
            selectInput("genus", "Choose a Genus:", choices = unique(trees_data$genus_name)),
            sliderInput("thinThreshold", "Thin-Medium Threshold (Diameter in inches):", min = 0, max = 15, value = 3),
            sliderInput("thickThreshold", "Medium-Thick Threshold (Diameter in inches):", min = 0, max = 15, value = 5),
            checkboxInput("sortOrder", "Sort Ascending by Tree Height Index:", value=TRUE),
            #Feature 1: The code below allows the user to change the color of the bar plots. This maybe useful for individuals who are color blind or if people prefer certain color schemes
            colourInput("colorThin", "Color for Thin Trees", value = "#FF9999"),
            colourInput("colorMedium", "Color for Medium Trees", value = "#9999FF"),
            colourInput("colorThick", "Color for Thick Trees", value = "#99FF99")
    ),
  mainPanel(
      tabsetPanel(
        #Feature 2: Turn static table into Interactive table: This allows the user to sort the data according to the variable that the user likes. 
        tabPanel("Graph", plotOutput("sizeCategoryPlot")),
        tabPanel("Thick Trees", 
                 uiOutput("thickTreesTableHeading"),
                 DT::dataTableOutput("thickTreesTable"),
                 downloadButton("downloadThick", "Download Thick Trees Table")),
        
        tabPanel("Medium Trees", 
                 uiOutput("mediumTreesTableHeading"),
                 DT::dataTableOutput("mediumTreesTable"),
                 downloadButton("downloadMedium", "Download Medium Trees Table")),
        tabPanel("Thin Trees", 
                 uiOutput("thinTreesTableHeading"),
                 DT::dataTableOutput("thinTreesTable"),
                 downloadButton("downloadThin", "Download Thin Trees Table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to create a dynamic dataset based on slider values
  dynamic_data <- reactive({
    trees_data %>%
      mutate(SizeCategory = case_when(
        diameter > input$thickThreshold ~ "Thick",
        diameter >= input$thinThreshold & diameter <= input$thickThreshold ~ "Medium",
        diameter < input$thinThreshold ~ "Thin",
        TRUE ~ NA_character_
      ))
  })
  
  # Reactive expression to filter data based on selected genus
  filtered_data <- reactive({
    dynamic_data() %>% filter(genus_name == input$genus)
  })
  # Function to sort and get top 5 trees based on height
  get_sorted_data <- function(data, size_category) {
    if (input$sortOrder) {
      sorted_data <- data %>% filter(SizeCategory == size_category) %>% arrange(height_range_id)
    } else {
      sorted_data <- data %>% filter(SizeCategory == size_category) %>% arrange(desc(height_range_id))
    }
    head(sorted_data)%>% select(TreeID=tree_id, Diameter=diameter, Height_Range_ID=height_range_id)
  }
  
  
  # Bar graph of tree size categories for selected genus
  output$sizeCategoryPlot <- renderPlot({
    # Use factor to order x-axis
    plot_data <- filtered_data()
    plot_data$SizeCategory <- factor(plot_data$SizeCategory, levels = c("Thin", "Medium", "Thick"))
    ggplot(plot_data, aes(x = SizeCategory, fill=SizeCategory)) +
      geom_bar( color = "black") +
      scale_fill_manual(values = c(Thin = input$colorThin, Medium = input$colorMedium, Thick = input$colorThick)) +
      theme_minimal() +
      labs(title = paste("Size Categories of Trees:", input$genus),
           x = "Size Category", y = "Count")+
      guides(fill = guide_legend(title = "Size Category"))
  })
  # Separate tables for each size category with headings
  
  output$thickTreesTable <- DT::renderDataTable({
    get_sorted_data(filtered_data(), "Thick")
  })
  
  output$mediumTreesTable <- DT::renderDataTable({
    get_sorted_data(filtered_data(), "Medium")
  })
  
  output$thinTreesTable <- DT::renderDataTable({
    get_sorted_data(filtered_data(), "Thin")
  })

# Download handlers
  #Feature 3: The code below allows users to download the table generated as a csv. This can the be used for data collection purposes
output$downloadThick <- downloadHandler(
  filename = function() { "thick_trees.csv" },
  content = function(file) {
    write.csv(get_sorted_data(filtered_data(), "Thick"), file, row.names = FALSE)
  }
)
output$downloadMedium <- downloadHandler(
  filename = function() { "medium_trees.csv" },
  content = function(file) {
    write.csv(get_sorted_data(filtered_data(), "Medium"), file, row.names = FALSE)
  }
)
output$downloadThin <- downloadHandler(
  filename = function() { "thin_trees.csv" },
  content = function(file) {
    write.csv(get_sorted_data(filtered_data(), "Thin"), file, row.names = FALSE)
  }
)
}

# Run the application
shinyApp(ui = ui, server = server)
