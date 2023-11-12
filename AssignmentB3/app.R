# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(datateachr)

# Load your dataset
trees_data <- vancouver_trees

# Add a category based on diameter
trees_data <- trees_data %>%
  mutate(SizeCategory = case_when(
    diameter > 10 ~ "Thick",
    diameter >= 5 & diameter <= 10 ~ "Medium",
    diameter < 5 ~ "Thin",
    TRUE ~ NA_character_
  ))

# Define UI
ui <- fluidPage(
  titlePanel("Tree Diameter Categories by Genus"),
  sidebarLayout(
    sidebarPanel(
      selectInput("genus", "Choose a Genus:", choices = unique(trees_data$genus_name))
    ),
    mainPanel(
      plotOutput("sizeCategoryPlot"),
      tableOutput("thickTreesTable"),
      tableOutput("mediumTreesTable"),
      tableOutput("thinTreesTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on selected genus
  filtered_data <- reactive({
    trees_data %>% filter(genus_name == input$genus)
  })
  
  # Bar graph of tree size categories for selected genus
  output$sizeCategoryPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = SizeCategory)) +
      geom_bar(fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = paste("Size Categories of Trees:", input$genus),
           x = "Size Category", y = "Count")
  })
  
  # Separate tables for each size category
  output$thickTreesTable <- renderTable({
    filtered_data() %>% filter(SizeCategory == "Thick") %>% 
      slice_head(n = 5) 
  })
  
  output$mediumTreesTable <- renderTable({
    filtered_data() %>% filter(SizeCategory == "Medium") %>% 
      slice_head(n = 5)  
  })
  
  output$thinTreesTable <- renderTable({
    filtered_data() %>% filter(SizeCategory == "Thin") %>% 
      slice_head(n = 5)  
  })
}

# Run the application
shinyApp(ui = ui, server = server)
