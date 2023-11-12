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
      tabsetPanel(
        tabPanel("Graph", 
                 fluidRow(
                   column(8, plotOutput("sizeCategoryPlot")),
                   column(4, img(src = "tree.png", height = "300px", width = "auto"))
                 )),
        tabPanel("Thick Trees", 
                 uiOutput("thickTreesTableHeading"),
                 tableOutput("thickTreesTable")),
        tabPanel("Medium Trees", 
                 uiOutput("mediumTreesTableHeading"),
                 tableOutput("mediumTreesTable")),
        tabPanel("Thin Trees", 
                 uiOutput("thinTreesTableHeading"),
                 tableOutput("thinTreesTable"))
      )
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
    ggplot(filtered_data(), aes(x = SizeCategory, fill=SizeCategory)) +
      geom_bar( color = "black") +
      theme_minimal() +
      labs(title = paste("Size Categories of Trees:", input$genus),
           x = "Size Category", y = "Count")+
      guides(fill = guide_legend(title = "Size Category"))
  })
  
  # Separate tables for each size category
  table_heading <- function(size_category) {
    paste(size_category, " :5 trees subset for", input$genus)
  }
  
  # Separate tables for each size category with headings
  output$thickTreesTableHeading <- renderUI({
    h4(table_heading("Thick"))
  })
  output$thickTreesTable <- renderTable({
    filtered_data() %>% filter(SizeCategory == "Thick") %>% 
      slice_head(n = 5) 
  })
  output$mediumTreesTableHeading <- renderUI({
    h4(table_heading("Medium"))
  })
  
  output$mediumTreesTable <- renderTable({
    filtered_data() %>% filter(SizeCategory == "Medium") %>% 
      slice_head(n = 5)  
  })
  output$thinTreesTableHeading <- renderUI({
    h4(table_heading("Thin"))
  })
  output$thinTreesTable <- renderTable({
    filtered_data() %>% filter(SizeCategory == "Thin") %>% 
      slice_head(n = 5)  
  })
}

# Run the application
shinyApp(ui = ui, server = server)
