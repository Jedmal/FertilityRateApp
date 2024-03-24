library(shiny)
library(readr)

my_data <- read_csv("data/my_data.csv")

ui <- fluidPage(
  titlePanel("Fertility rate visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countrySelect1", "Choose a country:", choices = unique(my_data$Country)),
      selectInput("countrySelect2", "Choose another country:", choices = unique(my_data$Country)),
      selectInput("countrySelect3", "Choose a third country:", choices = unique(my_data$Country)) # Third country selector
    ),
    mainPanel(
      uiOutput("babyImages1"), # Displays baby images for the first selected country
      uiOutput("babyImages2"), # Displays baby images for the second selected country
      uiOutput("babyImages3")  # Displays baby images for the third selected country
    )
  )
)

server <- function(input, output) {
  
  renderImages <- function(countrySelect) {
    selected_country <- countrySelect
    
    if(is.null(selected_country)){
      return()
    }
    
    fertility_rate <- my_data$`Fertility_rate`[my_data$Country == selected_country]
    
    full_babies <- floor(fertility_rate)
    partial_baby <- fertility_rate - full_babies
    
    full_baby_tags <- lapply(seq_len(full_babies), function(i) {
      tags$img(src = "baby.png", height = "100px")
    })
    
    if(partial_baby > 0) {
      partial_height <- paste0(100 * partial_baby, "px")
      partial_baby_tag <- list(tags$img(src = "baby.png", height = partial_height))
      full_baby_tags <- c(full_baby_tags, partial_baby_tag)
    }
    
    do.call(tagList, full_baby_tags)
  }
  
  output$babyImages1 <- renderUI({
    renderImages(input$countrySelect1)
  })
  
  output$babyImages2 <- renderUI({
    renderImages(input$countrySelect2)
  })
  
  # Handling the third country selection
  output$babyImages3 <- renderUI({
    renderImages(input$countrySelect3)
  })
}

# Remember to replace "path_to_your_data.csv" with the actual path to your CSV file
# Run the app
shinyApp(ui = ui, server = server)


