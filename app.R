


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(data.table)

# setwd("C:/Users/beltrans/Box/Beltran-Sanchez/Book Mortality in LAC/Chapter_causes_of_death")
# setwd("//Client/C$/Users/Hiram/Box Sync/Beltran-Sanchez/Book Mortality in LAC/Chapter_causes_of_death")
# setwd("C:/Users/Hiram/Box Sync/Beltran-Sanchez/Book Mortality in LAC/Chapter_causes_of_death")
# setwd("E:/Box Sync/Beltran-Sanchez/Book Mortality in LAC/Chapter_causes_of_death/Comparison ASDR/McKeown")
# Load your data here
load("Age_Std_Cause_mx_McKeown_list_uptoAge75PLUS.RData")
# Make sure your data is loaded before running the app

# library(rsconnect)
# 
# # the run the code below
#   rsconnect::writeManifest(
#    appDir = getwd(),
#     appFiles = c("app.R", "Age_Std_Cause_mx_McKeown_list_uptoAge75PLUS.RData"), # Add any data file names needed to run the app. These files should be uploaded in GitHub
#     appPrimaryDoc = "app.R"  # don't change the name of the file app, it has to be called "app.R"
#   )


# Define UI
ui <- fluidPage(
  titlePanel("Mortality Data Visualization"),
  
  tabsetPanel(
    # First Tab: Multiple Countries Comparison
    tabPanel("Country Comparison",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("countries_multi",
                                    "Select Countries:",
                                    choices = NULL,  # Will be populated from data
                                    selected = NULL),
                 width = 3
               ),
               mainPanel(
                 plotOutput("plot_multi_country", height = "700px"),
                 width = 9
               )
             )
    ),
    
    # Second Tab: Single Country Detail
    tabPanel("Country Detail by ICD",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country_single",
                             "Select One Country:",
                             choices = NULL,  # Will be populated from data
                             selected = NULL),
                 width = 3
               ),
               mainPanel(
                 h3("Females"),
                 plotOutput("plot_females", height = "800px"),
                 br(),
                 h3("Males"),
                 plotOutput("plot_males", height = "800px"),
                 width = 9
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load your data - REPLACE THIS WITH YOUR ACTUAL DATA
  # cd.age.std.long <- reactive({
  #   readRDS("your_data.rds")
  # })
  
  # For testing purposes, create a placeholder
  # Remove this and uncomment the code above when you have your real data
  cd.age.std.long <- reactive({
    # This is placeholder - replace with your actual data loading
    req(exists("cd.age.std.long", envir = .GlobalEnv))
    get("cd.age.std.long", envir = .GlobalEnv)
  })
  
  # Update country choices when data is loaded
  observe({
    req(cd.age.std.long())
    
    country_choices <- sort(unique(cd.age.std.long()$Country.lab))
    
    # Update multiple country selection
    updateCheckboxGroupInput(session, "countries_multi",
                              choices = country_choices,
                              selected = country_choices[1:min(3, length(country_choices))])
    
    # Update single country selection
    updateSelectInput(session, "country_single",
                      choices = country_choices,
                      selected = country_choices[1])
  })
  
  # Tab 1: Multiple Countries Plot
  output$plot_multi_country <- renderPlot({
    req(input$countries_multi)
    req(cd.age.std.long())
    
    ctries <- input$countries_multi
    
    plot_data <- cd.age.std.long() %>%
      filter(Country.lab %in% ctries & 
               (age.std.rate != 0 & !is.na(age.std.rate)))
    
    req(nrow(plot_data) > 0)
    
    ggplot(plot_data, 
           aes(y = age.std.rate, x = Year, 
               group = Country.lab, color = Country.lab)) + 
      geom_point() + 
      xlab("Year") +  
      ylab("Age-standardized death rate, log-scale") +	
      facet_grid(Sex ~ reorder(cause, cause.order), switch = "y" ) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      #theme_minimal() +
      theme(
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      labs(color = "Country")
  })
  
  # Tab 2: Females Plot
  output$plot_females <- renderPlot({
    req(input$country_single)
    req(cd.age.std.long())
    
    pais <- input$country_single
    
    datos <- cd.age.std.long() %>%
      filter(Country.lab == pais & 
               Sex == "Female" & 
               (age.std.rate != 0 & !is.na(age.std.rate)))
    
    req(nrow(datos) > 0)
    
    ggplot(datos,	
           aes(y = age.std.rate, x = Year, 
               group = as.factor(icd), colour = as.factor(icd))) + 
      ggtitle(paste0(pais, ": Females")) +
      geom_point() + 
      xlab("Year") +  
      ylab("Age-standardized death rate, log-scale") +	
      facet_wrap(~ reorder(cause, cause.order) ) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      #theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      labs(colour = "ICD")
  })
  
  # Tab 2: Males Plot
  output$plot_males <- renderPlot({
    req(input$country_single)
    req(cd.age.std.long())
    
    pais <- input$country_single
    
    datos <- cd.age.std.long() %>%
      filter(Country.lab == pais & 
               Sex == "Male" & 
               (age.std.rate != 0 & !is.na(age.std.rate)))
    
    req(nrow(datos) > 0)
    
    ggplot(datos,	
           aes(y = age.std.rate, x = Year, 
               group = as.factor(icd), colour = as.factor(icd))) + 
      ggtitle(paste0(pais, ": Males")) +
      geom_point() + 
      xlab("Year") +  
      ylab("Age-standardized death rate, log-scale") +	
      facet_wrap(~ reorder(cause, cause.order) ) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      #theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      labs(colour = "ICD")
  })
}

# Run the application
shinyApp(ui = ui, server = server)