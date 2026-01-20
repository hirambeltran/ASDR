
# instructions to deploy the app in GitHub
# https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html

# setwd("E:/Box Sync/Beltran-Sanchez/Book Mortality in LAC/Chapter_causes_of_death/Comparison ASDR/ASDR")


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(data.table)




library(haven)
tmp <- read_dta("SDR_comparison_jan_2026.dta") 
library(labelled)
 # Add as a new variable (the labels corresponding to each value)
tmp$Country.lab <- to_character(tmp$Country_lab)

tmp <- data.table(tmp)

new <-  names(tmp)[grep("newSDR", names(tmp))]
old <-  names(tmp)[grep("SDR_", names(tmp))]
old <- old[!old %in% new]

lnew <- melt(tmp[ , c("Country.lab", "year", "sex", new) , with = FALSE], 
                id.vars = c("Country.lab", "year", "sex"),
                measure.vars = patterns("^newSDR"),
                variable.name = "Cause",
                value.name = "age.std.rate")  |>
	         mutate(Case = "New")

lnew <- lnew[!is.na(lnew$Country.lab), ] 
lnew <- left_join(lnew, tmp[ , c("Country.lab", "year", "sex", "icd") ], by = c("Country.lab", "year", "sex") )

library(tidyverse)
lnew$Cause <- fct_recode(lnew$Cause,
  "Total"       ="newSDR_tot" ,
  "TB"          = "newSDR_tb", 
  "Malaria"     = "newSDR_malaria",
  "IPB"         = "newSDR_IPB",     
  "Oinfections" = "newSDR_Oinfections",
  "Cancer"      = "newSDR_cancer",
  "VCD"         = "newSDR_cvd",
  "Diarrhea"    = "newSDR_diarrhea",      
  "Degendis"    = "newSDR_degendis",
  "Oinfancy"    = "newSDR_Oinfancy",
  "Accidents"   = "newSDR_accidents",
  "OViol"       = "newSDR_Oviol",
  "Senil"       = "newSDR_senill",
  "Maternal"    = "newSDR_maternal",
  "Ounkn"    = "newSDR_Ounkn"
)


lold <- melt(tmp[ , c("Country.lab", "year", "sex", old) , with = FALSE], 
                id.vars = c("Country.lab", "year", "sex"),
                measure.vars = patterns("^SDR"),
                variable.name = "Cause",
                value.name = "age.std.rate") |>
	          mutate(Case = "Old")

lold$Cause <- fct_recode(lold$Cause,
  "Total"          = "SDR_tot" ,
  "TB"             = "SDR_tb", 
  "Malaria"        = "SDR_malaria",
  "IPB"            = "SDR_IPB",     
  "Oinfections"    = "SDR_Oinfections",
  "Cancer"         = "SDR_cancer",
  "VCD"            = "SDR_cvd",
  "Diarrhea"       = "SDR_diarrhea",      
  "Degendis"       = "SDR_degendis",
  "Oinfancy"       = "SDR_Oinfancy",
  "Accidents"      = "SDR_accidents",
  "OViol"          = "SDR_Oviol",
  "Senil"          = "SDR_senill",
  "Maternal"       = "SDR_maternal",
  "deathcount_WHO" = "SDR_deathcount_WHO",
  "Ounkn"          = "SDR_Ounknadj"
)

lold <- lold[!is.na(lold$Country.lab), ] 
lold <- left_join(lold, tmp[ , c("Country.lab", "year", "sex", "icd") ], by = c("Country.lab", "year", "sex") )


# putting the data together
datos <- data.table(rbind(lold, lnew))

#datos <- datos[!is.na(datos$Country.lab), ] 
datos$sex <- factor(datos$sex, levels= 1:2, labels = c("Male", "Female"))


# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Age-Standardized Death Rates"),
  
fluidRow(
    column(12,
      wellPanel(
      # Conditional Input: Only show 'C' selector when on Tab 1
      conditionalPanel(
        condition = "input.main_tabs == 'tab1'",
        selectInput("input_C", "Select Country:", 
                    choices = unique(datos$Country.lab))
      ),
      
      # Conditional Input: Only show 'Cau' selector when on Tab 2
      conditionalPanel(
        condition = "input.main_tabs == 'tab2'",
        selectInput("input_Cau", "Select Cause of death:", 
                    choices = unique(datos$Cause))
      )
     )
    )	
   ),
    
fluidRow(
    column(12,
      tabsetPanel(id = "main_tabs",
        
        # --- TAB 1 ---
        tabPanel("Females: Analysis by Country", value = "tab1",
                 h4("Females"),
                 plotOutput("plot1_F", height = "80vh", width = "100%"), # Top Panel
                 #hr(),
                 #h4("Males"),
                 #plotOutput("plot1_M", height = "500px")  # Bottom Panel
        ),
        # --- TAB 2 ---
        tabPanel("Males: Analysis by Country", value = "tab1",
                 #h4("Females"),
                 #plotOutput("plot1_F", height = "500px"), # Top Panel
                 #hr(),
                 h4("Males"),
                 plotOutput("plot1_M", height = "80vh", width = "100%")  # Bottom Panel
        ),      	      	
        
        # --- TAB 3 ---
        tabPanel("Females: Analysis by Cause of death", value = "tab2",
                 h4("Females"),
                 plotOutput("plot2_F", height = "80vh", width = "100%"), # Top Panel
                 #hr(),
                 #h4("Males"),
                 #plotOutput("plot2_M", height = "500px")  # Bottom Panel
        ),
        # --- TAB 4 ---      	
        tabPanel("Males: Analysis by Cause of death", value = "tab2",
                 #h4("Females"),
                 #plotOutput("plot2_F", height = "500px"), # Top Panel
                 #hr(),
                 h4("Males"),
                 plotOutput("plot2_M", height = "80vh", width = "100%")  # Bottom Panel
        )      	
      )
    )
  )
)

# -------------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------------
server <- function(input, output) {
  
  # === TAB 1 LOGIC ===
  
  # Plot 1 Top: C selected, Sex = F
  output$plot1_F <- renderPlot({
    req(input$input_C)
    plot_data <- datos[datos$Country.lab == input$input_C & datos$sex == "Female" & 0< datos$age.std.rate, ]
    
    # Check if data exists to avoid empty plot errors
    validate(need(nrow(plot_data) > 0, "No data available for Females in this selection."))
    
    ggplot(plot_data, 
           aes(y = age.std.rate, x = year, 
               group = Case, color = Case, shape = Case)) + 
      geom_point(size = 2) + 
      scale_shape_manual(values = c(19, 8)) +
      scale_color_manual(values = c("red", "black")) +
      xlab("Year") +  
      ylab("Age-std rate (log)") +    
      facet_wrap(~ Cause) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      theme(
      	    legend.text=element_text(size=15),legend.title=element_text(size=15), legend.justification=c(1,0),
						legend.position=c(0.1,0.8), legend.background=element_rect(fill="transparent"),
						legend.key.size = unit(1,"line"), 
      	    strip.text.x = element_text(size = 15, colour = "black", angle = 0),
      	    axis.text.x = element_text(angle = 0, vjust = 0.5,size= 13),
      	    axis.text.y = element_text(angle = 0, vjust = 0.5,size= 13))
  })
  
  # Plot 1 Bottom: C selected, Sex = M
  output$plot1_M <- renderPlot({
    req(input$input_C)
    plot_data <- datos[datos$Country.lab == input$input_C & datos$sex == "Male" & 0< datos$age.std.rate, ]
    
    validate(need(nrow(plot_data) > 0, "No data available for Males in this selection."))
    
    ggplot(plot_data, 
           aes(y = age.std.rate, x = year, 
               group = Case, color = Case, shape = Case)) + 
      geom_point(size = 2) + 
      scale_shape_manual(values = c(19, 8)) +
      scale_color_manual(values = c("red", "black")) +
      xlab("Year") +  
      ylab("Age-std rate (log)") +    
      facet_wrap(~ Cause) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      theme(
      	    legend.text=element_text(size=15),legend.title=element_text(size=10), legend.justification=c(1,0),
						legend.position=c(0.1,0.8),legend.background=element_rect(fill="transparent"),
						legend.key.size = unit(1,"line"), 
      	    strip.text.x = element_text(size = 15, colour = "black", angle = 0),
      	    axis.text.x = element_text(angle = 0, vjust = 0.5,size= 13),
      	    axis.text.y = element_text(angle = 0, vjust = 0.5,size= 13))
  })
  
  # === TAB 2 LOGIC ===
  
  # Plot 2 Top: Cau selected, Sex = F
  output$plot2_F <- renderPlot({
    req(input$input_Cau)
    plot_data <- datos[datos$Cause == input$input_Cau & datos$sex == "Female" & 0< datos$age.std.rate, ]
    
    validate(need(nrow(plot_data) > 0, "No data available for Females in this selection."))
    
    ggplot(plot_data, 
           aes(y = age.std.rate, x = year, 
               group = factor(icd), color = factor(icd), shape = Case)) + 
      geom_point(size = 2) + 
      scale_shape_manual(values = c(19, 8)) +
      # Note: Removed fixed red/black colors since you are coloring by 'icd'
      xlab("Year") +  
      ylab("Age-std rate (log)") +    
      facet_wrap(Country.lab ~ .) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      theme(
      	    legend.text=element_text(size=15),legend.title=element_text(size=10), legend.justification=c(1,0),
						legend.position=c(0.03,0.8),legend.background=element_rect(fill="transparent"),
						legend.key.size = unit(1,"line"), 
      	    strip.text.x = element_text(size = 15, colour = "black", angle = 0),
      	    axis.text.x = element_text(angle = 0, vjust = 0.5,size= 13),
      	    axis.text.y = element_text(angle = 0, vjust = 0.5,size= 13))
  })
  
  # Plot 2 Bottom: Cau selected, Sex = M
  output$plot2_M <- renderPlot({
    req(input$input_Cau)
    plot_data <- datos[datos$Cause == input$input_Cau & datos$s == "Male" & 0< datos$age.std.rate, ]
    
    validate(need(nrow(plot_data) > 0, "No data available for Males in this selection."))
    
    ggplot(plot_data, 
           aes(y = age.std.rate, x = year, 
               group = factor(icd), color = factor(icd), shape = Case)) + 
      geom_point(size = 2) + 
      scale_shape_manual(values = c(19, 8)) +
      xlab("Year") +  
      ylab("Age-std rate (log)") +    
      facet_wrap(Country.lab ~ .) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      theme(
      	    legend.text=element_text(size=15),legend.title=element_text(size=10), legend.justification=c(1,0),
						legend.position=c(0.03,0.8),legend.background=element_rect(fill="transparent"),
						legend.key.size = unit(1,"line"), 
      	    strip.text.x = element_text(size = 15, colour = "black", angle = 0),
      	    axis.text.x = element_text(angle = 0, vjust = 0.5,size= 13),
      	    axis.text.y = element_text(angle = 0, vjust = 0.5,size= 13))
  })
}

shinyApp(ui, server)

