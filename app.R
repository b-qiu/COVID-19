library("EpiEstim")
library("tidyverse")
library("ggpubr")
library("shiny")
library("scales")
library("shinyjs")
library("magrittr")
library("googlesheets4")

source("data_COVID.R")
source("EstimatingR_shiny_func.R")
source("case_sim.R")

# OW <- readRDS(file = "our_world_data.RDS")
# WO <- readRDS(file = "worldometer_data.RDS")
# JH <- readRDS(file = "john_hopkins_data.RDS")

OW <- read_sheet("1OfdJUC-1nB8cqQ0HsrCpNFC8yPDjDPUUkQFxZA6EdYY", sheet = "ow") %>%
  mutate(dates = as.Date(dates))
JH <- read_sheet("1OfdJUC-1nB8cqQ0HsrCpNFC8yPDjDPUUkQFxZA6EdYY", sheet = "jh") %>%
  mutate(dates = as.Date(dates))
WO <- read_sheet("1OfdJUC-1nB8cqQ0HsrCpNFC8yPDjDPUUkQFxZA6EdYY", sheet = "wo") %>%
  mutate(dates = as.Date(dates))

max_date <- Sys.Date() + 1

ui <- fluidPage(
  
  withMathJax(),
  useShinyjs(),
  
  titlePanel("Estimating Australia's COVID-19 instantaneous reproduction number"),
  
  helpText("Ben Qiu"),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  tabsetPanel(
    tabPanel("Actuals", fluid = TRUE,
             
             sidebarLayout(
               sidebarPanel(      
                 
                 h4(em("Inputs to estimate R")),
                 
                 selectInput("source_data",
                             label = "Select confirmed cases source data",
                             choices = list("Our World in Data",
                                            "John Hopkins University",
                                            "Worldometer"),
                             selected = "John Hopkins University"),
                 
                 numericInput("mean",
                              label = "Serial interval (mean)",
                              value = 3.96,
                              min = 1.01,
                              max = 15),
                 
                 numericInput("standard_deviation",
                              label = "Serial interval (standard deviation)",
                              value = 4.75,
                              min = 0.1,
                              max = 15),
                 
                 dateRangeInput("date_range",
                                label = "Choose date range",
                                start = "2020-03-05",
                                end = max_date,
                                min = "2020-01-06",
                                max = max_date),
                 
                 checkboxInput("y_axes",
                               label = "Set Y-axis heights",
                               value = FALSE),
                 
                 uiOutput(outputId = "yy"),
                 
                 hr(),
                 
                 helpText("Bonus:"),
                 
                 h4(em("Basic immunity threshold calculations")),
                 
                 numericInput("R_0",
                              label = "R_0: Basic reproduction number",
                              value = 5.7,
                              min = 0.1,
                              max = 15),
                 
                 numericInput("E",
                              label = "E: Vaccine effectiveness (0 - 1)",
                              value = 0.85,
                              min = 0.1,
                              max = 1),
                 
                 p("Herd immunity threshold:"),
                 
                 strong(textOutput("Herd")),
                 
                 br(),
                 
                 p("Critical vaccination coverage level:"),
                 strong(textOutput("Vacc")),
                 
                 br(),
                 
                 p("See the following ",
                   a("article",
                     href = "https://academic.oup.com/cid/article/52/7/911/299077"), 
                   " for information on immunity thresholds and vaccination coverage levels."),
                 
                 p("Also see following paper for a ",
                   a("R_0 estimate",
                     href = "https://wwwnc.cdc.gov/eid/article/26/7/20-0282_article"),
                   " for SARS-CoV-2.")
               ),
               
               mainPanel(
                 
                 br(),
                 
                 textOutput("source_selection"),
                 textOutput("si_selection"),
                 textOutput("date_selection"),
                 
                 plotOutput("map"),
                 
                 strong("Inputs:"),
                 
                 p(em("Cases:"), "new daily cases over time. Note I did not make a distinction between imported and local cases."),
                 p(em("Serial interval:"), "the period between successive cases in a chain of transmission measured in days."),
                 
                 strong("Output:"),
                 
                 p(em("R:"), "the expected number of secondary cases caused by each infected individual. Calculated using a rolling weekly window."),
                 
                 br(),
                 
                 h4("Simple explanation"),
                 
                 p("The instantaneous reproduction number (the expected number of secondary cases caused by each infected individual) is the number of newly infected cases at a given point in time, divided by the total infection potential across all infected individuals at that time. "),
                 
                 p("The total infection potential across all infected individuals is equal to the sum of the previous newly infected cases, each multiplied by the probability of a secondary case arising a certain time period after the primary case - a serial interval distribution."),
                 
                 p("The complete estimation process is listed in the Epidemics paper shown below."),
                 
                 p("Levels of R below 1 suggest interventions have been succesful. Established outbreaks will fade out if the number is maintained below 1."),
                 
                 h4("Supporting information"),
                 
                 p("R package \"EpiEstim\" was used for the above demonstration."),
                 
                 p("The package was developed by authors that recently published in ",
                   a("Epidemics:",
                     href = "https://www.sciencedirect.com/science/article/pii/S1755436519300350")),
                 
                 p("Thompson RN, Stockwin JE, van Gaalen RD, Polonsky JA, et al. Improved inference of time-varying reproduction numbers during infectious disease outbreaks. Epidemics (2019)."),
                 
                 p("Case information was taken from Our World in Data, John Hopkins University, and Worldometer."),
                 
                 p("Serial interval data for COVID-19 was obtained from the following ",
                   a("paper.",
                     href = "https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article"))
                 
               ) 
             )
    ),
    
    tabPanel("Simulation", fluid = TRUE,
             
             sidebarLayout(
               sidebarPanel(      
                 
                 h4(em("Inputs to simulate new cases")),
                 
                 selectInput("source_data_1",
                             label = "Select confirmed cases source data",
                             choices = list("Our World in Data",
                                            "John Hopkins University",
                                            "Worldometer"),
                             selected = "John Hopkins University"),
                 
                 dateRangeInput("date_range_1",
                                label = "Choose date range",
                                start = "2020-03-05",
                                end = max_date + 30,
                                min = "2020-01-06",
                                max = "2021-12-31"),
                 
                 checkboxInput("y_axes_1",
                               label = "Set Y-axis heights",
                               value = FALSE),
                 
                 uiOutput(outputId = "yy_1"),
                 
                 hr(),
                 
                 numericInput("sim_delay",
                              label = "Days till simulated new cases",
                              value = 1,
                              min = 1,
                              max = 30),
                 
                 numericInput("sim_dur",
                              label = "Duration of simulated cases",
                              value = 60,
                              min = 5,
                              max = 150),
                 
                 numericInput("sim_cases",
                              label = "Starting number of new cases",
                              value = 5,
                              min = 0,
                              max = 100),
                 
                 sliderInput("sim_g", "Starting case growth rate",
                             min = 0, max = 1,
                             value = 0.45, step = 0.05),
                 
                 sliderInput("sim_bias", "Logistic curve mid-point bias",
                             min = -30, max = 30,
                             value = -12, step = 1),
                 
                 sliderInput("sim_k_1", "Logistic curve steepness during growth phase",
                             min = 0, max = 1,
                             value = 0.4, step = 0.05),
                 
                 sliderInput("sim_k_2", "Logistic curve steepness during decay phase",
                             min = 0, max = 1,
                             value = 0.25, step = 0.05)
                 
               ),
               
               mainPanel(
                 
                 br(),
                 
                 textOutput("source_selection_1"),
                 textOutput("date_selection_1"),
                 
                 plotOutput("map_1"),
                 
                 p("This dashboard uses a simple logistic curve to generate a wave of simulated cases."),
                 
                 p("Logistic function (sigmoid curve):"),
                 
                 uiOutput("logi"),
                 
                 p("The logistic function is broken into two components at the defined \"mid-point bias\" point."),
                 
                 p("The first component represents the growth phase of the curve, and the second component represents the decay phase."),
                 
                 p("The steepeness of each phase is defined by their respective \"k\" terms."),
                 
                 p("Combined, the resultant curve defines the overall shape of the growth factor applied to the simulated cases."),
                 
                 plotOutput("map_2", height = "600px")
               ) 
             )
    )
  )
)
  
  server <- function(input, output) {
    
    output$Herd <- renderText({
      paste("1 - 1/R_0:", percent((1 - 1 / input$R_0)))
    })
    
    output$Vacc <- renderText({
      paste("(1 - 1/R_0)/E:", percent(((1 - 1 / input$R_0))/input$E))
    })
    
    output$source_selection <- renderText({
      paste("Source data:", input$source_data)
    })
    
    output$source_selection_1 <- renderText({
      paste("Source data:", input$source_data_1)
    })
    
    output$si_selection <- renderText({
      paste0("Serial interval mean (standard deviation): ", input$mean, 
             " (", input$standard_deviation, ")")
    })
    
    output$date_selection <- renderText({
      paste("Display data range:", input$date_range[1], 
            "to", input$date_range[2])
    })
    
    output$date_selection_1 <- renderText({
      paste("Display data range:", input$date_range_1[1], 
            "to", input$date_range_1[2])
    })
    
    output$yy <- renderUI({
      
      inputlist <- list()
      
      inputlist[[1]]<- numericInput("r_y",
                   label = "Y-axis height: R estimate",
                   value = NULL,
                   min = 0.1,
                   max = 15)
      
      inputlist[[2]] <- numericInput("c_y",
                   label = "Y-axis height: confirmed cases",
                   value = NULL,
                   min = 5,
                   max = 100000)
      
      tagList(inputlist)
      
      shinyjs::hidden(inputlist)

    })
    
    output$yy_1 <- renderUI({
      
      inputlist_1 <- list()
      
      inputlist_1[[1]]<- numericInput("r_y_1",
                                    label = "Y-axis height: R estimate",
                                    value = NULL,
                                    min = 0.1,
                                    max = 15)
      
      inputlist_1[[2]] <- numericInput("c_y_1",
                                     label = "Y-axis height: confirmed cases",
                                     value = NULL,
                                     min = 5,
                                     max = 100000)
      
      tagList(inputlist_1)
      
      shinyjs::hidden(inputlist_1)
      
    })
    
    observe({
      
      if(input$y_axes == T){
        
        shinyjs::show("r_y")
        shinyjs::show("c_y")
      }
      
      if(input$y_axes == F){
        
        shinyjs::hide("r_y")
        shinyjs::hide("c_y")
        
      }
      
      if(input$y_axes_1 == T){
        
        shinyjs::show("r_y_1")
        shinyjs::show("c_y_1")
      }
      
      if(input$y_axes_1 == F){
        
        shinyjs::hide("r_y_1")
        shinyjs::hide("c_y_1")
        
      }
      
    })
    
    output$logi <- renderUI({
      withMathJax(
        p("$$\\frac{-1}{1+e^{-k (x - midpoint)}} + 1$$")
      )
    })
    
    output$map <- renderPlot({
      
      source <- switch(input$source_data,
                       "Our World in Data" = OW,
                       "John Hopkins University" = JH,
                       "Worldometer" = WO)
      
      gr <- esti_r(source, 
                   input$mean, 
                   input$standard_deviation, 
                   input$date_range[1],
                   input$date_range[2],
                   input$r_y,
                   input$c_y)
      
      ggarrange(gr[[1]], gr[[2]], ncol = 1, align = "v")
      
    })
    
    output$map_1 <- renderPlot({

      source_1 <- switch(input$source_data_1,
                       "Our World in Data" = OW,
                       "John Hopkins University" = JH,
                       "Worldometer" = WO)

      sim <- case_sim(source_1,
                      input$sim_delay,
                      input$sim_dur,
                      input$sim_cases,
                      input$sim_g,
                      input$sim_k_1,
                      input$sim_k_2,
                      input$sim_bias)

      gr_1 <- esti_r(sim[[1]],
                    input$mean,
                    input$standard_deviation,
                    input$date_range_1[1],
                    input$date_range_1[2],
                    input$r_y_1,
                    input$c_y_1)

      ggarrange(gr_1[[1]], gr_1[[2]], ncol = 1, align = "v")

    })
    
    output$map_2 <- renderPlot({
    
      source_1 <- switch(input$source_data_1,
                         "Our World in Data" = OW,
                         "John Hopkins University" = JH,
                         "Worldometer" = WO)
      
      sim <- case_sim(source_1,
                      input$sim_delay,
                      input$sim_dur,
                      input$sim_cases,
                      input$sim_g,
                      input$sim_k_1,
                      input$sim_k_2,
                      input$sim_bias)
        
      sim[[2]]
      
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  
  
  