library("EpiEstim")
library("tidyverse")
library("ggpubr")
library("shiny")
library("scales")

source("EstimatingR_shiny_func.R")
source("case_sim.R")

OW <- readRDS(file = "our_world_data.RDS")
WO <- readRDS(file = "worldometer_data.RDS")
JH <- readRDS(file = "john_hopkins_data.RDS")

max_date <- Sys.Date() + 1

ui <- fluidPage(
  
  titlePanel("Estimating Australia's COVID-19 instantaneous reproduction number"),
  
  helpText("Built by Ben Qiu"),
  
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
                                            "Worldometer",
                                            "John Hopkins University"),
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
                 
                 numericInput("r_y",
                              label = "Y-axis height: R estimate",
                              value = NULL,
                              min = 0.1,
                              max = 15),
                 
                 numericInput("c_y",
                              label = "Y-axis height: confirmed cases",
                              value = NULL,
                              min = 5,
                              max = 100000),
                 
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
                 
                 h4("Simple explanation"),
                 
                 strong("Inputs:"),
                 
                 p(em("Cases:"), "new cases over time measured in days. Note I did not make a distinction between imported and local cases."),
                 p(em("Serial interval:"), "the period between successive cases in a chain of transmission measured in days."),
                 
                 strong("Output:"),
                 
                 p(em("R:"), "the expected number of secondary cases caused by each infected individual. Calculated using a rolling weekly window."),
                 
                 p("The instantaneous reproduction number (the expected number of secondary cases caused by each infected individual) is the number of newly infected cases at a given point in time, divided by the total infection potential across all infected individuals at that time. "),
                 
                 p("The total infection potential across all infected individuals is equal to the sum of the previous newly infected cases, each multiplied by the probability of a secondary case arising a certain time period after the primary case - a serial interval distribution."),
                 
                 p("The complete estimation process is listed in the Epidemics paper shown below."),
                 
                 p("Levels of R below 1 suggest interventions have been succesful. Established outbreaks will fade out if the number is maintained below 1."),
                 
                 br(),
                 
                 h4("Supporting information"),
                 
                 p("R package \"EpiEstim\" was used for the above demonstration."),
                 
                 p("The package was developed by authors that recently published in ",
                   a("Epidemics:",
                     href = "https://www.sciencedirect.com/science/article/pii/S1755436519300350")),
                 
                 p("Thompson RN, Stockwin JE, van Gaalen RD, Polonsky JA, et al. Improved inference of time-varying reproduction numbers during infectious disease outbreaks. Epidemics (2019)."),
                 
                 p("Case information was taken from Our World in Data, Worldometer, and John Hopkins University (note earlier data is missing from the Worldometer source)."),
                 
                 p("Serial interval data for COVID-19 was obtained from the following ",
                   a("paper.",
                     href = "https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article"))
                 
               ) 
             )
    ),
    
    tabPanel("Simulation (under construction)", fluid = TRUE,
             
             sidebarLayout(
               sidebarPanel(      
                 
                 h4(em("Inputs to simulate new cases")),
                 
                 selectInput("source_data_1",
                             label = "Select confirmed cases source data",
                             choices = list("Our World in Data",
                                            "Worldometer",
                                            "John Hopkins University"),
                             selected = "John Hopkins University"),
                 
                 dateRangeInput("date_range_1",
                                label = "Choose date range",
                                start = "2020-03-05",
                                end = "2020-06-15",
                                min = "2020-01-06",
                                max = "2020-12-31"),
                 
                 numericInput("r_y_1",
                              label = "Y-axis height: R estimate",
                              value = NULL,
                              min = 0.1,
                              max = 15),
                 
                 numericInput("c_y_1",
                              label = "Y-axis height: confirmed cases",
                              value = NULL,
                              min = 5,
                              max = 100000),
                 
                 numericInput("sim_delay",
                              label = "Days till simulated new cases",
                              value = 5,
                              min = 2,
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
                 
                 sliderInput("sim_k_1", "Logistic curve steepness during growth phase",
                             min = 0, max = 1,
                             value = 0.4, step = 0.05),
                 
                 sliderInput("sim_k_2", "Logistic curve steepness during decay phase",
                             min = 0, max = 1,
                             value = 0.25, step = 0.05),
                 
                 sliderInput("sim_bias", "Logistic curve mid-point bias",
                             min = -30, max = 30,
                             value = -12, step = 1)
                 
               ),
               
               mainPanel(
                 
                 br(),
                 
                 textOutput("source_selection_1"),
                 textOutput("date_selection_1"),
                 
                 plotOutput("map_1"),
                 
                 p("Logistic function (sigmoid curve):"),
                 
                 p(" -1 / (1 + exp( -k * (x - midpoint))) + 1"),
                 
                 p("k is the steepness of the growth and decay curve, which define the curves located on either side of the chosen mid-point.")
                 
                 
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
    
    output$map <- renderPlot({
      
      source <- switch(input$source_data,
                       "Our World in Data" = OW,
                       "Worldometer" = WO,
                       "John Hopkins University" = JH)
      
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
                       "Worldometer" = WO,
                       "John Hopkins University" = JH)

      sim <- case_sim(source_1,
                      input$sim_delay,
                      input$sim_dur,
                      input$sim_cases,
                      input$sim_g,
                      input$sim_k_1,
                      input$sim_k_2,
                      input$sim_bias)

      gr_1 <- esti_r(sim,
                    input$mean,
                    input$standard_deviation,
                    input$date_range_1[1],
                    input$date_range_1[2],
                    input$r_y_1,
                    input$c_y_1)

       ggarrange(gr_1[[1]], gr_1[[2]], ncol = 1, align = "v")

    })
    
  }
  
  shinyApp(ui = ui, server = server)
  
  
  