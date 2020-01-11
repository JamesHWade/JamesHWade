library(tidyverse)
library(shiny)
library(shinydashboard)

rainfall <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

temperature_2 <- temperature %>% 
    filter(complete.cases(.) & temp_type == "max") %>% 
    mutate(year = as.integer(lubridate::year(date))) %>%
    group_by(city_name) %>% 
    mutate(mean_by_city = mean(temperature),
           sd_by_city = sd(temperature)) %>% 
    ungroup() %>% 
    group_by(city_name, year) %>% 
    mutate(mean_temp = mean(temperature),
           temp_score = (mean_temp - mean_by_city) / sd_by_city) %>% 
    ungroup() 

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Weather in Australia"),
    
    fluidRow(
        column(width = 4,
               sliderInput(inputId = "year1", label = "Year 1", 
                           min = min(temperature_2$year), 
                           max = max(temperature_2$year),
                           step = 1, 
                           value = 1955,
                           sep = "",
                           animate = TRUE)
        ),
        column(width = 4,
               sliderInput(inputId = "year2", label = "Year 2", 
                           min = min(temperature_2$year), 
                           max = max(temperature_2$year),
                           step = 1, 
                           value = 2017,
                           sep = "",
                           animate = TRUE),
        ),
        column(width = 4,
               selectInput(inputId = "cities", label = "City", 
                           choices =  unique(temperature_2$city_name),
                           selected = c("SYDNEY", "PORT", "KENT", "CANBERRA"), 
                           multiple = TRUE,
                           selectize = TRUE)
        ),
        column(
            width = 12,
            plotOutput("p1")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    b <- reactive({
        temperature_2 %>% 
            filter(city_name %in% input$cities &
                       (year == input$year1 | year == input$year2))
    })
    
    output$p1 <- renderPlot({
        ggplot(b()) + 
            # geom_density(data = avg, aes(x = avg_temp), fill = "grey") +  
            geom_density(aes(x = temperature, fill = temp_score, 
                             group = interaction(city_name, year)), alpha = 0.5) +
            scale_fill_viridis_c(option = "C") +
            geom_vline(aes(xintercept = mean_temp, color = factor(year)), size = 1, linetype = "dashed") +
            scale_color_viridis_d() +
            cowplot::theme_cowplot() +
            theme(axis.line.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank()) +
            facet_wrap(~city_name) +
            # transition_time(year) + ease_aes("linear") +
            labs(x = "Temperature", 
                 fill = "Temp Score",
                 color = "Mean Temp",
                 title = "Temperature Distribution by Year", 
                 subtitle = paste("Years:", input$year1, "and", input$year2),
                 caption = "#TidyTuesday | @JamesHWade")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
