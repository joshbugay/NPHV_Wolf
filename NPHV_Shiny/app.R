#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(tidyverse)
library(deldir)
library(sf)

wolf_data <- read_csv('https://raw.githubusercontent.com/joshbugay/NPHV_Wolf/refs/heads/main/NPHVWOLFAPP/JBugay_Wolf_Video_Records.csv')
camera_locations <- read_csv('https://raw.githubusercontent.com/joshbugay/NPHV_Wolf/refs/heads/main/NPHVWOLFAPP/Camera_locations.csv')

wolf_data_timed <- wolf_data %>%
  mutate(Camera_ID = toupper(Camera_ID),
         Date = ymd(Date),
         Time = hms(Time),
         time_stamp = Date + Time,
         hr = hour(Time)
  ) %>%
  rename(wolf_count = Count) %>%
  group_by(Camera_ID) %>%
  arrange(time_stamp) %>%
  mutate(time_next_sec = as.integer(lead(time_stamp) - time_stamp),
         wolf_comp = (lead(wolf_count) - wolf_count)) %>%
  filter(time_next_sec <= 300)
joined_cam <- left_join(wolf_data_timed, camera_locations, by = 'Camera_ID') %>%
  na.omit()

month_indep_obs <- joined_cam %>%
  mutate(mon = month(time_stamp, label = TRUE),
         hr = hour(time_stamp))

temporal_var <- month_indep_obs %>%
  group_by(hr) %>%
  count() %>%
  mutate(hr_shift = if(hr < 12){
    hr = (hr + 12)
  }else{
    hr = (hr - 12)
  }
  ) %>%
  arrange((hr_shift)) %>%
  ggplot(aes(x=hr_shift, y=n)) +
  geom_col() +
  xlim(0,24)+
  theme_classic()

?geom_histogram

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NPHV Wolf Data Visualization"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 24,
                        value = 12)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- as.data.frame(joined_cam)[, 6]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
