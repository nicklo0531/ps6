library(shiny)
library(shinythemes)
library(tidyverse)
read_delim("../vgsales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Video Game Trends Overtime"),
  
  navbarPage("", 
             tabPanel("Introduction",
                      h2(p("This is the ", em("ps6"), ", which is a demo for my ", strong("final project")))),
             
             # Sidebar with a selectInput widget for number of bins 
             tabPanel("Platforms",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("platforms", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                            "PC", "NES (Nintendo Entertainment System)",
                                                                            "GB (Game Boy)", "GBA (Game Boy Advance)", "DS (Dual Screen)",
                                                                            "x360 (Xbox360)", "SNES (Super Nintendo Entertainment System)",
                                                                            "3DS", "N64 (Nintendo 64)", "XB (Xbox)", "2600 (Atari 2600)",
                                                                            "PSP", "XOne", "WiiU", "GC (GameCube)", "GEN", "DC",
                                                                            "SAT", "SCD", "WS (Wonder Swan)", "NG", "TG16", "3DO", "GG",
                                                                            "PCFX"), 
                                      multiple = TRUE)
                        ),
                        mainPanel(
                          plotOutput("videogame_platforms")
                        )
                      )
             ),
             
             tabPanel("Genres")
  )
)









# Define server logic required to draw a histogram
server <- function(input, output) {

    output$videogame_platforms <- renderPlot({
      
      input$platforms 
      
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
