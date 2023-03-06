library(shiny)
library(shinythemes)
library(tidyverse)
video_game <- read_delim("../vgsales.csv")

##UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  ##Application title
  titlePanel("Video Game Trends Overtime"),
  
  ##Navigation Page
  navbarPage("", 
             ##Introduction
             tabPanel("Introduction",
                      h2(p("This is the ", em("ps6"), ", which is a demo for my ", strong("final project."), 
                           "This data is from Kaggle contains information about video games, such as names, platforms,
                           sales, etc. I am going to use the platform, sales and year columns. Here is a small sample
                           of the data:"))),
             
             ##Drop-down menu
             tabPanel("Platform Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("platforms", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                            "PC", "NES (Nintendo Entertainment System)",
                                                                            "GB (Game Boy)", "GBA (Game Boy Advance)", "DS (Dual Screen)",
                                                                            "x360 (Xbox360)", "SNES (Super Nintendo Entertainment System)",
                                                                            "3DS", "N64 (Nintendo 64)", "XB (Xbox)", "2600 (Atari 2600)",
                                                                            "PSP", "XOne", "WiiU", "GC (GameCube)", "GEN", "DC",
                                                                            "SAT", "SCD", "WS (Wonder Swan)", "NG", "TG16", "3DO", "GG",
                                                                            "PCFX"), multiple = TRUE)),
                        mainPanel(
                          plotOutput("videogame_platforms")
                        )
                      )
             ),
             
             tabPanel("Platform Table")
  )
)



##Server
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
