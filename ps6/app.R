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
                          selectInput("videogame_platforms_plot", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                            "PC", "NES (Nintendo Entertainment System)",
                                                                            "GB (Game Boy)", "GBA (Game Boy Advance)", "DS (Dual Screen)",
                                                                            "x360 (Xbox360)", "SNES (Super Nintendo Entertainment System)",
                                                                            "3DS", "N64 (Nintendo 64)", "XB (Xbox)", "2600 (Atari 2600)",
                                                                            "PSP", "XOne", "WiiU", "GC (GameCube)", "GEN", "DC",
                                                                            "SAT", "SCD", "WS (Wonder Swan)", "NG", "TG16", "3DO", "GG",
                                                                            "PCFX"))),
                        mainPanel(
                          plotOutput("videogame_platforms_plot")
                        )
                      )
             ),
             
             tabPanel("Platform Table",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("videogame_platforms_table", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                                           "PC", "NES (Nintendo Entertainment System)",
                                                                                           "GB (Game Boy)", "GBA (Game Boy Advance)", "DS (Dual Screen)",
                                                                                           "x360 (Xbox360)", "SNES (Super Nintendo Entertainment System)",
                                                                                           "3DS", "N64 (Nintendo 64)", "XB (Xbox)", "2600 (Atari 2600)",
                                                                                           "PSP", "XOne", "WiiU", "GC (GameCube)", "GEN", "DC",
                                                                                           "SAT", "SCD", "WS (Wonder Swan)", "NG", "TG16", "3DO", "GG",
                                                                                           "PCFX"))))),
                       mainPanel(
                         plotOutput("videogame_platforms_table")
             )
                      )
  )




##Server
server <- function(input, output) {
  output$videogame_platforms_plot <- renderPlot({
    video_game %>% 
      select(Global_Sales,
             Year,
             Platform) %>% 
      filter(!is.na(Global_Sales),!is.na(Year), !is.na(Platform),
             Platform %in% input$platforms) %>%
      group_by(Platform, Year) %>% 
      summarize(global_sales_in_that_year = mean(Global_Sales)) %>% 
      ggplot(aes(x = Year, y = global_sales_in_that_year)) +
      geom_line() +
      labs(x = "Year", y = "Sales in Million")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
