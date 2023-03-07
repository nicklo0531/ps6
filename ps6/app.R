library(shiny)
library(shinythemes)
library(tidyverse)
video_game <- read_delim("vgsales.csv")

##UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  ##Application title
  titlePanel("Video Game Trends Overtime"),
  
  ##Navigation Page
  navbarPage("", 
             ##Introduction
             tabPanel("Introduction",
                      h2(p("This is my ", em("ps6"), ", which is a demo for my ", strong("final project."), 
                           "This data from Kaggle contains information about video games, such as names, platforms,
                           sales, etc. I am going to use the platform, sales and year columns. This data
                           contains", nrow(video_game), "rows and", ncol(video_game), "columns of data")),
                      h3("Data Sample"),
                      mainPanel(
                        tableOutput("datasample")
                      )),

             ##Drop-down menu
             tabPanel("Platform Plot",
                      sidebarLayout(
                        sidebarPanel(p("This graph shows the global sales of the assigned platform under the years when there were releases. This graph provides information about the trend and popularity of the platform.
          Users are able to explore the platform as they wish!"),
                                     selectInput("videogame_platforms", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                                                 "PC", "NES", "GB", "GBA", "DS", "X360", "SNES",
                                                                                                 "3DS", "N64", "XB", "2600", "GEN", "DC",
                                                                                                 "PSP", "XOne", "WiiU", "GC", "SAT", "SCD", "WS",
                                                                                                 "NG", "TG16", "3DO", "GG", "PCFX")),
                                     radioButtons("color", "Select the Color of the Bins", choices = c("Green", "Red", "Blue"))
                                     ),
                        mainPanel(
                          plotOutput("videogame_platforms_plot"),
                          textOutput("platform_plot_text")
                        )
                      )
             ),
             
             tabPanel("Platform Table",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("videogame_platforms_table", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                                            "PC", "NES", "GB", "GBA", "DS", "x360", "SNES",
                                                                                            "3DS", "N64", "XB", "2600", "GEN", "DC",
                                                                                            "PSP", "XOne", "WiiU", "GC", "SAT", "SCD", "WS",
                                                                                            "NG", "TG16", "3DO", "GG", "PCFX"))),
                        mainPanel(
                          tableOutput("videogame_platforms_table"),
                          textOutput("platform_table_text")
                        )
                      )
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
      filter(Platform %in% input$videogame_platforms,
             Global_Sales != "N/A", Year != "N/A", Platform != "N/A") %>%
      group_by(Year) %>%
      summarize(global_sales_in_that_year = mean(Global_Sales)) %>% 
      ggplot(aes(x = Year, y = global_sales_in_that_year)) +
      geom_col(fill = input$color) +
      labs(x = "Year", y = "Sales in Million")
  })
  output$platform_plot_text <- renderText({
    paste("You are currently viewing the graph trend of", input$videogame_platforms)
  })
  output$platform_table_text <- renderText({
    paste("You are currently viewing the table trend of", input$videogame_platforms_table)
  })
  output$videogame_platforms_table <- renderTable({
    video_game %>% 
      select(Global_Sales,
             Year,
             Platform) %>% 
      filter(Platform %in% input$videogame_platforms_table,
             Global_Sales != "N/A", Year != "N/A", Platform != "N/A") %>%
      group_by(Year) %>% 
      summarize(global_sales_in_that_year_in_millions = mean(Global_Sales))
  })
  output$datasample <- renderTable({
    video_game %>% 
      select(Name, Platform, Year, Global_Sales) %>%
      sample_n(10)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)