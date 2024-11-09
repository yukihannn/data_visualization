# Final Project

# load packages
library(shiny)
library(dplyr)
library(psych)
library(tidyverse)
library(janitor)
library(readxl)
library(rsconnect)

# load data
timing_df <- read_xlsx("data/timing data.xlsx", sheet = "all_data") %>%
  clean_names()

timing_training_sspa <- read_xlsx("data/timing data.xlsx", sheet = "sspa_train") %>%
  clean_names()
timing_training_speed <- read_xlsx("data/timing data.xlsx", sheet = "speed_train") %>%
  clean_names()

timing_interfere_sspa <- read_xlsx("data/timing data.xlsx", sheet = "sspa_interference") %>%
  clean_names()

timing_interfere_speed <- read_xlsx("data/timing data.xlsx", sheet = "speed_interference") %>%
  clean_names()

timing_transfer_sspa <- read_xlsx("data/timing data.xlsx", sheet = "sspa transfer") %>%
  clean_names()

timing_transfer_speed <- read_xlsx("data/timing data.xlsx", sheet = "speed transfer") %>%
  clean_names()

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Serial Interception Sequence Learning"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput("var1",
        label = "Choose an effect to display",
        choices = list(
          "Transferability",
          "Interference",
          "Training"
        ),
        selected = "Transferability"
      ),
      selectInput("var2",
        label = "Choose a variable to display",
        choices = list(
          "Accuracy",
          "Speed"
        ),
        selected = "Accuracy"
      ),
      h4("SISL"),
      img(src = "sisl.png", height = 140, width = 240),
      br(),
      p("SISL (Serial Interception Sequence Learning) is a task that is similar to Guitar Hero."),
      p("Participants make precisely timed responses to cues following a covert 12-item repeating sequence 
        that had embedded rhythmic timing patterns: Strong, Weak, or Isochronous."),
      p("Try it out yourself:",
        a("SISL task", 
          href = "https://www.reberlab.org/file/show/SISL?group=667b435bdb923a53")),
      h4("Effects"),
      p(strong("Transferability"), 
        "was the effect that learned knowledge from one context partially carries to an novel one and allows faster learning"),
      p(strong("Interference"), 
        "was the order effect that testing in a novel context before the trained one disrupts konwledge expression"),
      h4("Measurements"),
      p(strong("Accuracy"), "was measured as the trained sequence accuracy minus novel sequences accuracy"),
      p(strong("Speed"), "was measured as the velocity of cues' movement")
    ),
    mainPanel(
      plotOutput("SISL")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$SISL <- renderPlot({
    if (input$var1 == "Interference") {
      if (input$var2 == "Accuracy") {
        data <- timing_interfere_sspa
      } else {
        data <- timing_interfere_speed
      }
    } else if (input$var1 == "Transferability") {
      if (input$var2 == "Accuracy") {
        data <- timing_transfer_sspa
      } else {
        data <- timing_transfer_speed
      }
    } else {
      if (input$var2 == "Accuracy") {
        data <- timing_training_sspa
      } else {
        data <- timing_training_speed
      }
    }

    x <- switch(input$var1,
      "Interference" = "order",
      "Transferability" = "transfer",
      "Training" = "training"
    )

    y <- switch(input$var2,
      "Accuracy" = "sspa",
      "Speed" = "speed",
    )

    label_y <- switch(input$var2,
      "Accuracy" = "sequence specific cue accuracy",
      "Speed" = "cue velocity",
    )

    training_names <- list(
      "iso" = "Isochrnous",
      "weak" = "Weak",
      "strong" = "Strong"
    )

    label_x <- switch(input$var1,
      "Interference" = c("first", "second"),
      "Transferability" = c("transfer", "trained"),
      "Training" = training_names
    )
    
    scale <- switch(input$var2,
                    "Accuracy" = "%",
                    "Speed" = "s",
    )

    variable_labeller <- function(variable, value) {
      return(training_names[value])
    }

    ggplot(
      data,
      aes_string(x, y, fill = x)
    ) +
      geom_boxplot() +
      theme_classic() +
      labs(
        x = str(x),
        y = label_y
      ) +
      scale_x_discrete(
        labels = label_x
      ) +
      scale_fill_discrete(
        name = input$var1,
        labels = label_x
        ) +
      scale_y_continuous(
          labels = scales::label_number(
            accuracy = 1,
            suffix = scale
          )
      ) +
      theme(
        legend.justification = c(1, 0.5)
        ) +
      facet_grid(
        ~training,
        scales = "free_x",
        space = "free",
        labeller = variable_labeller
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# rsconnect::deployApp('/Users/hanziyan/Desktop/STAT302/shiny_labs/Han_Yuki_Final/final_app')
