library(shiny)

ui <- fluidPage(
  titlePanel("Data Wrangling App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("calc_method", "Select Calculation Method",
                  choices = c("Addition", "Multiplication"),
                  selected = "Addition"),
      selectInput("type", "Select Type",
                  choices = c("positive", "negative", "depressive", "total"),
                  selected = "positive"),
      conditionalPanel(
        condition = "input.type == 'positive'",
        p("Range: 0 - 160")
      ),
      conditionalPanel(
        condition = "input.type == 'negative'",
        p("Range: 0 - 112")
      ),
      conditionalPanel(
        condition = "input.type == 'depressive'",
        p("Range: 0 - 48")
      ),
      conditionalPanel(
        condition = "input.type == 'total'",
        p("Range: 0 - 320")
      ),
      selectInput("split_type", "Choose Split Type", choices = c("Number", "Category")),
      conditionalPanel(
        condition = "input.split_type == 'Number'",
        numericInput("split_value", "Enter Split Value", value = 0)
      ),
      conditionalPanel(
        condition = "input.split_type == 'Category'",
        selectInput("split_method", "Choose Split Method", choices = c("Median", "Mean", "Quantile")),
        conditionalPanel(
          condition = "input.split_method == 'Quantile'",
          numericInput("quantile_value", "Enter Quantile Value (0-1)",
                       value = 0.5, min = 0, max = 1, step = 0.01),
          radioButtons("quantile_option", "Select Quantile Option",
                       choices = c("Selected quantile and the rest", 
                                   "Top and bottom quantiles"),
                       selected = "Selected quantile and the rest")
        )
        ),
      actionButton("apply_split", "Apply Split")
    ),
    mainPanel(
      h3("Score distribution of CAPE"),
      plotOutput("score_plot"),
      h3("repeat vs. non-repeat absolute precision (12-trial block)"),
      plotOutput("reps_fig5_plot"),
      h3("non-repeat absolute precision distribution (12-trial block)"),
      plotOutput("reps_fig5_plot2"),
      h3("non-repeat absolute precision regression with score (12-trial block)"),
      plotOutput("reps_fig5_plot3"),
      h3("repeat absolute precision distribution (12-trial block)"),
      plotOutput("reps_fig5_plot4"),
      h3("repeat absolute precision regression with score (12-trial block)"),
      plotOutput("reps_fig5_plot5"),
      h3("sequence only precision across blocks (12-trial block)"),
      plotOutput("reps_fig5_plot6"),
      h3("sequence only precision across blocks (60-trial block)"),
      plotOutput("reps_fig5_plot7"),
      h3("precision regression with score (60-trial block)"),
      plotOutput("reps_fig5_plot8"),
      h3("number of repeat times and sequence precision (12-trial block)"),
      plotOutput("reps_fig5_plot9")
    )
  )
)

library(shiny)
library(dplyr)
library(psych)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(readxl)
library(lsr)
library(wesanderson)
library(ggsignif)
library(car)
library(ez)
library(lmerTest)
library(lme4)
library(janitor)
library(ggbreak)
library(ggpubr)

exclude_id <- c("5e6a9510e644f7045cb0b536","610ec31b33de16c5167f3062","63e51e528df11f83a1aacab8","606f3058977e247974d43fc2","65c630b2a2cc61ba0cfc934c", "664da2e40e44a9895e2a5c1e", "6091448b2ddfafebfc8b83b3", "6400d9316da986445e56d8e0", "5d30cabd5124970019cb706c", "660ce8118af7c468fd0f996b", "62e185484154c451882a8a3d", "5e6a9510e644f7045cb0b536")

server <- function(input, output, session) {
  # Load and preprocess data
  data <- read_csv("scored_cape.csv") %>%
    clean_names() %>%
    mutate(score = as.numeric(score),
           score_prod = as.numeric(score_prod)) %>%
    drop_na(score)
  
  data_all_dimension <- data %>%
      group_by(prolific_id, type) %>%
      summarize(score_sum = sum(score),
                score_prod = prod(score_prod))
  
  # data_all_dimension <- reactive({
  #   req(data)
  #   data_all_dimension <- if (input$calc_method == "Addition") {
  #     data %>%
  #       group_by(prolific_id, type) %>%
  #       summarize(score = sum(score, na.rm = TRUE))
  #   } else {
  #     data %>%
  #       group_by(prolific_id, type) %>%
  #       summarize(score = prod(score, na.rm = TRUE))
  #   }
  # })

  data_total <- data %>%
    group_by(prolific_id) %>%
    summarize(score_sum = sum(score),
              score_prod = prod(score))
  
  data_total$type <- "total"
  
  data_all_dimension <-rbind(data_all_dimension, data_total)
  
  data_all_dimension <- data_all_dimension %>% 
    rename(subject_id = prolific_id)
  
  output$quantile_input <- renderUI({
    if (input$split_method == "Quantile") {
      numericInput("quantile_value", "Enter Quantile (0-1)", value = 0.5, min = 0, max = 1)
    }
  })

    # Reactive expression for split data
    split_data <- eventReactive(input$apply_split, {
      req(data_all_dimension)
      
      filtered_data <- 
        if (input$calc_method == "Addition") {
          data_all_dimension[,c("subject_id", "type", "score_sum")]
        }
      else {data_all_dimension[,c("subject_id", "type", "score_prod")]
      }
      
    # print(head(filtered_data))
    filtered_data <- setNames(filtered_data, c("subject_id", "type", "score"))
    # print(tail(filtered_data))
    
    # Filter the data based on selected type
    filtered_data <- filtered_data %>%
      filter(type == input$type)
    
    if (input$split_type == "Number") {
      control <- filtered_data %>% filter(score <= input$split_value)
      high <- filtered_data %>% filter(score > input$split_value)
    } else if (input$split_type == "Category") {
      
      if (input$split_method != "Quantile") {
      split_value <- switch(input$split_method,
                            "Median" = median(filtered_data$score, na.rm = TRUE),
                            "Mean" = mean(filtered_data$score, na.rm = TRUE)
                            # "Quantile" = quantile(filtered_data$score, probs = input$quantile_value, na.rm = TRUE)
                            )
      print(split_value)
      control <- filtered_data %>% filter(score <= split_value)
      high <- filtered_data %>% filter(score > split_value) }
      
      else if (input$split_method == "Quantile") {
        
        if (input$quantile_option == "Selected quantile and the rest") {
        split_value <-quantile(filtered_data$score, probs = input$quantile_value, na.rm = TRUE)
        
        control <- filtered_data %>% filter(score <= split_value)
        high <- filtered_data %>% filter(score > split_value)
        
      } else if (input$quantile_option == "Top and bottom quantiles") {
        split_value <-quantile(filtered_data$score, probs = input$quantile_value, na.rm = TRUE)
        
        top_quantile <- quantile(filtered_data$score, probs = input$quantile_value, na.rm = TRUE)
        bottom_quantile <- quantile(filtered_data$score, probs = 1 - input$quantile_value, na.rm = TRUE)
        
        control <- filtered_data %>% filter(score <= bottom_quantile)
        high <- filtered_data %>% filter(score >= top_quantile)
      }
      }
    }
    
    control <- mutate(control, group = "control")
    high <- mutate(high, group = "high")

    
    cape_score <-bind_rows(control, high)
    cape_score
    })
  
  data_rep_order <- reactive({
    req(split_data())
    data_rep_order_raw <- read.csv('reps_wrangled_order_24spr_12.csv') %>%
      clean_names() %>%
      as.data.frame() %>%
      drop_na() %>%
      filter(!subject_id %in% exclude_id)
  
  
  
    data_60 <- read.csv('reps_wrangled_order_24spr_60.csv')%>%
      clean_names()%>%
      as.data.frame()%>%
      drop_na()%>% 
      filter(!subject_id %in% exclude_id)
    
    # Merge with cape_scored from split_data()
    merged_data <- merge(data_rep_order_raw, split_data(), by = "subject_id")
    
    print(length(unique(merged_data$subject_id[merged_data$group == 'control'])))
    print(length(unique(merged_data$subject_id[merged_data$group == 'high'])))
    
    merged_data_60 <- merge(data_60, split_data(), by = "subject_id")
    
    list(
      merged_data = merged_data,
      merged_data_60 = merged_data_60
    )
  })
  
  # reactive expression for reps.fig5
  reps.fig5 <- reactive({
    req(data_rep_order())
    
    data_rep_5 <- data_rep_order()$merged_data %>%
      filter(rep_num1 < 5) %>%
      mutate(condition = ifelse(block < 81, 'train', 'test')) %>%
      group_by(subject_id, rep1, seqfoil1, condition, group) %>%
      summarize(
        pixl_dist1 = mean(pixl_dist1, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      group_by(rep1, seqfoil1, condition, group) %>%
      summarize(
        n = n(),
        mean = mean(pixl_dist1),
        se = sd(pixl_dist1) / sqrt(n)
      )
    
    data_rep_train <- data_rep_order()$merged_data %>%
      mutate(condition = ifelse(block < 81, 'train', 'test')) %>%
      filter(block < 41) %>%
      group_by(subject_id, seqfoil1, group, block) %>%
      summarize(
        pixl_dist1 = mean(pixl_dist1, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      group_by(seqfoil1, group, block) %>%
      summarize(
        n = n(),
        mean = mean(pixl_dist1),
        se = sd(pixl_dist1) / sqrt(n)
      )
    
    data_rep_train_regression <- data_rep_order()$merged_data %>%
      mutate(condition = ifelse(block < 81, 'train', 'test')) %>%
      filter(block < 41) %>%
      group_by(subject_id, rep1, seqfoil1, group) %>%
      summarize(
        n = n(),
        mean = mean(pixl_dist1),
        score = mean(score),
        se = sd(pixl_dist1) / sqrt(n)
      )
    
    data_rep_train_60 <- data_rep_order()$merged_data_60 %>%
      mutate(condition = ifelse(block < 17, 'train', 'test')) %>%
      filter(block < 17) %>%
      group_by(subject_id, seqfoil1, group, block) %>%
      summarize(
        pixl_dist1 = mean(pixl_dist1, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      group_by(seqfoil1, group, block) %>%
      summarize(
        n = n(),
        mean = mean(pixl_dist1),
        se = sd(pixl_dist1) / sqrt(n)
      )
    
    data_rep_train_60_regression <- data_rep_order()$merged_data_60 %>%
      mutate(condition = ifelse(block < 17, 'train', 'test')) %>%
      filter(block < 17) %>%
      group_by(subject_id, group, seqfoil1) %>%
      summarize(
        pixl_dist1 = mean(pixl_dist1),
        score = mean(score)) 
    
    data_rep5_subset <- data_rep_order()$merged_data %>%
      filter(rep_num1 < 10) %>%
      mutate(condition = ifelse(block < 81, 'train', 'test')) %>%
      filter(condition == "train") %>%
      filter(seqfoil1 == " seq") %>%
      group_by(subject_id, rep_num1, group) %>%
      summarize(
        pixl_dist1 = mean(pixl_dist1, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      group_by(rep_num1, group) %>%
      summarize(
        n = n(),
        mean = mean(pixl_dist1),
        se = sd(pixl_dist1) / sqrt(n)
      )
    
    data_rep_5$data <- factor(data_rep_5$seqfoil1, levels = c(' seq', ' foil'))
    data_rep_5$condition <- factor(data_rep_5$condition, levels = c('train', 'test'))
    data_rep_train$data <- factor(data_rep_train$seqfoil1, levels = c(' seq', ' foil'))
    data_rep_train_regression$data <- factor(data_rep_train_regression$seqfoil1, levels = c(' seq', ' foil'))
    
    list(
      data_rep_5_fig = data_rep_5,
      data_rep_train_fig = data_rep_train,
      data_rep_train_regression_fig = data_rep_train_regression,
      data_rep_train_60_fig = data_rep_train_60,
      data_rep_train_60_regression_fig = data_rep_train_60_regression,
      data_rep5_subset_fig = data_rep5_subset
    )
  })
  
  # Render the distribution plot
  output$score_plot <- renderPlot({
    req(split_data())
    ggplot(split_data(), aes(x = score, fill = group)) +
      geom_histogram(position = "dodge") +
      labs(title = "Score Distribution", x = "Score", y = "Count") +
      theme_minimal() +
      scale_fill_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen'))
  })
  
  # plot 2: repeat vs. non-repeat precision 12-trial block
  output$reps_fig5_plot <- renderPlot({
    req(reps.fig5()$data_rep_5_fig)
    ggplot(subset(reps.fig5()$data_rep_5_fig, reps.fig5()$data_rep_5_fig$condition == "train"), 
           aes(x = seqfoil1, y = mean, fill = group)) +
      geom_col(position = position_dodge2(preserve = 'single'), width = 0.7) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), show.legend = FALSE, size = 0.5, width = 0.6,
                    position = position_dodge2(padding = 0.5, preserve = 'single')) +
      ggbreak::scale_y_break(c(0.1, 30), scales = 20) +
      scale_fill_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      labs(x = "Seqfoil", y = "Pixel Distance from Target") +
      theme_classic() +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top",
            text = element_text(size = 14),
            axis.line = element_line(colour = 'black'),
            axis.text = element_text(size = 15)) +
      expand_limits(y = c(0, 55)) +
      scale_y_continuous(breaks = seq(0, 55, 5)) +
      facet_grid(~ rep1)
  })
  
  # plot 3: non-repeat precision distribution 12-trial block
  output$reps_fig5_plot2 <- renderPlot({
    req(data_rep_order()$merged_data)
    ggplot()+
      geom_density(data = subset(data_rep_order()$merged_data,data_rep_order()$merged_data$rep1 == ' False'), 
                   aes(x = pixl_dist1, group = group, color = group), alpha = 0.7) +
      theme_classic()+
      guides(alpha = FALSE, fill = FALSE) +
      labs(x = NULL,
           y = "Density")+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top",
            text = element_text(size = 12),
            axis.line = element_line(colour = 'black', linewidth = 1)) +
      scale_fill_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen'))+
      ylim(0, 0.05) +facet_grid(~seqfoil1)
  })   
  
  # plot 4: non-repeat precision regression with score 12-trial block
  output$reps_fig5_plot3 <- renderPlot({
    req(reps.fig5()$data_rep_train_regression_fig)
    ggplot(subset(reps.fig5()$data_rep_train_regression_fig, reps.fig5()$data_rep_train_regression_fig$rep1 == " False"), 
           aes(y = mean, x = score, fill = group, color = group)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      theme_classic() +
      ggpubr::stat_cor(method = 'spearman')+
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 14),
        axis.line = element_line(colour = 'black'),
        axis.text = element_text(size = 15)
      ) +
      xlab('Score') +
      ylab('Mean pixel distance from target') +
      facet_grid(~seqfoil1)
  }) 

  # plot 5: repeat precision distribution 12-trial block
  output$reps_fig5_plot4 <- renderPlot({
    req(reps.fig5())
    ggplot()+
      geom_density(data = subset(data_rep_order()$merged_data,data_rep_order()$merged_data$rep1 == ' True'), 
                   aes(x = pixl_dist1, group = group, color = group), alpha = 0.7) +
      theme_classic()+
      guides(alpha = FALSE, fill = FALSE) +
      labs(x = NULL,
           y = "Density")+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top",
            text = element_text(size = 12),
            axis.line = element_line(colour = 'black', linewidth = 1)) +
      scale_fill_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen'))+
      ylim(0, 0.05) +facet_grid(~seqfoil1)
  })
  
  # plot 6: non-repeat precision regression with score 12-trial block
  output$reps_fig5_plot5 <- renderPlot({
    req(reps.fig5()$data_rep_train_regression_fig)
    ggplot(subset(reps.fig5()$data_rep_train_regression_fig, reps.fig5()$data_rep_train_regression_fig$rep1 == " True"), aes(y = mean, x = score, fill = group, color = group)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      theme_classic() +
      ggpubr::stat_cor(method = 'spearman')+
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 14),
        axis.line = element_line(colour = 'black'),
        axis.text = element_text(size = 15)
      ) +
      xlab('Score') +
      ylab('Mean pixel distance from target') +
      facet_grid(~seqfoil1)
  }) 
  
  # plot 7: sequence only absolute precision 12-trial block
  output$reps_fig5_plot6 <- renderPlot({
    req(reps.fig5())
    ggplot(subset(reps.fig5()$data_rep_train_fig, reps.fig5()$data_rep_train_fig$seqfoil1 == " seq"), aes(x = block, y = mean, fill = group, color=group)) +
      geom_line() +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se),show.legend=F,size=0.5,width=.1) + 
      # scale_y_break(c(0.1, 30), scales = 20)+
      geom_pointrange(aes(ymin=mean-se, ymax=mean+se)) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      theme_classic() + theme(panel.background = element_blank(),
                              panel.grid = element_blank(),
                              legend.position = "top",
                              text = element_text(size=14),
                              axis.line = element_line(colour = 'black'),
                              axis.text = element_text(size =15))+
      # theme(legend.position = "none")+
      ylab('pixel distance from target') +
      expand_limits(y = c(0, 55)) +
      scale_y_continuous(breaks = seq(0, 55, 5))  
    })
  
  # plot 8: sequence only absolute precision 60-trial block (CHR paper)
  output$reps_fig5_plot7 <- renderPlot({
    req(reps.fig5())
    ggplot(subset(reps.fig5()$data_rep_train_60_fig, reps.fig5()$data_rep_train_60_fig$seqfoil1 == " seq"), 
           aes(x = block, y = mean, fill = group, color=group)) +
      geom_line() +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se),show.legend=F,size=0.5,width=.1) + 
      # scale_y_break(c(0.1, 30), scales = 20)+
      geom_pointrange(aes(ymin=mean-se, ymax=mean+se)) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      theme_classic() + theme(panel.background = element_blank(),
                              panel.grid = element_blank(),
                              legend.position = "top",
                              text = element_text(size=14),
                              axis.line = element_line(colour = 'black'),
                              axis.text = element_text(size =15))+
      # theme(legend.position = "none")+
      ylab('pixel distance from target') +
      expand_limits(y = c(0, 55)) +
      scale_y_continuous(breaks = seq(0, 55, 5))
  })
  
  # plot 9: absolute precision regression with score 60-trial block (CHR paper)
  output$reps_fig5_plot8 <- renderPlot({
    req(reps.fig5())
    ggplot(reps.fig5()$data_rep_train_60_regression_fig, 
           aes(x = score, y = pixl_dist1, fill = group, color=group)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
      scale_color_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      theme_classic() +
      ggpubr::stat_cor(method = 'spearman')+
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 14),
        axis.line = element_line(colour = 'black'),
        axis.text = element_text(size = 15)
      ) +
      xlab('Score') +
      ylab('Mean pixel distance from target') +
      facet_grid(~seqfoil1)
  })
  
  # plot 10: number of precision and repeat times across groups (12-trial block)
  output$reps_fig5_plot9 <- renderPlot({
    req(reps.fig5())
    ggplot(reps.fig5()$data_rep5_subset, 
           aes(x = rep_num1, y = mean, fill = group)) +
      geom_col(position = position_dodge2(preserve = 'total'), width = 0.7) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se),show.legend=F,size=0.5,width=.6,
                    position=position_dodge2(padding=0.5,preserve='single'))+ 
      # scale_y_break(c(0.1, 30), scales = 20)+
      # geom_pointrange(aes(ymin=mean-se, ymax=mean+se)) +
      scale_fill_manual(values = c('control' = 'cornflowerblue', 'high' = 'darkseagreen')) +
      theme_classic() + theme(panel.background = element_blank(),
                              panel.grid = element_blank(),
                              legend.position = "top",
                              text = element_text(size=14),
                              axis.line = element_line(colour = 'black'),
                              axis.text = element_text(size =15))+
      # theme(legend.position = "none")+
      ylab('pixel distance from target') +
      expand_limits(y = c(0, 55)) +
      scale_y_continuous(breaks = seq(0, 55, 5)) })
  
}

# rsconnect::deployApp('/Users/hanziyan/Desktop/23fall CHRSISL/chr_spr24_app')
shinyApp(ui, server)

