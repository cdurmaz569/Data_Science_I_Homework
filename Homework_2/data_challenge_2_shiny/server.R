# Shiny Web Application: ATP Men's Tennis Grand Slam Analysis
# Author: Ceyda Durmaz
# Date: 10/14/2020
# source: https://www.kaggle.com/sijovm/atpdata

# Load libraries
library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(rsconnect)

# set theme
theme_set(theme_clean())

# Load data
# setwd("~/Documents/Data_Science_I/Homework/Homework_2/data_challenge_2_shiny/")
atp <- read.csv("ATP.csv")

# Get finalist stats from each slam
aussie_finals <- atp %>%
  filter(tourney_name == "Australian Open" | 
           tourney_name == "Australian Chps.") %>% 
  filter(round == "F")
aussie_finals$tourney_name = "Australian Open"
french_finals <- atp %>% 
  filter(tourney_name == "Roland Garros") %>%
  filter(round == "F")
wimbledon_finals <- atp %>% 
  filter(tourney_name == "Wimbledon") %>%
  filter(round == "F")
us_finals <- atp %>% 
  filter(tourney_name == "US Open" | tourney_name == "Us Open") %>% 
  filter(round == "F")
us_finals$tourney_name = "US Open"
slam_finals <- rbind(aussie_finals, french_finals, wimbledon_finals, us_finals)

# Get names of finalists in grand slams 
winner_names <- data.frame(slam_finals$winner_name)
names(winner_names) <- c("player")
loser_names <- data.frame(slam_finals$loser_name)
names(loser_names) <- c("player")
player_names <- merge(winner_names, loser_names, 
                      by = "player", 
                      sort = TRUE)
player_names <- distinct(player_names)

# Define server logic for random distribution app 
function(input, output) {
  
  # Histogram of player profiles
  output$hist <- renderPlotly({
    grand_slam <- switch(input$grand_slam,
                         ao = aussie_finals,
                         rg = french_finals,
                         w = wimbledon_finals,
                         us = us_finals)
    
    # select color based on slam
    if(identical(grand_slam, aussie_finals)){
      hist_color = "skyblue"
    } else if(identical(grand_slam, french_finals)){
      hist_color = "peru"
    } else if(identical(grand_slam, wimbledon_finals)){
      hist_color = "springgreen4"
    } else{
      hist_color = "firebrick3"
    }
    
    bin_size = input$bin_size
    if (input$profile == "Age"){
      # setup dataframes
      grand_slam$title1 <- "Ages of Champions"
      grand_slam$title2 <- "Ages of Runner-Ups"
      age_winner <- ggplot(grand_slam, aes(x = winner_age)) + 
        geom_histogram(color = "white", 
                       fill = hist_color, 
                       binwidth = bin_size) + 
        labs(x = "Age", y = "Count") + 
        facet_wrap(~title1)
      age_loser <- ggplot(grand_slam, aes(x = loser_age)) + 
        geom_histogram(color = "white", 
                       fill = hist_color, 
                       binwidth = bin_size) + 
        labs(x = "Age", y = "Count") + 
        facet_wrap(~title2)
      age_both <- subplot(list(age_winner, age_loser), 
                          nrows = 2, margin = 0.08,
                          shareX = TRUE, shareY = TRUE)
      
      # plot histograms
      if (length(input$podium) > 1){
        ggplotly(age_both)
      } else if (input$podium == "Champion"){
        ggplotly(age_winner)
      } else {
        ggplotly(age_loser)
      }
      
    } else {
      # setup dataframes
      grand_slam$title1 <- "Heights of Champions"
      grand_slam$title2 <- "Heights of Runner-Ups"
      ht_winner <- ggplot(grand_slam, aes(x = winner_ht)) + 
        geom_histogram(color = "white", 
                       fill = hist_color, 
                       binwidth = bin_size) + 
        labs(x = "Height (cm)", y = "Count") + 
        facet_wrap(~title1)
      ht_loser <- ggplot(grand_slam, aes(x = loser_ht)) + 
        geom_histogram(color = "white", 
                       fill = hist_color, 
                       binwidth = bin_size) + 
        labs(x = "Height (cm)", y = "Count") + 
        facet_wrap(~title2)
      ht_both <- subplot(ht_winner, ht_loser, 
                         nrows = 2, margin = 0.08,
                         shareX = TRUE, shareY = TRUE)
      
      # plot histograms
      if (length(input$podium) > 1){
        ggplotly(ht_both)
      } else if (input$podium == "Champion"){
        ggplotly(ht_winner)
      } else {
        ggplotly(ht_loser)
      }
    } 
  })
  
  # Dot plot of wins and losses in grand slam finals by country 
  output$dot <- renderPlotly({
    grand_slam <- switch(input$grand_slam,
                         ao = aussie_finals,
                         rg = french_finals,
                         w = wimbledon_finals,
                         us = us_finals)
    
    # select color based on slam
    if(identical(grand_slam, aussie_finals)){
      dot_color = "skyblue"
    } else if(identical(grand_slam, french_finals)){
      dot_color = "peru"
    } else if(identical(grand_slam, wimbledon_finals)){
      dot_color = "springgreen4"
    } else{
      dot_color = "firebrick3"
    }
    
    # reformat grand_slam data frame
    winners <- grand_slam %>% 
      select(winner_ioc) %>%
      count(winner_ioc)
    losers <- grand_slam %>% 
      select(loser_ioc) %>%
      count(loser_ioc)
    winners$title1 <- "Number of Champions by Country"
    losers$title2 <- "Number of Runner-Ups by Country"
    
    # build ggplots
    ggplot_winner <- ggplot(winners, aes(x = winner_ioc, y = n)) + 
      geom_point(col = dot_color, size = 3) + 
      geom_segment(aes(x = winner_ioc,
                       xend = winner_ioc,
                       y = 0, 
                       yend = n),
                   col = dot_color) + 
      theme_clean() + 
      labs(x = "Country", y = "Count") + 
      facet_wrap(~title1)
    ggplot_loser <- ggplot(losers, aes(x = loser_ioc, y = n)) + 
      geom_point(col = dot_color, size = 3) + 
      geom_segment(aes(x = loser_ioc,
                       xend = loser_ioc,
                       y = 0, 
                       yend = n),
                   col = dot_color) + 
      theme_clean() + 
      labs(x = "Country", y = "Count") + 
      facet_wrap(~title2)
    ggplot_both <- subplot(ggplot_winner, ggplot_loser, 
                           nrows = 2, margin = 0.1)
    
    # choose output based on podium position
    if(length(input$podium) > 1){
      ggplotly(ggplot_both)
    } else if (input$podium == "Champion"){
      ggplotly(ggplot_winner)
    } else {
      ggplotly(ggplot_loser)
    } 
  })
  
  # Stacked bar plot of player records at finals of Grand Slams
  output$bar <- renderPlotly({
    # setup data
    player_wins <- slam_finals %>% filter(winner_name == input$player) %>%
      select(tourney_name)
    player_loss <- slam_finals %>% filter(loser_name == input$player) %>%
      select(tourney_name)
    player_wins$podium <- "Champion"
    player_loss$podium <- "Runner-Up"
    player_record <- rbind(player_wins, player_loss)
    player_record <- count(player_record, tourney_name, podium, sort = TRUE)
    
    ggplot_bar <- ggplot(player_record, 
                         aes(x = reorder(tourney_name, n), 
                             y = n, 
                             fill = factor(podium, 
                                           levels = c("Runner-Up",
                                                      "Champion")))) + 
      geom_bar(position = "stack", stat = "identity") + 
      coord_flip() + 
      theme_classic() + 
      scale_fill_brewer(palette = "Dark2") + 
      labs(title = paste0("Player Record for: ",input$player),
           x = "Count", 
           y = "Grand Slam",
           fill = "Podium Position")
    ggplotly(ggplot_bar)
  })
  
}

