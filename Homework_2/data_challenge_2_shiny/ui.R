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

# Define UI for random distribution app 
fluidPage(
  
  # set shiny theme
  theme = shinytheme("superhero"),
  
  # App title 
  titlePanel("Tennis Grand Slams"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: grand slam
      radioButtons("grand_slam", "Grand Slam",
                   c("Australian Open" = "ao", 
                     "Roland Garros" = "rg", 
                     "Wimbledon" = "w", 
                     "US Open" = "us")),
      
      # Extra vertical spacing
      br(),
      
      # Input: player position on podium
      checkboxGroupInput("podium", "Podium Position",
                         c("Champion", 
                           "Runner-Up"),
                         selected = "Champion"),
      
      # Extra vertical spacing
      br(),
      
      # Input: player profile
      radioButtons("profile", "Player Profile",
                   c("Age", "Height")),
      
      # Extra vertical spacing
      br(),
      
      # Input: bin size for histogram
      sliderInput("bin_size", "Bin Size",
                  min = 1, max = 10,
                  value = 1),
      
      # Extra vertical spacing
      br(),
      
      # Input: player name 
      selectInput("player", "Player Name",
                  player_names$player)
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: histogram, stacked bar plot, and boxplots 
      tabsetPanel(type = "tabs",
                  
                  # Background Tab
                  tabPanel("Background",
                           
                           # Add book icon
                           icon = icon("book"),
                           
                           # Description 
                           strong("Background", style = "font-size: 35px"),
                           p(tagList("Data Source:", 
                                     a("https://www.kaggle.com/sijovm/atpdata",
                                       href = "https://www.kaggle.com/sijovm/atpdata"),
                                     style = "font-size:25px")
                           ),
                           p("In tennis, Grand Slam tournaments are considered the four
                                 most important tennis tournaments. There are four Grand
                                 Slams that occur year in the following locations and 
                                   time:",
                             style = "font-size:20px"),
                           br(),
                           h5("1. Australian Open: Hard Court; January in 
                                    Melbourne, Australia",
                              style = "font-size:18px"),
                           h5("2. Roland Garros (French Open): Clay Court; 
                                 May-June in Paris, France",
                              style = "font-size:18px"),
                           h5("3. Wimbledon: Grass Court; July in London, United Kingdom",
                              style = "font-size:18px"),
                           h5("4. US Open: Hard Court; August-Septemenber in New York, USA",
                              style = "font-size:18px"),
                           br(),
                           p("This application focuses solely on the results of the 
                                 single men's finalists at the Grand Slams, as this 
                                 category of competition typically garners both the most 
                                 interest and the results produced by players on the men's 
                                 side have historically been more consistent than on the 
                                   women's side, where upsets are more common.",
                             style = "font-size:20px"),
                           p("The goal of this analysis is to determine which Grand
                                 Slam final deviates the most from others. The app will 
                                 examine the Grand Slams based on three categories of finalists: 
                                 player profile (age, height), represented nations, and 
                                 individiual finalist records. The database used for this 
                                 analysis consists of information from the 'Open Era' of 
                                 tennis, marked from 1968 to present day. However, this 
                                 dataset is not current, as it only includes tournaments 
                                   up until 2017.",
                             style = "font-size:20px")
                           
                  ),
                  
                  # Finalist Tab
                  tabPanel("Finalist Profiles",
                           
                           # Add fingerprint icon
                           icon = icon("fingerprint"),
                           
                           br(),
                           
                           # Widget Description
                           p("This tab uses the following widgets: Grand 
                                   Slam, Podium Position, Player Profile, and 
                                   Bin Size", 
                             style = "font-size:20px"),
                           
                           br(),
                           
                           # Plot 
                           plotlyOutput("hist"),
                           
                           br(),
                           
                           # Description
                           strong("Description:", style = "font-size: 25px"),
                           p("These histograms displays the distribution of a
                              selected player profile, age or height, for 
                              either the Champion or Runner-Up at the selected 
                              Grand Slam.",
                             style = "font-size:20px"),
                           p("Among the four Grand Slams, both the Chapmions 
                              and the Runner-Ups are mostly symmetrical and 
                              could even be considered skewed right. It appears
                              that the Champions tend to fall within the ages of
                              22 and 26 years old. While Rolland Garros, 
                              Wimbledon, and the US Open tend to have both 
                              champions and runner-ups around the age of 25 or 
                              below, the Australian Open tends to have older champions. 
                              The Australian Open also appears to have the widest 
                              spread of championages.",
                             style = "font-size:20px"),
                           p("Like player age, the height distributions across
                            the Grand Slams are mostly symmetrical, with the 
                            majority of both champions and runner-ups being between
                            180-185 cm tall. However, Roland Garros tends to have
                            shorter more short finalists, as it is the only slam
                            that does not have a finalist over 200 cm and has a 
                            finalist under 170 cm.",
                             style = "font-size:20px")
                           
                  ),
                  tabPanel("Finalist Countries", 
                           
                           # Add flag icon
                           icon = icon("flag"),
                           
                           br(),
                           
                           # Widget Description
                           p("This tab uses the following widgets: Grand 
                                   Slam and Podium Position.", 
                             style = "font-size:20px"),
                           
                           br(),
                           
                           # Plot
                           plotlyOutput("dot"),
                           
                           br(),
                           
                           # Description 
                           strong("Description:", style = "font-size: 25px"),
                           p("These lollipop charts display the raw counts of 
                            of either champions or runner-ups by country for a 
                            selected Grand Slam. ",
                             style = "font-size:20px"),
                           p("The United States leads in both the number of 
                            champions and number of runner-ups at all the Grand 
                            Slams except Roland Garros in the Champions podium
                            position. Instead, Spain leads with the most 
                            Roland Garros Champions, and is also second behind 
                            the U.S. in the number of runner-ups at the 
                            tournament. This makes sense, given the preferred 
                            surface for training in Spain are clay courts, 
                            which is the same surface at Roland Garros.",
                             style = "font-size:20px")
                           
                  ),
                  tabPanel("Finalist Records", 
                           
                           # Add trophy icon
                           icon = icon("trophy"),
                           
                           br(),
                           
                           # Widget Description
                           p("This tab uses the following widgets: Player
                                   Name.", 
                             style = "font-size:20px"),
                           
                           br(),
                           
                           # Plot
                           plotlyOutput("bar"),
                           
                           br(),
                           
                           # Description
                           strong("Description:", style = "font-size: 25px"),
                           p("After examining the represented nations with the most
                      champions and runner-ups in the 'Represented Nations' 
                      it would be interesting to look at the individual players
                      and see which Grand Slam seems most difficult for players
                      on average to win.",
                             style = "font-size:20px"),
                           p("Going through the players, one begins to see that court
                      surface may play a role in victories. For example, if a
                      player found more success at Wimbledon, with the faster 
                      surface of grass, they were more likely to have less 
                      appearances and wins at the final of Roand Garros. This 
                      can be exemplified with Boris Becker, Andy Murray, 
                      Roger Federer and Pete Sampras who never made it to the 
                      finals at Roland Garros despite having multiple appearances
                      at the other three and being undefeated in the finals at 
                      Wimbledon. Meanwhile, a similar story can be seen by those 
                      who find success on the slower clay courts of Roland Garros
                      have more difficulty finding the same success at Wimbledon. 
                      This can be exemplified with Guillermo Vilas, Sergi Bruguera, 
                      Jim Courier, and the dubbed 'King of Clay' Rafael Nadal. 
                      However, thereare very few cases where if a player 
                      dominates on clay at Roland Carros that they will not do 
                      well at other Grand Slams. For example, Bjorn Borg and 
                      Novak Djokovic, while seemingly finding success at Roland
                      Garros, manage to do well at other tournaments.",
                             style = "font-size:20px"),
                           p("Thus, Roland Garros appears to deviate the most from 
                      the other Grand Slams.",
                             style = "font-size:20px")
                           
                  )
      )
    )
  )
)
