### Welcome etc.

# Load the .csv files for each respective league dataset using the 'here' package, meaning that the app can always locate the root folder despite being on different computers
# Each league has a 'pre' Covid-19 set and a 'during' or post Covid-19 postponement set, meaning 10 data frames must be combined into 1.
library(here)

plprecov <- read.csv(here("data", "pl1920precovidfixtures.csv"))
plduringcov <- read.csv(here("data", "pl1920duringcovidfixtures.csv"))

blprecov <- read.csv(here("data", "bl1920precovidfixtures.csv"))
blduringcov <- read.csv(here("data", "bl1920duringcovidfixtures.csv"))

ligprecov <- read.csv(here("data", "lig1920precovidfixtures.csv"))
ligduringcov <- read.csv(here("data", "lig2021duringcovidfixtures.csv"))

laligaprecov <- read.csv(here("data", "laliga1920precovidfixtures.csv"))
laligaduringcov <- read.csv(here("data", "laliga1920duringcovidfixtures.csv"))

serprecov <- read.csv(here("data", "ser1920precovidfixtures.csv"))
serduringcov <- read.csv(here("data", "ser1920duringcovidfixtures.csv"))


# Define values for pre or during Covid-19 data frames. These will be added to a new variable column.
predesignation <- c("pre")
duringdesignation <- c("during")

# Add the new column with correct values to the respective data frame.
plprecov$preduring <- predesignation
plduringcov$preduring <- duringdesignation

blprecov$preduring <- predesignation
blduringcov$preduring <- duringdesignation

ligprecov$preduring <- predesignation
ligduringcov$preduring <- duringdesignation

laligaprecov$preduring <- predesignation
laligaduringcov$preduring <- duringdesignation

serprecov$preduring <- predesignation
serduringcov$preduring <- duringdesignation

# This will also be done for what league a match took place in. This will later be used to subset data based on user input.
# Define the values for the different leagues.
pldesignation <- c("premier league")
bldesignation <- c("bundesliga")
ligdesignation <- c("ligue 1")
laligadesignation <- c("laliga")
serdesignation <- c("serie a")

# Add the column with corresponding value to the respective data frame.
plprecov$League <- pldesignation
plduringcov$League <- pldesignation

blprecov$League <- bldesignation
blduringcov$League <- bldesignation

ligprecov$League <- ligdesignation
ligduringcov$League <- ligdesignation

laligaprecov$League <- laligadesignation
laligaduringcov$League <- laligadesignation

serprecov$League <- serdesignation
serduringcov$League <- serdesignation


# Some values seem to be corrupted during the exportation of data from fbref.com to a .csv file. The two Ligue 1 sets have incorrect column names which prevent data frames from being bound.
# Rename the Ligue 1 'day' columns to match the names of the other files before we can merge. This can be done using dplyr's 'rename' function.
library(dplyr)

ligprecov <- ligprecov %>% 
  rename("ï..Week" = Week)
ligduringcov <- ligduringcov %>% 
  rename("ï..Week" = Week)

# Merge the 10 separate data frames into 1 'Big 5' leagues frame.  This is done using the base function 'rbind'.
big5data <- rbind(plprecov, plduringcov, blprecov, blduringcov, ligprecov, ligduringcov, laligaprecov, laligaduringcov, serprecov, serduringcov)


# Dropping irrelevant columns using the subset function, such as day, time, attendence etc.
big5data = subset(big5data, select = -c(ï..Week, Day, Time, Attendance, Venue))

# Rename variable columns using the dplyr function rename.
big5data = big5data %>% 
  rename("homexG" = xG,
         "awayxG" = xG.1)

# Once again, 
# We need to split the total score into 'home' and 'away' goals. Right now it is formatted altogether as '0-0', with the home team represented as the first score
# Splitting the score column using the tidyr 'seperate' function with 'parse_number', with the first number as the seperator.
# This score can then be added to a new 'homegoals' column, with the rest of the string added to an 'awaygoals' column.
library(tidyverse)
library(readr)

big5data <- big5data %>% separate(Score, into = c("homegoals" , "awaygoals"), sep = parse_number(str_sub(1)))
big5data$homegoals <- as.numeric(big5data$homegoals)

# Drop all non-numeric string from the newly formed awaygoals column to get the correct values.
big5data$awaygoals <- parse_number(big5data$awaygoals)

# Use the function 'tolower' to change column names to lowercase, making them easier to work with
names(big5data) <- tolower(names(big5data))

# Create new columns using the data
# Firstly, a goal difference column
goaldiff <- big5data$homegoals - big5data$awaygoals
big5data$goaldiff <- goaldiff

# Home points/ away points column
homepoints <- ifelse(big5data$homegoals > big5data$awaygoals, homepoints <- 3, 
                     homepoints <- ifelse(big5data$homegoals == big5data$awaygoals, homepoints <- 1, 
                                          homepoints <- ifelse(big5data$homegoals < big5data$awaygoals, homepoints <- 0, NA)))

big5data$homepoints <- homepoints


awaypoints <- ifelse(big5data$awaygoals > big5data$homegoals, awaypoints <- 3, 
                     awaypoints <- ifelse(big5data$awaygoals == big5data$homegoals, awaypoints <- 1, 
                                          awaypoints <- ifelse(big5data$awaygoals < big5data$homegoals, awaypoints <- 0, NA)))

big5data$awaypoints <- awaypoints

# Clean sheets column. In official statistics, a 0-0 draw is counted as both teams keeping a clean sheet, so this checks whether the opposition team's goals = 0
# Kept clean sheets coded as 1, non-clean sheets coded as 0
homecleansheets <- ifelse(big5data$awaygoals == 0, homecleansheets <- 1, homecleansheets <- 0)
big5data$homecleansheets <- homecleansheets

awaycleansheets <- ifelse(big5data$homegoals == 0, awaycleansheets <- 1, awaycleansheets <- 0)
big5data$awaycleansheets <- awaycleansheets


# The shiny app will use the 'preduring' values as names as labels on the interactive graphs, so this will need to be changed to be grammatically correct
# To do this, we will use the dplyr functions 'mutate' and 'recode'
big5data <- big5data %>% 
  mutate(preduring = recode(preduring, pre = "Pre Covid-19 Postponement", during = "Post Covid-19 Postponement"))

# We will need a long format data version for the option to view individual teams. We will use tidyr's 'gather' function for this
big5data_long <- big5data %>% 
  gather(big5data, )

library(ggplot2)

############################################################################################

#Load the library for shiny and define the application's UI
library(shiny)
library(shinythemes)

ui <- navbarPage("Kieran Taggart (2022)",
                 
                 tabPanel("Home",
                          icon = icon("home"),
                          
                          h1("Home Advantage and Covid-19; Football's Natural Experiment"),
                          
                          h3("What can a year without fans tell us about the efficacy of home advantage?"),
                          
                          hr(),
                          
                          img(src = "https://images.pexels.com/photos/46798/the-ball-stadion-football-the-pitch-46798.jpeg", height = 400, width = 600, align = "center"),
                          
                          br(),
                          
                          h4("This is a Shiny app to demonstrate to what extent the absence of crowds during the Covid-19 pandemic affected home advantage across the top 5 European leagues."),
                          h4("To access theinteractive graph or to learn more about the background of the project, please use the navigation bar at the top of the page"),
                          
                          br(),
                          
                          h4("The interactive graph enables you to ADD WHAT GRAPH SHOWS"),
                          
                          br(),
                          
                          h4("To access files associated with this project, please go to my GitHub LINK GITHUB")
                          ),
                 
                 tabPanel("Background and Questions",
                          icon = icon("question-sign", lib = "glyphicon"),
                          
                          h2(strong("Project Background and Reseach Questions")),
                          
                          hr(),
                          
                          img(src = "https://images.pond5.com/soccer-football-stadium-night-match-footage-123662921_iconl.jpeg", height = 550, width = 750, align = "right"),
                          
                          h3("'Home Advantage' - the idea that teams who play in their home stadium with a majority share of fans perform better than away teams - is a well-documented phenomenon. In the 18/19 English Premier League season, home teams won 47% of matches whereas away teams won 34% of matches."),
                          h3("Home advantage is attributed to a few sources; Psychological and physiological effects on home players drawn from the support of home fans, social pressure exerted upon match officials to make decisions that benefit the home team, negative psychological effects on away players from home fans or the effects of travel and unfamiliarity of the home team's facilities."),
                          h3("The Covid-19 pandemic provided a window of opportunity as football from each of the top-flight European leagues was played without spectators behind closed doors. This allowed for the observation of the effect of home-advantage wihtout the presence of fans, enabling us to measure to what extent fans had an influence on both team's and match official's performance."),
                          h3("As such, the research questions are thus:"),
                          
                          br(),
                          
                          h3("1.  Is there a difference in terms of recorded football statistics with and without home crowd support?"),
                          h3("2.  If so, how does this differ across the top 5 European Leagues?")
                          
                          
                          
                 ),
                 
                 tabPanel("Interactive Graph",
                          icon = icon("chart-bar"),
                          
                          tabsetPanel(
                            
                            tabPanel("View Data by Individual League",
                                     
                                     sidebarPanel(width = 3,
                                       
                                       selectizeInput("user_league", "Select a League:",
                                                      
                                                      c("Top 5 Leagues Combined",
                                                        'Premier League' = "premier league", 
                                                        'Bundesliga' = "bundesliga", 
                                                        'Ligue 1' =  "ligue 1", 
                                                        'Serie A' = "serie a", 
                                                        'LaLiga' = "laliga"),
                                                      
                                       ),
                                       
                                       selectizeInput("user_statistic", "Select a Statistic to Compare:",
                                                      
                                                      c('Home Goals per Game' = "homegoals", 
                                                        'Away Goals per Game' = "awaygoals", 
                                                        'Home xG per Game' = "homexg", 
                                                        'Away xG per Game' = "awayxg",
                                                        'Points per Game at Home' = "homepoints",
                                                        'Points per Game Away' = "awaypoints",
                                                        'Home Clean Sheets' = "homecleansheets",
                                                        'Away Clean Sheets' = "awaycleansheets",
                                                        'Mean Goal Difference per Game' = "goaldiff")
                                                      
                                       ),
                                       
                                       h4("Please select a League to observe and a statistic to compare."),
                                       h4("Home/Away Goals and Points, as well as xG statistics are a mean number per game."),
                                       h4("Home/Away Clean Sheet statistics show the percentage of games in which teams did not concede a goal, expressed as a decimal"),
                                       h4("Goal difference is the mean goal difference per game. Goal Difference is calculated using  home goals - away goals. 
                                          As such, a positive number denotes more goals scored on average by home sides while a negative number denotes more goals scored on average by away sides."),
                                       
                                       br(),
                                       
                                       h4(strong("What is xG?")),
                                       h4("xG, or 'expected goals', is a measure between 0 and 1 assigned to shots made in a game of football and measures the probability that a shot will result in a goal, with the closer a shot's xG is to 1 the more likely that shot is to result in a goal."),
                                       h4("To calculate a shots allocated xG, it is compared to thousands of similar shots with similar characteristics, such as the location of the shooter and positions of defenders, and it is observed how many shots from similar positions resulted in goals."),
                                       h4("For a more detailed explanation, visit" , a("FBRef's xG explained page.", href = "https://fbref.com/en/expected-goals-model-explained/"))
                                     ),
                                     
                                     mainPanel("",
                                               
                                               plotOutput("plot1", height = 850, width = 1000)
                                               
                                     ),
                            ),
                            
                            
                            tabPanel("View All Leagues Together",
                                     
                                     sidebarPanel(width = 3,
                                    
                                       selectizeInput("user_statistic_ov", "Select a Statistic to Compare:",
                                                      
                                                      c('Home Goals per Game' = "homegoals", 
                                                        'Away Goals per Game' = "awaygoals", 
                                                        'Home xG per Game' = "homexg", 
                                                        'Away xG per Game' = "awayxg",
                                                        'Points per Game at Home' = "homepoints",
                                                        'Points per Game Away' = "awaypoints",
                                                        'Home Clean Sheets' = "homecleansheets",
                                                        'Away Clean Sheets' = "awaycleansheets",
                                                        'Mean Goal Difference per Game' = "goaldiff")
                                                      
                                       ),
                                       
                                       h4("Please select a statistic to compare across all 5 leagues simultaneously."),
                                       h4("Home/Away Goals and Points, as well as xG statistics are a mean number per game"),
                                       h4("Home/Away Clean Sheet statistics show the percentage of games in which teams did not concede a goal, expressed as a decimal"),
                                       h4("Goal difference is the mean goal difference per game. Goal Difference is calculated using  home goals - away goals. 
                                          As such, a positive number denotes more goals scored on average by home sides while a negative number denotes more goals scored on average by away sides."),
                                       
                                       br(),
                                       
                                       h4(strong("What is xG?")),
                                       h4("xG, or 'expected goals', is a measure between 0 and 1 assigned to shots made in a game of football and measures the probability that a shot will result in a goal, with the closer a shot's xG is to 1 the more likely that shot is to result in a goal."),
                                       h4("To calculate a shots allocated xG, it is compared to thousands of similar shots with similar characteristics, such as the location of the shooter and positions of defenders, and it is observed how many shots from similar positions resulted in goals."),
                                       h4("For a more detailed explanation, visit" , a("FBRef's xG explained page.", href = "https://fbref.com/en/expected-goals-model-explained/"))
                                     ),
                                     
                                     mainPanel("",
                                               
                                               plotOutput("plot3", height = 850, width = 1325)
                                               
                                     )
                            )
                            
                         )),
                 
                 tabPanel("  Interpretation and Discussion",
                          icon = icon("comment", lib = "glyphicon"),
                          
                          h2(strong("Interpretation and Discussion")),
                          
                          hr(),
                          
                 ),
                 
                 
                 tabPanel("Limitations and Future Implementation",
                          icon = icon("forward", lib = "glyphicon"),
                          
                          h2(strong("Limitations and Future Implementation")),
                          
                          hr(),
                          
                          
                 ),
                 
                 
                 tabPanel("Dataset Information",
                          icon = icon("info"),
                          
                         h2(strong("Dataset Information")),
                         
                         hr(),
                         
                         img(src = "https://miro.medium.com/max/1400/1*yDcbh8c5um2rI8dWMWU4-g.png", height = 450, width = 850, align = "right"),
                         
                         h4("Data for fixtures and results was exported from", a("FBRef", href = "https://fbref.com/en/"), "Data was extracted from the so called 'Big 5' leagues across europe, which consists 5 top flight leagues across Europe:"),
                         
                         br(),
                         
                         h5("English Premier League"),
                         h5("Bundesliga"),
                         h5("Ligue 1 Uber Eats"),
                         h5("Serie A TIM"),
                         h5("LaLiga Santander"),
                         
                         br(),
                         
                         h4("These leagues, during the 19/20 season, had their fixtures suspended and at a later date resumed play behind closed doors, with the exception of the French Ligue 1, who abandoned their 19/20 season before any crowd-less games took place and concluded league positions based on an average points-per-game basis. As such, this analysis uses games from the 20/21 season which was played entirely behind closed doors.
")
                           ),

                 collapsible = TRUE, fluid = TRUE, inverse = TRUE, theme = shinytheme("yeti"),
                 windowTitle = "Home Advantage and Covid-19: Football's Natural Experiment.",
                 position = "static-top")

server <- function(input, output, session) {
  
  user_leaguefilter <- reactive({
    
    if (input$user_league == "Top 5 Leagues Combined") {

      df <- big5data
      
    }
    
    else if (input$user_league != "Top 5 Combined") {
    
    df <- subset(big5data, big5data$league %in% input$user_league)
      
    }
    
  })
  

  
  output$plot1 <- renderPlot({
    
    active_plot <- ggplot(user_leaguefilter(), aes_string(x="preduring", y=input$user_statistic, fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "purple4"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    if (input$user_statistic == "homecleansheets"){
      
      active_plot <- active_plot+
        coord_cartesian(ylim = c(0, 1))
      
    } else if (input$user_statistic == "awaycleansheets"){
      
      active_plot <- active_plot+
        coord_cartesian(ylim = c(0, 1))
      
    } else if (input$user_statistic == "goaldiff"){
      
      active_plot <- active_plot+
        coord_cartesian(ylim = c(-0.5, 0.5))+
        geom_hline(yintercept = 0)
      
    } else {
      
      active_plot <- active_plot+
        coord_cartesian(ylim = c(0.7, 1.8))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      
    }
  
    
    print(active_plot)
    ggsave(filename = paste(input$user_league, input$user_statistic, ".png", sep = ""), path = here("figs"))
    
    
    
  })
  
  output$plot3 <- renderPlot({
    
    overview_plot <- ggplot(big5data, aes_string(x = "league", y = input$user_statistic_ov, fill = "preduring"))+
      geom_bar(stat = "summary", fun = "mean",
               width = 0.7,
               position = position_dodge2(0.5, reverse = TRUE))+
      scale_x_discrete(labels = c("premier league" = "Premier League", "bundesliga" = "Bundesliga", "ligue 1" = "Ligue 1", "serie a" = "Serie A", "laliga" = "LaLiga"), 
                       limits = c("premier league", "bundesliga", "ligue 1", "serie a", "laliga"))+
      theme_minimal()+
      scale_fill_manual(breaks = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"),
                        values=c("purple4",
                                 "orange2"))+
      labs(x = " ", y = " ")+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 16),
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))+
      labs(fill="Pre/Post Covid-19")
    
    if (input$user_statistic_ov == "homecleansheets"){
      
      overview_plot <- overview_plot+
        coord_cartesian(ylim = c(0, 1))
      
    } else if (input$user_statistic_ov == "awaycleansheets"){
      
      overview_plot <- overview_plot+
        coord_cartesian(ylim = c(0, 1))
      
    } else if (input$user_statistic_ov == "goaldiff"){
      
      overview_plot <- overview_plot+
        coord_cartesian(ylim = c(-0.5, 0.5))+
        geom_hline(yintercept = 0)
      
    } else {
      
      overview_plot <- overview_plot+
        coord_cartesian(ylim = c(0.7, 1.8))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    }
    
    print(overview_plot)
    ggsave(filename = paste("Top 5 Leagues Together", input$user_statistic_ov, ".png", sep = ""), path = here("figs"))
    
  })
    
}

shinyApp(ui, server)                 