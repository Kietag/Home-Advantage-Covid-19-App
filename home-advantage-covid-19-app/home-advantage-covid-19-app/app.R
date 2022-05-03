### This is the app.r file for the Shiny App 'Home Advantage and Covid-19'.
### To view a web-based version of this Shiny App, visit https://kietag.shinyapps.io/home-advantage-covid-19-app/

## Firstly, the raw data will need to be top-loaded, as well as all data processing before we get to the contents of the shiny app.

# Load the relevant libraries for the app.
library(here)
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(shiny)
library(shinythemes)

# Load the .csv files for each respective league dataset using the 'here' package, meaning that the app can always locate the root folder despite being on different computers.
# Each league has a 'pre' Covid-19 set and a 'during' or post Covid-19 postponement set, meaning 10 data frames must be combined into 1 large data frame.
setwd(here())

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


# Some values seem to be corrupted during the exportation of data from fbref.com to a .csv file. The two Ligue 1 sets have incorrect column names 
# which prevent data frames from being bound.
# Subset the data frames, selecting to drop the first column, 'Day' from all data frames.

plprecov <- subset(plprecov, select = -c(1))
plduringcov <- subset(plduringcov, select = -c(1))

blprecov <- subset(blprecov, select = -c(1))
blduringcov <- subset(blduringcov, select = -c(1))

ligprecov <- subset(ligprecov, select = -c(1))
ligduringcov <- subset(ligduringcov, select = -c(1))

laligaprecov <- subset(laligaprecov, select = -c(1))
laligaduringcov <- subset(laligaduringcov, select = -c(1))

serprecov <- subset(serprecov, select = -c(1))
serduringcov <- subset(serduringcov, select = -c(1))

# Merge the 10 separate data frames into 1 'Big 5' leagues frame.  This is done using the base function 'rbind'.
big5data <- rbind(plprecov, plduringcov, blprecov, blduringcov, ligprecov, ligduringcov, laligaprecov, laligaduringcov, serprecov, serduringcov)

# Drop irrelevant columns using the subset function, such as time, attendence etc.
big5data = subset(big5data, select = -c(Day, Time, Attendance, Venue))

# Rename the two xG variable columns to home/away variables using the dplyr function 'rename'.
big5data = big5data %>% 
  rename("homexG" = xG,
         "awayxG" = xG.1)

# Once again We need to split the total score into 'home' and 'away' goals. Right now it is formatted altogether as '0-0', with the home team represented as the first score.
# Splitting the score column using the tidyr 'seperate' function with 'parse_number', with the first number as the seperator.
# This score can then be added to a new 'homegoals' column, with the rest of the string added to an 'awaygoals' column.
big5data <- big5data %>% separate(Score, into = c("homegoals" , "awaygoals"), sep = parse_number(str_sub(1)))
big5data$homegoals <- as.numeric(big5data$homegoals)

# Drop all non-numeric string from the newly formed awaygoals column to get the correct values.
big5data$awaygoals <- parse_number(big5data$awaygoals)

# Use the function 'tolower' to change column names to lowercase, making them easier to work with.
names(big5data) <- tolower(names(big5data))

# Create new columns for variables that will be analysed later.
# Firstly, a goal difference column. This is calulated by subtracting away goals from home goals.
goaldiff <- big5data$homegoals - big5data$awaygoals
big5data$goaldiff <- goaldiff

# Home points/ away points column.
# This is created using nested 'ifelse' functions to determine how many points should be designated to home/away teams depending on results.
homepoints <- ifelse(big5data$homegoals > big5data$awaygoals, homepoints <- 3, 
                     homepoints <- ifelse(big5data$homegoals == big5data$awaygoals, homepoints <- 1, 
                                          homepoints <- ifelse(big5data$homegoals < big5data$awaygoals, homepoints <- 0, NA)))
big5data$homepoints <- homepoints

awaypoints <- ifelse(big5data$awaygoals > big5data$homegoals, awaypoints <- 3, 
                     awaypoints <- ifelse(big5data$awaygoals == big5data$homegoals, awaypoints <- 1, 
                                          awaypoints <- ifelse(big5data$awaygoals < big5data$homegoals, awaypoints <- 0, NA)))
big5data$awaypoints <- awaypoints

# Clean sheets column. In official statistics, a 0-0 draw is counted as both teams keeping a clean sheet, so this checks whether the opposition team's goals = 0
# Kept clean sheets coded as 1, non-clean sheets coded as 0. We can use this to calculate a percentage expressed as a decimal.
homecleansheets <- ifelse(big5data$awaygoals == 0, homecleansheets <- 1, homecleansheets <- 0)
big5data$homecleansheets <- homecleansheets

awaycleansheets <- ifelse(big5data$homegoals == 0, awaycleansheets <- 1, awaycleansheets <- 0)
big5data$awaycleansheets <- awaycleansheets

# The shiny app will use the 'preduring' values as labels on the interactive graphs, so this will need to be changed to be grammatically correct.
# To do this, we will use the dplyr functions 'mutate' and 'recode'
big5data <- big5data %>% 
  mutate(preduring = recode(preduring, pre = "Pre Covid-19 Postponement", during = "Post Covid-19 Postponement"))

# To render graphs for the interpretation section of the Shiny App, there will need to be pre-determined subsets for leagues not based on user input.
plsubset <- subset(big5data, big5data$league == "premier league")
ligsubset <- subset(big5data, big5data$league == "ligue 1")
sersubset <- subset(big5data, big5data$league == "serie a")

####################### Shiny App ##########################

# First we will define the user interface for the shiny app.

# A navigation bar will enable users to select different tabs for information on the project.
ui <- navbarPage("Home Advantage and Covid-19: Football's Natural Experiment",
                 
                 # Each tabPanel will become its own page accessible via the navbar, as well as being given 
                 tabPanel("Home",
                          icon = icon("home"),
                          
                          h1("Home Advantage and Covid-19: Football's Natural Experiment"),
                          
                          h3("What can a year without fans tell us about the efficacy of home advantage?"),
                          
                          hr(),
                          
                          img(src = "https://images.pexels.com/photos/46798/the-ball-stadion-football-the-pitch-46798.jpeg", height = 400, width = 600, align = "center"),
                          
                          br(),
                          
                          h4("This is a Shiny app to demonstrate to what extent the absence of crowds during the Covid-19 pandemic affected home advantage 
                             across the top 5 European leagues."),
                          h4("To access the interactive graph or to learn more about the background of the project, please use the navigation bar at the top of the page."),
                          
                          br(),
                          
                          h4("The interactive graph enables you to compare different home/away statistics across different top-flight Eurpoean leagues both before 
                             and after the Covid-19 postponement of league football."),
                          
                          br(),
                          
                          h4("To access files associated with this project, please go to the" , a("GitHub for this app.", href = "https://github.com/Kietag/Home-Advantage-Covid-19-App"))
                 ),
                 
                 tabPanel("Background and Questions",
                          icon = icon("question-sign", lib = "glyphicon"),
                          
                          h2(strong("Project Background and Reseach Questions")),
                          
                          hr(),
                          
                          img(src = "https://images.pond5.com/soccer-football-stadium-night-match-footage-123662921_iconl.jpeg", height = 450, width = 650, align = "right"),
                          
                          h4("'Home Advantage' - the idea that teams who play in their home stadium with a majority share of fans perform better than away teams - is a well-documented 
                             phenomenon. In the 18/19 English Premier League season, home teams won 47% of matches whereas away teams won 34% of matches."),
                          h4("A literature review in 1992 by Courneya & Carrion found that home teams win more than half of the games given that home and away games are distributed evenly. 
                             The literature review took place in 1992, and it is argued that the effect of home advantage has dimished over time."),
                          h4("Home advantage is attributed to a few sources; Psychological and physiological effects on home players drawn from the support of home fans, social 
                             pressure exerted upon match officials to make decisions that benefit the home team, negative psychological effects on away players from home fans or 
                             the effects of travel and unfamiliarity of the home team's facilities."),
                          h4("The Covid-19 pandemic provided a window of opportunity as football from each of the top-flight European Leagues was played without spectators behind closed
                             doors. This allowed for the observation of the effect of home-advantage wihtout the presence of fans, enabling us to measure to what extent fans had an 
                             influence on both team's and match official's performance."),
                          h4("However, some studies disagree with the idea that an abesent crowd affects home advantage. An inter-season analysis by Ramchandi & Millar (2021) concluded that
                             there was insufficient evidence to conclude that absent fans affects home advantage in football."),
                          h4(strong("As such, the research questions are thus:")),
                          
                          br(),
                          
                          h3("1.  Do home team statistics 'worsen' and away team statistics 'improve' when games are played behind closed doors without a crowd presence across 
                             the Top 5 European leagues"),
                          h3("2.  If so, does this/how does this differ across the top 5 European Leagues?"),
                          
                          hr(),
                          
                          h5(strong("References")),
                          h5("Courneya KS, Carron AV  (1992)  The Home Advantage in  Sport Competitions: A Literature Review. J  Sport Exercise Psy 14(1):13–27. https://doi.org/10.1123/jsep.14.1.13"),
                          h5("Ramchandani, G., & Millar, R. (2021). Investigating the “Twelfth Man” Effect in Five European Domestic Football Leagues: A COVID-19 Induced Natural Experiment. Journal of Global Sport Management, 1-15."),
                          
                          
                          
                 ),
                 
                 tabPanel("Interactive Graph",
                          icon = icon("chart-bar"),
                          
# Create two inset tabPanels, one for viewing individual leagues, one for viewing all 5 leagues on the same graph.
                          tabsetPanel(
                            tabPanel("View Data by Individual League",
                                     
# A SidebarPanel will be used to keep all user inputs on the opposing side to the main output panel.
                                     sidebarPanel(width = 3,
                                       
# Two drop-down menus will accept user input for both league and viewed statistic. 
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
                                       h4("Home/Away Clean Sheet statistics show the percentage of games in which teams did not concede a goal, expressed as a decimal."),
                                       h4("Goal difference is the mean goal difference per game. Goal Difference is calculated using  home goals - away goals. 
                                          As such, a positive number denotes more goals scored on average by home sides while a negative number denotes more goals scored on average
                                          by away sides."),
                                       
                                       br(),
                                       
                                       h4(strong("What is xG?")),
                                       h4("xG, or 'expected goals', is a measure between 0 and 1 assigned to shots made in a game of football and measures the probability that a shot
                                          will result in a goal, with the closer a shot's xG is to 1 the more likely that shot is to result in a goal."),
                                       h4("To calculate a shots allocated xG, it is compared to thousands of similar shots with similar characteristics, such as the location of the
                                          shooter and positions of defenders, and it is observed how many shots from similar positions resulted in goals."),
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
                                       h4("Home/Away Goals and Points, as well as xG statistics are a mean number per game."),
                                       h4("Home/Away Clean Sheet statistics show the percentage of games in which teams did not concede a goal, expressed as a decimal."),
                                       h4("Goal difference is the mean goal difference per game. Goal Difference is calculated using  home goals - away goals. 
                                          As such, a positive number denotes more goals scored on average by home sides while a negative number denotes more goals scored on average
                                          by away sides."),
                                       
                                       br(),
                                       
                                       h4(strong("What is xG?")),
                                       h4("xG, or 'expected goals', is a measure between 0 and 1 assigned to shots made in a game of football and measures the probability that a shot 
                                          will result in a goal, with the closer a shot's xG is to 1 the more likely that shot is to result in a goal."),
                                       h4("To calculate a shots allocated xG, it is compared to thousands of similar shots with similar characteristics, such as the location of the 
                                          shooter and positions of defenders, and it is observed how many shots from similar positions resulted in goals."),
                                       h4("For a more detailed explanation, visit" , a("FBRef's xG explained page.", href = "https://fbref.com/en/expected-goals-model-explained/"))
                                     ),
                                     
                                     mainPanel("",
                                               
                                               plotOutput("plot3", height = 850, width = 1325)
                                               
                                     )
                            )
                            
                         )
),

tabPanel("  Interpretation and Discussion",
         icon = icon("comment", lib = "glyphicon"),
         
         h2(strong("Interpretation and Discussion")),
         
         hr(),
         
         h4("Previous research and prior knowledge of home advantage tells us that we would expect crowdless games to have fewer home goals per game and greater away goals per game.
            This is exactly what we see when we view the combined data of the top 5 leagues."),
         
         hr(),
         
         h4(strong("Top 5 Leagues Combined: Home Goals per Game and Away Goals per Game")),
         
# Placing 2 plotOutputs in a fluidRow enables the user to view two rendered graphs side-by-side         
         fluidRow(
           column(
             width = 6,
             plotOutput("top5homegoals")
           ),
           column(
             width = 6,
             plotOutput("top5awaygoals")
           )
         ),
         
         hr(),
         
         h4("Home goals in a crowdless environment for all 5 leagues combined drops from ~1.55 goals per game to ~1.45 goals per games, whereas away goals increase more drastically 
            from ~1.2 goals per game to ~1.35 goals per game."),
         h4("This pattern is also followed for home and away xG and points per game, as well as clean sheets."),
         h4("Now lets examine how this breaks down across the individual leagues:"),

         hr(),
         
         h4(strong("All 5 Leagues Individually: Home Goals per Game and Away Goals per Game")),
         
         fluidRow(
           column(
             width = 6,
             plotOutput("all5homegoals")
           ),
           column(
             width = 6,
             plotOutput("all5awaygoals")
           )
         ),
         
         hr(),
         
         h4("The Bundesliga, Ligue 1 and LaLiga all follow the expected pattern, with home goals per game decreasing in the crowdless environment and away goals per game increasing.
            None more typifies this than the Bundesliga, where average goal difference per game swings from ~+0.25 to ~-0.25."),
         
         hr(),
         
         h4(strong("All 5 Leagues Individually: Goal Difference")),
         
         plotOutput("all5goaldiff"),
         
         hr(),
         
         h4("However, the remaining two leagues do not follow the expected pattern: The Premier League and Serie A."),
         h4("In the Premier League, home goals per game minimally increases after the postponement and away goals per game minimally decreases. Interestingly, home xG per game
            drops by ~0.1 during the crowdless environment period. This, taken with the slight increase in home goals, suggests that Premier League home teams were scoring more goals
            from harder chances when their own fans were not present, potentially suggesting that players could convert from harder chances when not subjected to the pressure of the
            home crowd. However, further analysis of this would need to be done to determine any psychological basis to this."),
         
         hr(),
         
         h4(strong("Premier League: Home Goals per Game and Home xG per Game")),
         
         fluidRow(
           column(
             width = 6,
             plotOutput("plhomegoals")
           ),
           column(
             width = 6,
             plotOutput("plhomexG")
           )
         ),
         
         hr(),
         
         h4("While on the topic of home xG, post postponement Ligue 1 home matches had almost identical average home goals per game and average xG per game. This is almost certainly
            a coincedence (although satisfying). "),
         
         hr(),
         
         h4(strong("Ligue 1: Home Goals per Game and Home xG per Game")),
         
         fluidRow(
           column(
             width = 6,
             plotOutput("lighomegoals")
           ),
           column(
             width = 6,
             plotOutput("lighomexG")
           )
         ),
         
         hr(),
         
         h4("However, Serie A statistic comparisons completely contradict all literature and previous observations. Mean home goals per game significantly increased from ~1.55 to
            ~1.75 following the restart without fans, and away goals per game also slightly increased from ~1.4 to ~1.45."),
         
         hr(),
         
         h4(strong("Serie A: Home Goals per Game and Away Goals per Game")),
         
         fluidRow(
           column(
             width = 6,
             plotOutput("serhomegoals")
           ),
           column(
             width = 6,
             plotOutput("serawaygoals")
           )
         ),
         
         hr(),
         
         h4("Average points per game for home teams increased from ~1.45 to ~1.55 whereas it decreased for away teams from ~1.35 to ~1.225. Mean goal difference per game changed from
            just over +0.135 to about +0.325."),
         
         hr(),
         
         h4(strong("Serie A: Goal Difference")),
         
         plotOutput("sergoaldiff"),
         
         hr(),
         
         h4("For as to why Serie A appears to flaunt convention, I believe that it is a victim of the relatively small sample of matches used in this analysis. Only about half
            a seasons worth of football was played without a crowd in Serie A, some extreme results can skew the data significantly and games like this did take place, such as
            Inter Milan 6-0 Brescia and Sassuolo 5-0 Genoa."),
),

tabPanel("Limitations and Future Implementation",
         icon = icon("forward", lib = "glyphicon"),
         
         h2(strong("Limitations and Future Implementation")),
         
         hr(),
         
         h4("As mentioned in the interpretation section, due to the nature of the events leading up to the Covid-19 football, the sample of crowdless matches to draw from is small.
         This leads to problems with using average statistics to compare leagues 
            as a few matches with extreme results can lead to skewed analysis. For example, whereas this analysis suggests that the absence of a crowd did not seem to affect Serie A,
            a study by Rovetta & Abate (2011) which used Welch's t-test found that 
            the average percentage of points collected by teams in home matches dropped by 8%. Additionally, this analysis only used matches with crowd presence from within the same
            season as those without crowd presence. This was to preserve as much as 
            possible a comparison of leagues which maintained mostly the same personnel and coaching staff, as well as the same teams not being lost to relegation/promotion. I think
            this is the right choice, although this means that both the sample for matches 
            pre and post Covid-19 postponement are relatively small."),
         h4("Additionally, Ligue 1 abandoned their 19/20 season instead of postponing it, and concluded the postions of teams based on a points per game coeffecient. This means that
         for the purposes of this analysis, I included the Ligue 1 20/21 season, which 
            took place entirely behind closed doors without a fan presence. However, teams were still promoted/relegated based on the 19/20 league postions, meaning that the Ligue 1
            crowd/crowdless comparison is perhaps a less fair one than that of other leagues in this analysis."),
         h4("In terms of future extensions to the project, if I had the time, I would like to have implemented a new tab in the interactive graph which gave a statistical comparison
         of refereeing statistics across the leagues. One of the factors often cited to 
            contribute to home advantage is referee bias towards home teams - the idea that that referees give more favourable decisions to home teams and are less likely to give
            punative measures to home players due to exterted social pressure from crowd noise. A meta-analysis 
            of studies investigating home advantage during the Covid-19 pandemic by Leitner et al. (2021) concluded that one of the major factors contributing to receeding home
            advantage was a reduced referee bias."),
         h4("Referee decision statistics were unfortunately unavailable due to the way I selected individual matches without attendance to export. Manually cataloging these decisions
         would be unfeasible, so a data scraper would need to be created in order to extract this information 
            into seperate datasets for crowdless and non-crowdless football."),
         h4("Another implementation I would have liked to have done is create a tab with a comparison of how individual teams' performance changed depending whether or not they had
         a crowd presence. This could have been done by transforming the wide data frame into a long-form data 
            frame using either the 'gather' function from the Tidyr package or 'melt' from the reshape2 package."),
         
         hr(),
         
         h5(strong("References")),
         h5("Leitner, M. C., Daumann, F., Follert, F., & Richlan, F. (2022). The cauldron has cooled down: a systematic literature review on home advantage in football during the
            COVID-19 pandemic from a socio-economic and psychological perspective. Management Review Quarterly, 1-29."),
         h5("Rovetta, A., & Abate, A. (2021). The impact of cheering on sports performance: comparison of serie a statistics before and during COVID-19. Cureus, 13(8)."),
),
                



                 
                 
                 tabPanel("Dataset Information",
                          icon = icon("info"),
                          
                         h2(strong("Dataset Information")),
                         
                         hr(),
                         
                         img(src = "https://miro.medium.com/max/1400/1*yDcbh8c5um2rI8dWMWU4-g.png", height = 450, width = 850, align = "right"),
                         
                         h3("Data for fixtures and results was exported from", a("FBRef.", href = "https://fbref.com/en/")),
                         h3("Data was extracted from the so called 'Big 5' leagues
                            across europe, which consists 5 top flight leagues across Europe:"),
                         
                         br(),
                         
                         h4("English Premier League"),
                         h4("Bundesliga"),
                         h4("Ligue 1 Uber Eats"),
                         h4("Serie A TIM"),
                         h4("LaLiga Santander"),
                         
                         br(),
                         
                         h3("These leagues, during the 19/20 season, had their fixtures suspended and at a later date resumed play behind closed doors, with the exception of the
                         French Ligue 1, who abandoned their 19/20 season before any crowd-less games took place and concluded league positions based on an average points-per-game
                         basis. As such, this analysis uses games from the 20/21 season which was played entirely behind closed doors.
")
                           ),
# Options for the ui, including a theme from the 'shinythemes' package.
                 collapsible = TRUE, fluid = TRUE, inverse = TRUE, theme = shinytheme("yeti"),
                 windowTitle = "Home Advantage and Covid-19: Football's Natural Experiment.",
                 position = "static-top")



############## Define server logic for the app. ############### 
server <- function(input, output, session) {

# Create a value using a reactive expression to be used with the ggplot graphs based on the users input for the selectizeInput for league selection in the ui.    
  user_leaguefilter <- reactive({
    
# Unless the user calls for the data for all 5 leagues combined, the data is subset for data with the correct variable called for in the user input for league.
# This is operated via an else if statement.
    if (input$user_league == "Top 5 Leagues Combined") {
      df <- big5data
    }
    
    else if (input$user_league != "Top 5 Combined") {
    df <- subset(big5data, big5data$league %in% input$user_league)
    }
  })
  

# The expression for a reactive plot that observes the user input for both league and statistic.
# Graphs are generated using the 'ggplot2' package.
  output$plot1 <- renderPlot({
    
    active_plot <- ggplot(user_leaguefilter(), aes_string(x="preduring", y=input$user_statistic, fill = "preduring")) +
# Statistic = summary and fun = mean are used to generate average stats per game.      
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+ 
# Blue and Orange are the two colours used throughout this app. This is chosen to be as colourblind-friendly a palette as possible.
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
# A series of else if statements allow the correct y axis limits to be set depending on what type of statistic is chosen.
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
# ggsave saves the graph to the 'figs' folder located in the root folder as a png, with a name combined of both the league and statistic selected by the user.
    ggsave(filename = paste(input$user_league, input$user_statistic, ".png", sep = ""), path = here("figs"))
    
    
    
  })

# Another reactive plot to view all leagues on a combined graph. This is done by having league on the x axis, with pre/post Covid-19 postponements as a fill column.
  output$plot3 <- renderPlot({
    
    overview_plot <- ggplot(big5data, aes_string(x = "league", y = input$user_statistic_ov, fill = "preduring"))+
      geom_bar(stat = "summary", fun = "mean",
               width = 0.7,
               position = position_dodge2(0.5, reverse = TRUE))+
      scale_x_discrete(labels = c("premier league" = "Premier League", "bundesliga" = "Bundesliga", "ligue 1" = "Ligue 1", "serie a" = "Serie A", "laliga" = "LaLiga"), 
                       limits = c("premier league", "bundesliga", "ligue 1", "serie a", "laliga"))+
      theme_minimal()+
      scale_fill_manual(breaks = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"),
                        values=c("dodgerblue2",
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
  
  output$top5homegoals <- renderPlot({
    
    top5homegoals <- ggplot(big5data, aes_string(x="preduring", y="homegoals", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    top5homegoals <- top5homegoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(top5homegoals)
    
  })
  
  output$top5awaygoals <- renderPlot({
    
    top5awaygoals <- ggplot(big5data, aes_string(x="preduring", y="awaygoals", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    top5awaygoals <- top5awaygoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(top5awaygoals)
    
  })
  
  output$plhomegoals <- renderPlot({
    
    plhomegoals <- ggplot(plsubset, aes_string(x="preduring", y="homegoals", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    plhomegoals <- plhomegoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(plhomegoals)
    
  })
  
  output$plhomexG <- renderPlot({
    
    plhomexG <- ggplot(plsubset, aes_string(x="preduring", y="homexg", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    plhomexG <- plhomexG+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(plhomexG)
    
  })
  
  output$lighomegoals <- renderPlot({
    
    lighomegoals <- ggplot(ligsubset, aes_string(x="preduring", y="homegoals", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    lighomegoals <- lighomegoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(lighomegoals)
    
  })
  
  output$lighomexG <- renderPlot({
    
    lighomexg <- ggplot(ligsubset, aes_string(x="preduring", y="homexg", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    lighomexg <- lighomexg+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(lighomexg)
    
  })
  
  output$serhomegoals <- renderPlot({
    
    serhomegoals <- ggplot(sersubset, aes_string(x="preduring", y="homegoals", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    serhomegoals <- serhomegoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(serhomegoals)
    
  })
  
  output$serawaygoals <- renderPlot({
    
    serawaygoals <- ggplot(sersubset, aes_string(x="preduring", y="awaygoals", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    serawaygoals <- serawaygoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    
    print(serawaygoals)
    
  })
  
  output$sergoaldiff <- renderPlot({
    
    sergoaldiff <- ggplot(sersubset, aes_string(x="preduring", y="goaldiff", fill = "preduring")) +
      geom_bar(stat = "summary", fun = "mean",
               width = 0.35)+
      theme_minimal()+
      scale_x_discrete(limits = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"))+
      labs(x = " ", y = " ")+
      scale_fill_manual(values=c("orange2",
                                 "dodgerblue2"))+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))
    
    
    sergoaldiff <- sergoaldiff+
      coord_cartesian(ylim = c(-0.5, 0.5))+
      geom_hline(yintercept = 0)
    
    
    print(sergoaldiff)
    
  })
  
  output$all5homegoals <- renderPlot({
    
    all5homegoals <- ggplot(big5data, aes_string(x = "league", y = "homegoals", fill = "preduring"))+
      geom_bar(stat = "summary", fun = "mean",
               width = 0.7,
               position = position_dodge2(0.5, reverse = TRUE))+
      scale_x_discrete(labels = c("premier league" = "Premier League", "bundesliga" = "Bundesliga", "ligue 1" = "Ligue 1", "serie a" = "Serie A", "laliga" = "LaLiga"), 
                       limits = c("premier league", "bundesliga", "ligue 1", "serie a", "laliga"))+
      theme_minimal()+
      scale_fill_manual(breaks = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"),
                        values=c("dodgerblue2",
                                 "orange2"))+
      labs(x = " ", y = " ")+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 16),
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))+
      labs(fill="Pre/Post Covid-19")
    
    all5homegoals <- all5homegoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    print(all5homegoals)
    
  })
  
  output$all5awaygoals <- renderPlot({
    
    all5awaygoals <- ggplot(big5data, aes_string(x = "league", y = "awaygoals", fill = "preduring"))+
      geom_bar(stat = "summary", fun = "mean",
               width = 0.7,
               position = position_dodge2(0.5, reverse = TRUE))+
      scale_x_discrete(labels = c("premier league" = "Premier League", "bundesliga" = "Bundesliga", "ligue 1" = "Ligue 1", "serie a" = "Serie A", "laliga" = "LaLiga"), 
                       limits = c("premier league", "bundesliga", "ligue 1", "serie a", "laliga"))+
      theme_minimal()+
      scale_fill_manual(breaks = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"),
                        values=c("dodgerblue2",
                                 "orange2"))+
      labs(x = " ", y = " ")+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 16),
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))+
      labs(fill="Pre/Post Covid-19")
    
    all5awaygoals <- all5awaygoals+
      coord_cartesian(ylim = c(0.7, 1.8))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    print(all5awaygoals)
    
  })

# In order to render plots for the interpretation section, a battery of plotOutputs are rendered for each output.   

    output$all5goaldiff <- renderPlot({
    
    all5goaldiff <- ggplot(big5data, aes_string(x = "league", y = "goaldiff", fill = "preduring"))+
      geom_bar(stat = "summary", fun = "mean",
               width = 0.7,
               position = position_dodge2(0.5, reverse = TRUE))+
      scale_x_discrete(labels = c("premier league" = "Premier League", "bundesliga" = "Bundesliga", "ligue 1" = "Ligue 1", "serie a" = "Serie A", "laliga" = "LaLiga"), 
                       limits = c("premier league", "bundesliga", "ligue 1", "serie a", "laliga"))+
      theme_minimal()+
      scale_fill_manual(breaks = c("Pre Covid-19 Postponement", "Post Covid-19 Postponement"),
                        values=c("dodgerblue2",
                                 "orange2"))+
      labs(x = " ", y = " ")+
      theme(axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 16),
            panel.border = element_rect(colour = "grey", fill=NA, size=0.01))+
      labs(fill="Pre/Post Covid-19")
    
    all5goaldiff <- all5goaldiff+
      coord_cartesian(ylim = c(-0.5, 0.5))+
      geom_hline(yintercept = 0)
    
    print(all5goaldiff)
    
  })
    
}

# Finally, use the 'shinyApp' function to create the Shiny App object from the ui and server.
shinyApp(ui, server)        


