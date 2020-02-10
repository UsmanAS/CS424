#Usman Siddiqui CS 424 Project 1

#Libraries to use

library(lubridate)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(fingerprint)
library(shiny)
library(shinydashboard)
library(rsconnect)

#Read in data
litter <- read.table(file = "litterati challenge-65.csv", TRUE, ",")

#Change timestamp into a better timestamp for R and change timezone to Chicago
newTime <- ymd_hms(litter$litterTimestamp)
newTime <- with_tz(newTime, "America/Chicago")
litter$TimeStamp <- newTime
litter$litterTimestamp <- NULL

#Change users without usernames to their userID a nd reinsert data
fixeduser <- sub("litterati-", "", litter$username)
litter$username <- fixeduser

#Recreate the table
str(litter)

#Count the number of entries per user and CountTop contains the top 10 users
countUsers <- plyr::count(litter, vars = "username")
CountTop <- arrange(countUsers,desc(countUsers$freq))
CountTop <- top_n(CountTop, 10)

#Fix the tags making the blank tags as untagged
fixedTags <- litter$tags
fixedTags <- as.character(fixedTags)
fixedTags[fixedTags== ""] <- "untagged"
litter$tags <- fixedTags

#Recreate the table with fixed tags
str(litter)

#Count the total number of litter picked up
totalPickup <- nrow(litter)


#Make a chart of litter picked up each day
pickUpDate <- as.Date(litter$TimeStamp)
Days <- table(cut(pickUpDate, 'day'))
PickUpDateChart <- data.frame(Date=format(as.Date(names(Days)), '%Y-%m-%d'), Frequency = as.vector(Days))

#Change data to be litter picked up each weekday
WeekdayPickup <- weekdays(pickUpDate)

#Change factor and order of Weekdays going from Sunday-Saturday
WeekdayPickup <- factor(WeekdayPickup, levels = 
                          c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
WeekdayPickup[order(WeekdayPickup)]

#Make a chart of the data of litter picked each weekday
WeekdayTablePickup <- table(WeekdayPickup)
WeekdayTablePickup <- data.frame(WeekdayTablePickup)

#Make a chart of the litter picked up per hour
pickUpTimeHour <- hour(litter$TimeStamp)
pickUpTimeHourTable <- table(pickUpTimeHour)
pickUpTimeHourTable <- data.frame(pickUpTimeHourTable)

#Change tags to be in a table per each tag
TagTable <- unlist(strsplit(litter$tags, split = ","))
TagTable <- unlist(strsplit(TagTable, split = " "))
TagTable <- table(TagTable)
TagTable <- data.frame(TagTable)

#Total number of each tag generated
TTF <- TagTable$Freq

#Tags arranged in descending order and then chagned to the top 10
TagTableTop <- arrange(TagTable, desc(TTF))
TagTableTop <- top_n(TagTableTop, 10)

#Frequency of each tag for the Top 10
TTTF <- TagTableTop$Freq

users <- CountTop$username
users <- append(users, "None")

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
    
    #selectInput("stuff", "Select the year to visualize", years, selected = 2019),
    selectInput("username", "Select the Data to visualize", users, selected = "None"),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "About")
    
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName = "dashboard",
       fluidRow(
               box(title = "Each Day Pick Up", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist0", height = 400)
               ),
               
               box(title = "Each Weekday Pick Up", solidHeader = TRUE, status = "primary", width = 6,
                   plotOutput("hist1", height = 400)
               ),
               
               box(title = "Each Hour Pickup", solidHeader = TRUE, status = "primary", width = 6,
                   plotOutput("hist2", height = 400)
               ),
               box(title = "Each Tag Pick Up", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist3", height = 400)
               ),
               
               box(title = "Noon Temps as Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("tab1")
               )
               
             
        )
      ),
      tabItem(tabName = "About",
            h2("About???")
      )
    )
  )
)

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 
  

    output$hist0 <- renderPlot({
      
        #Make a plot of the litter by day
        if(input$username == "None")  {
          ggplot(litter, aes(pickUpDate, frequency(pickUpDate))) +
            geom_bar(fill = "black", stat = "identity") + 
            labs(x = "Day", y = "Amount")
        }
        
        else{
          pickUpDateUser <- as.Date(litter$TimeStamp[litter$username == input$username])
          ggplot(litter[litter$username == input$username,], aes( x = pickUpDateUser, frequency(pickUpDateUser))) +
            geom_bar(fill = "black", stat = "identity") + 
            labs( x = "Day", y = "Amount")
        }
      
    })
  
  
    # show all of the temperatures for a given room for a given year
    output$hist1 <- renderPlot({
    
        if( input$username == "None") {
          #Plot the data for litter picked by each weekday
          ggplot(litter, aes(WeekdayPickup, frequency(WeekdayPickup))) +
            geom_bar(fill = "black", stat = "identity") +
            labs(x = "Weekday", y = "Amount")
        }
      
        else  {
          pickUpDateUser <- as.Date(litter$TimeStamp[litter$username == input$username])
          WeekdayPickupUser <- weekdays(pickUpDateUser)
          ggplot(litter[litter$username == input$username,], aes(WeekdayPickupUser, frequency(WeekdayPickupUser))) +
            geom_bar(fill = "black", stat = "identity") +
            labs(x = "Weekday", y = "Amount")
          
        }
    
    })
  
  
    # show a line graph of the temperatures at noon for a given room for a given year
    output$hist2 <- renderPlot({
      
        if( input$username == "None"){
          #Plot the litter picked up per hour
          ggplot(litter, aes(pickUpTimeHour, frequency(pickUpTimeHour))) +
            geom_bar(fill = "black", stat = "identity") +
            labs( x = "Hour", y = "Amount")
        }
      
        else  {
          pickUpTimeHourUser <- hour(litter$TimeStamp[litter$username == input$username])
          ggplot(litter[litter$username == input$username,], aes(pickUpTimeHourUser, frequency(pickUpTimeHourUser))) +
            geom_bar(fill = "black", stat = "identity") +
            labs( x = "Hour", y = "Amount")
        }
    })
  
  

  
    # show box plot of the temperatures at noon for a given room for a given year
    output$hist3 <- renderPlot({
    
      if( input$username == "None") {
        #Plot the Top 10 tags
        ggplot(TagTableTop, aes(TagTable, TTTF)) +
          geom_bar(fill = "black", stat = "identity") +
          labs(x = "Tag", y = "Amount")
      }
      
      else  {
        
        TagTableUser <- unlist(strsplit(litter$tags[litter$username == input$username], split = ","))
        TagTableUser <- unlist(strsplit(TagTableUser, split = " "))
        TagTableUser <- table(TagTableUser)
        TagTableUser <- data.frame(TagTableUser)
        TagTableUser$TagTable <- TagTableUser$TagTableUser
        TagTableUser$TagTableUser <- NULL
        TagTableUserTop <- merge(TagTableUser,TagTableTop, by="TagTable")
        TTUF <- TagTableUserTop$Freq.x
        ggplot(TagTableUserTop, aes(TagTableUserTop$TagTable, TTUF)) +
          geom_bar(fill = "black", stat = "identity") +
          labs(x = "Tag", y = "Amount")
        
      }
    })
    # use DT to help out with the tables - https://datatables.net/reference/option/
    output$tab1 <- DT::renderDataTable(
      DT::datatable({ 
        litter
      }, 
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
      )
    )
  }

    
#Launch Shiny
shinyApp(ui = ui, server = server)

