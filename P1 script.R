#Usman Siddiqui CS 424 Project 1

#Libraries to use
#Install all these packages
library(lubridate)
library(leaflet)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(fingerprint)
library(shiny)
library(shinydashboard)
library(DT)
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
topTags <- as.character(TagTableTop$TagTable)
topTags <- append(topTags, "None")

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
    
    selectInput("tag", "Select the tag to visualize", topTags, selected = "None"),
    selectInput("username", "Select the Data to visualize", users, selected = "None"),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "About")
    
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName = "dashboard",
               fluidRow(
                 box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                     leafletOutput("leaf", height = 800)
                 )
               ),
               h2("Total Pickup: ", totalPickup),
            
               
                 
               fluidRow(
                 
                 box(title = "Top 10 pickers", solidHeader = TRUE, status = "primary", width = 12,
                     dataTableOutput("tab0", height = 400)
                 )
               ),
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
               )
             
        ),
      fluidRow(
        
        box(title = "Litter picked up each Day", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("tab1", height = 400)
        ),
        
        box(title = "Litter picked up each Weekday", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("tab2", height = 400)
        ),
        
        box(title = "Litter picked up each Hour", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("tab3", height = 400)
        ),
        
        box(title = "Top 10 Tags", solidHeader = TRUE, status = "primary", width = 12,
            dataTableOutput("tab4", height = 400)
        )
        
        
        
        
        
      )
      ),
      tabItem(tabName = "About",
            h2("This Project was created by Usman Siddiqui and the data is from littereli.")
      )
    )
  )
)

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 
  

    output$hist0 <- renderPlot({
      
        #Make a plot of the litter by day
        if(input$username == "None" && input$tag == "None")  {
          ggplot(litter, aes(pickUpDate, frequency(pickUpDate))) +
            geom_bar(fill = "black", stat = "identity") + 
            labs(x = "Day", y = "Amount")
        }
        
        else if( input$username != "None" && input$tag == "None")  {
          pickUpDateUser <- as.Date(litter$TimeStamp[litter$username == input$username])
          ggplot(litter[litter$username == input$username,], aes( x = pickUpDateUser, frequency(pickUpDateUser))) +
            geom_bar(fill = "black", stat = "identity") + 
            labs( x = "Day", y = "Amount")
        }
        
        else  {
          
          pickUpDateTag <- as.Date(litter$TimeStamp[litter$tags == input$tag])
          ggplot(litter[litter$tags == input$tag,], aes( x = pickUpDateTag, frequency(pickUpDateTag))) +
            geom_bar(fill = "black", stat = "identity") + 
            labs( x = "Day", y = "Amount")
        }
      
    })
  
  
    # show all of the temperatures for a given room for a given year
    output$hist1 <- renderPlot({
    
        if( input$username == "None" && input$tag == "None") {
          #Plot the data for litter picked by each weekday
          ggplot(litter, aes(WeekdayPickup, frequency(WeekdayPickup))) +
            geom_bar(fill = "black", stat = "identity") +
            labs(x = "Weekday", y = "Amount")
        }
      
        else if( input$username != "None" && input$tag == "None") {
          pickUpDateUser <- as.Date(litter$TimeStamp[litter$username == input$username])
          WeekdayPickupUser <- weekdays(pickUpDateUser)
          
          WeekdayPickupUser <- factor(WeekdayPickupUser, levels = 
                                    c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
          WeekdayPickupUser[order(WeekdayPickupUser)]
          ggplot(litter[litter$username == input$username,], aes(WeekdayPickupUser, frequency(WeekdayPickupUser))) +
            geom_bar(fill = "black", stat = "identity") +
            labs(x = "Weekday", y = "Amount")
          
        }
        else {
          
          pickUpDateTag <- as.Date(litter$TimeStamp[litter$tags == input$tag])
          WeekdayPickupTag <- weekdays(pickUpDateTag)
          
          WeekdayPickupTag <- factor(WeekdayPickupTag, levels = 
                                        c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
          WeekdayPickupTag[order(WeekdayPickupTag)]
          ggplot(litter[litter$tags == input$tag,], aes(WeekdayPickupTag, frequency(WeekdayPickupTag))) +
            geom_bar(fill = "black", stat = "identity") +
            labs(x = "Weekday", y = "Amount")
        }
        
    
    })
  
  
    # show a line graph of the temperatures at noon for a given room for a given year
    output$hist2 <- renderPlot({
      
        if( input$username == "None" && input$tag == "None"){
          #Plot the litter picked up per hour
          ggplot(litter, aes(pickUpTimeHour, frequency(pickUpTimeHour))) +
            geom_bar(fill = "black", stat = "identity") +
            labs( x = "Hour", y = "Amount")
        }
      
        else if( input$username != "None" && input$tag == "None")  {
          
          pickUpTimeHourUser <- hour(litter$TimeStamp[litter$username == input$username])
          ggplot(litter[litter$username == input$username,], aes(pickUpTimeHourUser, frequency(pickUpTimeHourUser))) +
            geom_bar(fill = "black", stat = "identity") +
            labs( x = "Hour", y = "Amount")
        }
        else  {
          
          pickUpTimeHourTag <- hour(litter$TimeStamp[litter$tags == input$tag])
          ggplot(litter[litter$tags == input$tag,], aes(pickUpTimeHourTag, frequency(pickUpTimeHourTag))) +
            geom_bar(fill = "black", stat = "identity") +
            labs( x = "Hour", y = "Amount")
        }
    })
  
  

  
    # show box plot of the temperatures at noon for a given room for a given year
    output$hist3 <- renderPlot({
    
      if( input$username == "None" && input$tag == "None") {
        #Plot the Top 10 tags
        ggplot(TagTableTop, aes(TagTable, TTTF)) +
          geom_bar(fill = "black", stat = "identity") +
          labs(x = "Tag", y = "Amount")
      }
      
      else if( input$username != "None" && input$tag == "None") {
        
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
      
      else  {
        
        TagTableTag <- unlist(strsplit(litter$tags, split = ","))
        TagTableTag <- unlist(strsplit(TagTableTag, split = " "))
        TagTableTag <- table(TagTableTag)
        TagTableTag <- data.frame(TagTableTag)
        TagTableTag$TagTable <- TagTableTag$TagTableTag
        TagTableTag$TagTableTag <- NULL
        TagTableTagTop <- merge(TagTableTag,TagTableTop, by="TagTable")
        TagTableTagTop <- TagTableTagTop[TagTableTagTop$TagTable == input$tag,]
        T3F <- TagTableTagTop$Freq.x
        ggplot(TagTableTagTop, aes(TagTableTagTop$TagTable, T3F)) +
          geom_bar(fill = "black", stat = "identity") +
          labs(x = "Tag", y = "Amount")
      }
    })
    # use DT to help out with the tables - https://datatables.net/reference/option/
    output$tab0 <- DT::renderDataTable(
      DT::datatable({ 
        CountTop
      }, 
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
      )
    )
    output$tab1 <- DT::renderDataTable(
      DT::datatable({ 
        if( input$username == "None" && input$tag == "None") {
          PickUpDateChart
        }
        
        else if( input$username != "None") {
          
          pickUpDateUser <- as.Date(litter$TimeStamp[litter$username == input$username])
          DaysU <- table(cut(pickUpDateUser, 'day'))
          PickUpDateChartUser <- data.frame(Date=format(as.Date(names(DaysU)), '%Y-%m-%d'), Frequency = as.vector(DaysU))
        }
        
        else  {
          
          pickUpDateTag <- as.Date(litter$TimeStamp[litter$tags == input$tag])
          DaysT <- table(cut(pickUpDateTag, 'day'))
          PickUpDateChartTag <- data.frame(Date=format(as.Date(names(DaysT)), '%Y-%m-%d'), Frequency = as.vector(DaysT))
          
        }
      }, 
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
      )
    )
    output$tab2 <- DT::renderDataTable(
      DT::datatable({ 
        if( input$username == "None" && input$tag == "None") {
          WeekdayTablePickup
        }
        else if( input$username != "None")  {
          
          pickUpDateUser <- as.Date(litter$TimeStamp[litter$username == input$username])
          WeekdayPickupUser <- weekdays(pickUpDateUser)
          WeekdayTablePickupUser <- table(WeekdayPickupUser)
          WeekdayTablePickupUser <- data.frame(WeekdayTablePickupUser)
        }
        
        else  {
          
          pickUpDateTag <- as.Date(litter$TimeStamp[litter$tags == input$tag])
          WeekdayPickupTag <- weekdays(pickUpDateTag)
          WeekdayTablePickupTag <- table(WeekdayPickupTag)
          WeekdayTablePickupTag <- data.frame(WeekdayTablePickupTag)
          
        }
      }, 
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
      )
    )
    output$tab3 <- DT::renderDataTable(
      DT::datatable({ 
        if( input$username == "None" && input$tag == "None") {
          pickUpTimeHourTable
        }
        else if( input$username != "None") {
          
          pickUpTimeHourUser <- hour(litter$TimeStamp[litter$username == input$username])
          pickUpTimeHourTableUser <- table(pickUpTimeHourUser)
          pickUpTimeHourTableUser <- data.frame(pickUpTimeHourTableUser)
        }
        
        else  {
          
          pickUpTimeHourTag <- hour(litter$TimeStamp[litter$tags == input$tag])
          pickUpTimeHourTableTag <- table(pickUpTimeHourTag)
          pickUpTimeHourTableTag <- data.frame(pickUpTimeHourTableTag)
          
        }
      }, 
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
      )
    )
    output$tab4 <- DT::renderDataTable(
      DT::datatable({ 
        if( input$username == "None" && input$tag == "None") {
          TagTableTop
        }
        else if( input$username != "None") {
          
          TagTableUser <- unlist(strsplit(litter$tags[litter$username == input$username], split = ","))
          TagTableUser <- unlist(strsplit(TagTableUser, split = " "))
          TagTableUser <- table(TagTableUser)
          TagTableUser <- data.frame(TagTableUser)
          TagTableUser$TagTable <- TagTableUser$TagTableUser
          TagTableUser$TagTableUser <- NULL
          TagTableUserTop <- merge(TagTableUser,TagTableTop, by="TagTable")
          TagTableUserTop$Freq.y <- NULL
          TagTableUserTop
        }
        
        else  {
          
          
          TagTableTag <- unlist(strsplit(litter$tags, split = ","))
          TagTableTag <- unlist(strsplit(TagTableTag, split = " "))
          TagTableTag <- table(TagTableTag)
          TagTableTag <- data.frame(TagTableTag)
          TagTableTag$TagTable <- TagTableTag$TagTableTag
          TagTableTag$TagTableTag <- NULL
          TagTableTagTop <- merge(TagTableTag,TagTableTop, by="TagTable")
          TagTableTagTop$Freq.y <- NULL
          TagTableTagTop <- TagTableTagTop[TagTableTagTop$TagTable == input$tag,]
        }
      }, 
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
      )
    )
    output$leaf <- renderLeaflet({
      if( input$username == "None" && input$tag == "None")  {
        TagName <- litter$tags
        map <- leaflet( data = litter)
      }
      
      else if ( input$username != "None") {
        
        dataUser <- litter[litter$username == input$username,]
        TagName <- dataUser$tags
        map <- leaflet( data = DataUser)
      }
      
      else  {
        
        dataTags <- litter[litter$tags == input$tag,]
        TagName <- dataTags$tags
        map <- leaflet( data = dataTags)
      }
      map <- addTiles(map)
      map <- setView(map, lng = -87.647998, lat = 41.870, zoom = 12)
      map <- addMarkers(map, lng = ~lon, lat = ~lat, popup = TagName,
                        clusterOptions = markerClusterOptions())
      map
    })
  }

    
#Launch Shiny
shinyApp(ui = ui, server = server)