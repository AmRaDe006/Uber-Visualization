#Web app interface

#Uber Data Analysis

# LOADING STUFF ###############################################################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, shiny, shinythemes, shinyWidgets)
pacman::p_load(ggplot2, ggthemes, lubridate,
               dplyr, tidyr, DT, scales)
pacman::p_load(wesanderson)
pacman::p_load(leaflet, magrittr)

# LOADING DATA ################################################################

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
pal <- wes_palette("Zissou1", 100, type = "continuous")

apr_data <- read.csv("../Uber-dataset/uber-raw-data-apr14.csv")
may_data <- read.csv("../Uber-dataset/uber-raw-data-may14.csv")
jun_data <- read.csv("../Uber-dataset/uber-raw-data-jun14.csv")
jul_data <- read.csv("../Uber-dataset/uber-raw-data-jul14.csv")
aug_data <- read.csv("../Uber-dataset/uber-raw-data-aug14.csv")
sep_data <- read.csv("../Uber-dataset/uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

#summary(data_2014)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time,
                                  format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time,
                                    format="%m/%d/%Y %H:%M:%S"),
                         format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label=TRUE))
data_2014$year = factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label=TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total=n())

hist_x_data_d <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

month_day <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total=n())

day_vs_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total=n())

month_vs_day <- data_2014 %>%
  group_by(day, month) %>%
  dplyr::summarize(Total=n())

month_vs_base <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total=n())

dayofweek_vs_base <- data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

Lon <- data_2014$Lon
Lat <- data_2014$Lat
Base <- data_2014$Base
Month <- data_2014$month

Pos_data <- data.frame(base=Base, lat = Lat, lon = Lon, month = Month)



# UI DESIGN #################################################################

ui <- fluidPage(theme = shinytheme("united"),
                
                titlePanel("Uber Data Analysis"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    conditionalPanel(
                      'input.graphGroup === "Histograms"',
                      helpText("Will decide the plot axis here"),
                      selectInput("histx", "Choose X axis:",
                                  c("Hour of the Day" = "hour", "Day" = "day", "Month" = "month", "Base of Operation" = "Base")
                                  )
                    ),
                    
                    conditionalPanel(
                      'input.graphGroup === "HeatMaps"',
                      helpText("Will decide the heatmap plot axes here"),
                      radioButtons("heat", "Relation between:",
                                   c("Hour-Day",
                                     "Month-Day",
                                     "Month-Week",
                                     "Month-Base",
                                     "Week-Base"))
                    ),
                    
                    conditionalPanel(
                      'input.graphGroup === "GeoPlot"',
                      helpText("Please note that this may take a considerable amount of time to process"),
                      helpText("NYC: 39 - 43 Latitude, -75 - -72 Longitude"),
                      numericRangeInput(
                        inputId="LatRange", label = h3("Latitude Range"), 
                        value= c(40.705, 40.715),
                        separator= "  -  "
                      ),
                      numericRangeInput(
                        inputId="LonRange", label = h3("Longitude Range"), 
                        value= c(-74.005, -73.995),
                        separator= " - "
                      ),
                      selectInput(
                        "baseName", h3("Choose Base filtering:"),
                        c("Unter LLC" = "B02512", "CMS Limo Inc" = "B02598", "Weiter LLC" = "B02617", "Schmecken LLC" = "B02682", "Danach-NY, LLC" = "B02764")
                      ),
                      radioButtons(
                        "monthFilter", h3("Choose Month filtering:"),
                        c(
                          "April"="Apr",
                          "May"="May",
                          "June"="Jun",
                          "July"="Jul",
                          "August"="Aug",
                          "September"="Sep"
                        )
                      )
                    )
                  ),
                  
                  mainPanel(
                    
                    tabsetPanel(
                      id = 'graphGroup',
                      tabPanel("Histograms",
                               
                               DT::dataTableOutput("table"),
                               plotOutput(outputId = "hist_graph")
                               
                               ),
                      
                      tabPanel("HeatMaps",
                               
                               plotOutput(outputId = "heat_map")
                               
                               ),
                      
                      tabPanel("GeoPlot",
                               
                               tags$style(type = "text/css", "#geo_plot {height: calc(100vh - 120px) !important;}"),
                               
                               leafletOutput(outputId = "geo_plot")
                               
                               )
                      
                    )
                  )
                )
                
                
                
)

# SERVER DESIGN ###############################################################

server <- function(input, output){
  
  # HISTOGRAMS #
  
  output$table <- DT::renderDataTable(DT::datatable({

    if(input$histx == "hour"){
      hist_x_data <- data_2014 %>%
        group_by(hour, month) %>%
        dplyr::summarize(Total = n())
    }

    if(input$histx == "day"){
      hist_x_data <- data_2014 %>%
        group_by(day, month) %>%
        dplyr::summarize(Total = n())
    }

    if(input$histx == "month"){
      hist_x_data <- data_2014 %>%
        group_by(month, dayofweek) %>%
        dplyr::summarize(Total = n())
    }

    if(input$histx == "Base"){
      hist_x_data <- data_2014 %>%
        group_by(Base, month) %>%
        dplyr::summarize(Total = n())
    }
    
    hist_x_data
  }))
  
  
  output$hist_graph <- renderPlot({
    
    if(input$histx == "hour"){
      ggplot(month_hour, aes(hour, Total, fill=month))+
        geom_bar(stat="identity")+
        ggtitle("Trips each Hour - Months inclusive")+
        xlab("Hours")+
        ylab("Trips")+
        labs(fill = "Month")+
        scale_y_continuous(labels=comma)
    }
    else
    if(input$histx == "day"){
      ggplot(hist_x_data_d, aes(day, Total, fill = month))+
        geom_bar(stat="identity", position="stack")+
        ggtitle("Trips each Week-Day - Months inclusive")+
        xlab("Day")+
        ylab("Trips")+
        labs(fill = "Days")+
        scale_y_continuous(label=comma)
    }
    else
    if(input$histx == "month"){
      ggplot(month_day, aes(month, Total, fill=dayofweek))+
        geom_bar(stat="identity", position="dodge")+
        ggtitle("Trips each Week-Day - Months inclusive")+
        xlab("Day")+
        ylab("Trips")+
        labs(fill = "Day of Week")+
        scale_y_continuous(label=comma)+
        scale_fill_manual(values=colors)
    }
    else
    if(input$histx == "Base"){
      ggplot(data_2014, aes(Base, fill = month))+
        geom_bar(position = "dodge")+
        scale_y_continuous(labels=comma)+
        ggtitle("Trips each Base - Months inclusive")+
        scale_fill_manual(values = colors)+
        ylab("Trips")+
        labs(fill = "Months")
    }
    
  })
  
  # HEATMAPS #
  
  output$heat_map <-renderPlot({
    
    if(input$heat == "Hour-Day"){
      ggplot(day_vs_hour, aes(day, hour, fill = Total))+
        geom_tile(color = "black")+
        scale_fill_gradientn(colours = pal)+
        ggtitle("Heat Map: Hour vs Day")+
        xlab("Day")+
        ylab("Hour")+
        labs(fill = "Trips")
    }
    else
      if(input$heat == "Month-Day"){
        ggplot(month_vs_day, aes(day, month, fill = Total))+
          geom_tile(color="black")+
          scale_fill_gradientn(colours = pal)+
          ggtitle("Heat Map: Month vs Day")+
          xlab("Day")+
          ylab("Month")+
          labs(fill = "Trips")
      }
    else
      if(input$heat == "Month-Week"){
        ggplot(month_day, aes(dayofweek, month, fill = Total))+
          geom_tile(color = "black")+
          scale_fill_gradientn(colours = pal)+
          ggtitle("Heat Map: Month vs Day of Week")+
          xlab("Day of Week")+
          ylab("Month")+
          labs(fill = "Trips")
      }
    else
      if(input$heat == "Month-Base"){
        ggplot(month_vs_base, aes(Base, month, fill = Total))+
          geom_tile(color = "black")+
          scale_fill_gradientn(colours = pal)+
          ggtitle("Heat Map: Month vs Base")+
          xlab("Base")+
          ylab("Month")+
          labs(fill = "Trips")
      }
    else
      if(input$heat == "Week-Base"){
        ggplot(dayofweek_vs_base, aes(Base, dayofweek, fill = Total))+
          geom_tile(color = "black")+
          scale_fill_gradientn(colours = pal)+
          ggtitle("Heat Map: Dy of Week vs Base")+
          xlab("Base")+
          ylab("Day of Week")+
          labs(fill = "Trips")
      }
    
  })
  
  # GEOPLOTS #
  
  output$geo_plot <- renderLeaflet({
    
    Pos_d1 <- subset(Pos_data, lat >= input$LatRange[1])
    Pos_d2 <- subset(Pos_d1, lat <= input$LatRange[2])
    Pos_d3 <- subset(Pos_d2, lon >= input$LonRange[1])
    Pos_d4 <- subset(Pos_d3, lon <= input$LonRange[2])
    
    Pos_d0 <- subset(Pos_d4, month == input$monthFilter)
    
    Pos_d <- subset(Pos_d0, base == input$baseName)
    
    Pos_f <- data.frame(lon = Pos_d$lon, lat = Pos_d$lat)
    
    mid_lat <- (input$LatRange[1] + input$LatRange[2])/2
    mid_lon <- (input$LonRange[1] + input$LonRange[2])/2
    
    leaflet() %>%
      addTiles() %>%
      setView(mid_lon, mid_lat, zoom = 16) %>%
      addMarkers(data = Pos_f)
    
  })
  
}

# APP RUN ###################################################################

shinyApp(ui = ui, server = server)


