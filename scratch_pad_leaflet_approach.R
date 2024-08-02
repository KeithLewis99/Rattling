
library(shiny)
library(xts)
library(leaflet)
library(dplyr)
source("../Rcode/scratchPad.R")

tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"


# make data ----
## https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts
date <- seq(as.Date("2015-01-01"), as.Date("2015-01-10"), by="day")
a <- xts(1:10,order.by=date)
df = data.frame(Lat = rnorm(10, 49, 0.1), Long = rnorm(10, -55, 0.1),Id=a)
str(df)
data_a<-data.frame(a)
data_a1 <- data_a %>%  
   #mutate("Lat" =as.numeric(df[1,1]),"Long"=as.numeric(df[2,1]),"Date"=rownames(data_a))
   mutate("Lat" = as.numeric(df[,1]),"Long"=as.numeric(df[,2]),"Date"=rownames(data_a))





# sample ----   
ui <- fluidPage(
      sliderInput("time", "date",
                  median(date), 
                  max(date),
                  value = max(date),
                  step=1,
                  animate=T),
      leafletOutput("mymap")
   )

   server <- function(input, output, session) {
      points <- reactive({
         data_a1 %>%
            filter(Date==input$time)
      })

# original code   
   # output$mymap <- renderLeaflet({
   #    leaflet() %>%
   #       addMarkers(data = points(),popup=as.character(points()$a))
   # })
   
   #in server replace 
   output$mymap <- renderLeaflet({
      leaflet() %>%
         addTiles(tilesURL)%>%
         addMarkers(data = points(),popup=as.character(points()$a))
   })      
}

shinyApp(ui, server)

# Amy's lake ----

df_fish$a <- seq(1, nrow(df_fish), 1)
df_fish$Date <- date(df_fish$Start.Time)

ui <- fluidPage(
   sliderInput("time", "Date",
               min(df_fish$Date), 
               max(df_fish$Date),
               value = max(df_fish$Date),
               step=1,
               animate=T),
   leafletOutput("mymap")
)


server <- function(input, output, session) {
   points <- reactive({
      df_fish %>%
         filter(Date == input$time) 
   })

   output$mymap <- renderLeaflet({
      leaflet() %>%
         setView(lng = -55.28,lat=48.99,zoom=11) %>%
         addTiles(tilesURL)%>%
         addMarkers(data = points(), popup=as.character(points()$a)) 
   })      
}


shinyApp(ui, server)

date(df_fish$Start.Time)


# for path
## https://stackoverflow.com/questions/42026578/drawing-journey-path-using-leaflet-in-r
## https://stackoverflow.com/questions/64973850/how-to-show-path-and-distance-on-map-with-leaflet-shiny-apps
# 


#fish slider----
# rat2021_fir
# 
# 
# rat2021_fir$a <- seq(1, nrow(rat2021_fir), 1)
# rat2021_fir$Date <- date(rat2021_fir$Start.Time)
# 
# ui <- fluidPage(
#    #titlePanel("Subset and Plot Fruit Data"),
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("time", "Date",
#                      min(rat2021_fir$Date), 
#                      max(rat2021_fir$Date),
#                      value = max(rat2021_fir$Date),
#                      step=1,
#                      animate=T),
#          sliderInput("tag", "Tag",
#                      min(rat2021_fir$Tag.ID), 
#                      max(rat2021_fir$Tag.ID),
#                      value = max(rat2021_fir$Tag.ID),
#                      step=1,
#                      animate=T)
#          ),
#       mainPanel(
#          leafletOutput("mymap")
#       )
#    )
# )
# 
# 
# 
# server <- function(input, output, session) {
#    points <- reactive({
#       df_fish %>%
#          filter(Date == input$time) %>%
#          filter(Tag.ID == input$tag)
#    })
#    
#    output$mymap <- renderLeaflet({
#       leaflet() %>%
#          setView(lng = -55.28,lat=48.99,zoom=11) %>%
#          addTiles(tilesURL)%>%
#          addMarkers(data = points(), popup=as.character(points()$a)) 
#       # %>%
#       #    addPolylines(
#       #       df_fish$Long,df_fish$Lat)
#       #       popup = paste(round(route_summary[1]/60), 'hr', br(), 
#       #       round(route_summary[2]), 'km'))
#    })      
# }
# 
# 
# shinyApp(ui, server)
# 


#fish dropdown----
rat2021_fir


rat2021_fir$a <-rep(NA, nrow = rat2021_fir)
for(i in unique(rat2021_fir$Tag.ID)){
   temp <- rat2021_fir[rat2021_fir$Tag.ID == i,]
   vec <- seq(1, nrow(temp), 1)
   rat2021_fir[rat2021_fir$Tag.ID == i,]$a <- vec
}


rat2021_fir$Date <- date(rat2021_fir$Start.Time)

ui <- fluidPage(
   #titlePanel("Subset and Plot Fruit Data"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("time", "Date",
                     min(rat2021_fir$Date), 
                     max(rat2021_fir$Date),
                     value = max(rat2021_fir$Date),
                     step=1,
                     animate=T),
         selectizeInput("tag", "Tag",
                     choices = c("Tag.ID" = "", 
                                 levels(as.factor(rat2021_fir$Tag.ID))))
      ),
      mainPanel(
         leafletOutput("mymap")
      )
   )
)


server <- function(input, output, session) {
   points <- reactive({
      df_fish %>%
         filter(Date == input$time & Tag.ID == input$tag) #%>%
         #filter(Tag.ID == input$tag)
   })
   
   output$mymap <- renderLeaflet({
      leaflet() %>%
         setView(lng = -55.28,lat=48.99,zoom=11) %>%
         addTiles(tilesURL)%>%
         addMarkers(data = points(), popup=as.character(points()$a)) 
      # %>%
      #    addPolylines(
      #       df_fish$Long,df_fish$Lat)
      #       popup = paste(round(route_summary[1]/60), 'hr', br(), 
      #       round(route_summary[2]), 'km'))
   })      
}


shinyApp(ui, server)


# path ----

df_fish$a <- seq(1, nrow(df_fish), 1)
df_fish$Date <- date(df_fish$Start.Time)
df_fish <- df_fish |>
   filter(year(Date) >=2021)

ui <- fluidPage(
   sliderInput("time", "Date",
               min(df_fish$Date), 
               max(df_fish$Date),
               value = max(df_fish$Date),
               step=1,
               animate=T),
   leafletOutput("mymap")
)


server <- function(input, output, session) {
   points <- reactive({
      df_fish %>%
         filter(Date == input$time) 
   })
   
   history <- reactive({
      df_fish %>%
         filter(Date <= input$time)
   })
   
   output$mymap <- renderLeaflet({
      leaflet() %>%
         setView(lng = -55.28,lat=48.99,zoom=11) %>%
         addTiles(tilesURL)%>%
         #addMarkers(data = points(), popup=as.character(points()$a)) 
         #addPolylines(lat = df_fish$Lat, lng = df_fish$Long)
         #addTrajPaths(lat = df_fish$Lat, lng = df_fish$Long)
      # %>%
      #    addPolylines(
      #       df_fish$Long,df_fish$Lat)
      #       popup = paste(round(route_summary[1]/60), 'hr', br(),
      #       round(route_summary[2]), 'km'))
      # addMarkers(lng = ~Long,
      #            lat = ~Lat,
      #            data = points()) %>%
      # addMarkers(lng = ~Long,
      #            lat = ~Lat,
      #            data = history()) %>%
        # for(i in Start.Time){
            addPolylines(lng = ~Long,
                         lat = ~Lat,
                         data = history())
            
         #}
   })      
}


shinyApp(ui, server)

date(df_fish$Start.Time)

# Date & fish ----

rat2021_fir$Date <- date(rat2021_fir$Start.Time)
rat2021_fir <- rat2021_fir |>
   filter(year(Date) >=2021)
rat2021_fir <- rat2021_fir |>
   group_by(Tag.ID) |>
   mutate(a = 1:length(Tag.ID))
rat2021_fir <- as.data.frame(rat2021_fir)
str(rat2021_fir)
#head(sort(rat2021_fir[,c(1,3, 8, 11:12, 17:18)], Tag.ID), 100)
rat2021_fir[order(rat2021_fir$Tag.ID),]

rat2021_fir$longj <- jitter(rat2021_fir$Long, factor = 1)
rat2021_fir$latj <- jitter(rat2021_fir$Lat, factor = 1)

#pal <- colorNumeric(palette = magma, domain = rat2021_fir$a)
pal <- colorNumeric(palette = "Blues", domain = as.numeric(rat2021_fir$a))
# pal <- colorFactor(
#    palette = c('red', 'blue', 'green', 'purple', 'orange'),
#    domain = rat2021_fir$a
# )


## ui ----
ui <- fluidPage(
   #titlePanel("Subset and Plot Fruit Data"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("time", "Date",
                     min(rat2021_fir$Date), 
                     max(rat2021_fir$Date),
                     value = max(rat2021_fir$Date),
                     step=1,
                     animate=T),
         # sliderInput("tag", "Tag",
         #             min(rat2021_fir$Tag.ID), 
         #             max(rat2021_fir$Tag.ID),
         #             value = max(rat2021_fir$Tag.ID),
         #             step=1,
         #             animate=T)
         selectizeInput("tag", "Tag",
                     choices = c("Tag.ID" = "", 
                           levels(as.factor(rat2021_fir$Tag.ID))))
      ),
      mainPanel(
         leafletOutput("mymap", height = 1000) 
      )
   )
)


server <- function(input, output, session) {
   points <- reactive({
      rat2021_fir %>%
         filter(Date == input$time & Tag.ID == input$tag) #%>%
#         filter(Tag.ID == input$tag)
   })
   
   history <- reactive({
      rat2021_fir %>%
         filter(Date <= input$time)
   })
   
#   pal <- colorNumeric(palette = "Blues", domain = as.numeric(rat2021_fir$a))  
   
   output$mymap <- renderLeaflet({
      leaflet() %>%
         setView(lng = -55.28,lat=48.99,zoom=12) %>%
        # fitBounds(lng1 = -55.31, lat1 = 48.9, lng2 = -55.24, lat2 = 49.08) %>%
         addTiles(tilesURL)%>%
         # addMarkers(lng = ~Long,
         #            lat = ~Lat,
         #            data = points()) %>%
         # addMarkers(lng = ~Long,
         #            lat = ~Lat,
         #            data = history())
#         clearMarkers() |>
         addCircles(lng = ~longj, #Long,
                    lat = ~latj, #Lat,
                    data = history(),
                    # color = ~pal(eval(as.symbol(input$a)))
                    color = ~ ifelse(a <= 3, "red", 
                        ifelse(a >3 & a <=7, "blue", "green"))
                    ) #%>%
      #,
#                  color= ~pal(a))
         # addPolylines(lng = ~longj, #Long,
         #           lat = ~latj, #Lat,
         #           data = history(),
         #           color = ~ ifelse(a <= 3, "red", 
         #             ifelse(a >3 & a <=7, "blue", "green"))
         #           )
   })      
}


shinyApp(ui, server)


# number of detections
rat2021_fir |> group_by(Tag.ID) |> summarize(max = max(a)) |> print(n = 100)
# select columns and then filter by tag to see why some have huge numbers of hits - seems like its mostly because they go between Caroline Upper and Lower (or Xmas lower and upper) and therefore, the detections are very close in time.
## but 255 moved back and forth between Caroline and Xmas
temp <- rat2021_fir |> select(-c("Antenna", "Stop.Time", "Site", "period", "transition", "longj", "latj"))
temp |> filter(Tag.ID == 14)
str(rat2021_fir)



# THIS WORKS
# Date & fish II ----
## https://rstudio.github.io/leaflet/articles/shiny.html

# create a date variable
rat2021_fir$Date <- date(rat2021_fir$Start.Time)
# filter so that just 2021
rat2021_fir <- rat2021_fir |>
   filter(year(Date) >=2021)
# add a sequence variable "a"
rat2021_fir <- rat2021_fir |>
   group_by(Tag.ID) |>
   mutate(a = 1:length(Tag.ID))
# make it a dataframe
rat2021_fir <- as.data.frame(rat2021_fir)
str(rat2021_fir)

rat2021_fir[order(rat2021_fir$Tag.ID),]
# this loop is redundant with "add a sequence variable "a""
# rat2021_fir$a <-rep(NA, nrow = rat2021_fir)
# for(i in unique(rat2021_fir$Tag.ID)){
#    temp <- rat2021_fir[rat2021_fir$Tag.ID == i,]
#    vec <- seq(1, nrow(temp), 1)
#    rat2021_fir[rat2021_fir$Tag.ID == i,]$a <- vec
# }

# add jittered coordinates - may not need this anymore
rat2021_fir$longj <- jitter(rat2021_fir$Long, factor = 100)
rat2021_fir$latj <- jitter(rat2021_fir$Lat, factor = 100)

## colours ----
# create a colour palette - may want to change this
## see below for palettes
##https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
library(RColorBrewer)
my_palette <- brewer.pal.info$maxcolors
# display.brewer.pal(n = 11, name = 'RdYlBu')
# pale <- brewer.pal(11, "RdYlBu")

# display.brewer.pal(n = 11, name = 'Paired')
# pale <- brewer.pal(11, "Paired")

display.brewer.pal(n = 8, name = 'Dark2')
pale <- brewer.pal(n = 8, name = 'Dark2')

# create a column to be filled with colours based on "pale"
rat2021_fir$colr <- rep(NA, nrow(rat2021_fir)) # 1. output

# just for testing
# test <- rat2021_fir |>
#    select(Tag.ID, a, Date, colr)
# arrange(test, Tag.ID)

# loop to put colours sequentially into column "colr" in order "a" no matter the length of "a"
for(i in unique(rat2021_fir$Tag.ID)){
   temp <- rat2021_fir[as.numeric(rat2021_fir$Tag.ID) == i,]
   temp$colr <- replace(temp$colr, temp$a, values = pale)
   rat2021_fir[rat2021_fir$Tag.ID == i,]$colr <- temp$colr
}

# just subsets on a Tag.ID
rat2021_fir[rat2021_fir$Tag.ID == 18,]

rat2021_fir |>
   group_by(Tag.ID) |>
   summarize(count = n()) |>
   print(n =100)

rat2021_fir |> group_by(Tag.ID) |> summarize(max = max(a)) |> print(n = 100)

rat2021_fir$Datechr <- as.character(rat2021_fir$Date)

## ui ----
#library(shinyWidgets)
ui <- fluidPage(
   #titlePanel("Subset and Plot Fruit Data"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("time", "Date",
            min(rat2021_fir$Date), 
            max(rat2021_fir$Date),
            value = min(rat2021_fir$Date),
            step=2,
            animate=T),# "time is the input value used below, "Date" is just a label, min/max sets the boundary on values in the slider, and value is the starting value.  Step is how fast the slider moves
         selectizeInput("tag", "Tag",
                     choices = c("Tag.ID" = "", 
                           levels(as.factor(rat2021_fir$Tag.ID)))),
         tableOutput("table")
      ),
      mainPanel(
         leafletOutput("mymap", height = 1000) 
      )
   )
)


server <- function(input, output) {
   
# this is about as close as I can come to reset https://stackoverflow.com/questions/62720103/r-shiny-reset-function-on-selected-inputs
   
   # Reactive expression for the data subsetted to what the user selected - in this case, Date and Tag.ID from rat2021_fir are now the variables time and tag in the data set points
   points <- reactive({
      rat2021_fir[,c(3, 10, 17, 19:21)] %>%
         filter(Date == input$time & Tag.ID == input$tag)
   })
   
   points_tab <- reactive({
      rat2021_fir[,c(3, 10, 19:20, 22)] %>%
         filter(Tag.ID == input$tag)
   })

   # Use leaflet() here, and only include aspects of the map that
   # won't need to change dynamically (at least, not unless the
   # entire map is being torn down and recreated).
   output$mymap <- renderLeaflet({
      leaflet(rat2021_fir) %>%
         setView(lng = -55.28,lat=48.99,zoom=12) %>%
         addTiles(tilesURL)
         #fitBounds(~min(Long), ~min(Lat), ~max(Long), ~max(Lat)) 
      })

   # Incremental changes to the map (in this case, replacing the
   # circles when a new color is chosen) should be performed in
   # an observer. Each independent set of things that can change
   # should be managed in its own observer.
   observe ({
      leafletProxy("mymap", data = points()) %>%
         #clearShapes() %>%
         #clearMarkerClusters() %>%
         addCircles(lng = ~longj, #Long,
                   lat = ~latj, #Lat,
                   weight = 20,
                    color = ~colr,
                   fillOpacity = 0.7,
                  # popup = ~paste(a)
                ) 
   })
   
   output$table <- renderTable(points_tab())
   
}

### shiny ----
shinyApp(ui, server)
   

# data summary ----
rat2021_fir |>
   group_by(Tag.ID) |>
   summarise(count = n()) |>
   filter(count <10) |>
   print(n = Inf)
   
rat2021_fir |>
   filter(Tag.ID == 11) |>
   summarise(FstDate = min(Date), LstDate = max(Date))

grt10 <- c(14, 18, 20, 28, 39, 46, 55, 255, 999)
  
rat2021_fir |>
   filter(!Tag.ID %in% grt10) |>
   group_by(Tag.ID) |>
   summarise(FstDate = min(Date), LstDate = max(Date), count = n()) |>
   mutate(diff = LstDate - FstDate) |>
   print(n = Inf)


rat2021_fir |>
   filter(Tag.ID == 11) |> 
   select(Location.Name, hrs, Date, colr, Long, longj)


# simulate ----
tag <- sort(rep(seq(1, 4, 1), 15))
date <- rep(seq(1, length(tag)/4, 1), 4)
long <- rnorm(length(tag), -55.28, 0.01)
lat <- rnorm(length(tag), 48.99, 0.01)

df <- as.data.frame(cbind(tag, date, long, lat))
str(df)


my_palette <- brewer.pal.info$maxcolors
display.brewer.pal(n = 11, name = 'RdYlBu')
pale <- brewer.pal(11, "RdYlBu")

df$colr <- rep(NA, nrow(df)) # 1. output
#
for(i in unique(df$tag)){
      temp <- df[df$tag == i,]
      df$colr <- replace(temp$colr, temp$date, values = pale)      
}

df


# this works
temp1 <- df[df$tag == 1, ]
temp1$colr <- replace(temp1$colr, temp1$date, values = pale)

###ui ----
ui <- fluidPage(
   #titlePanel("Subset and Plot Fruit Data"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("time", "Date", # "time is the input value used below, "Date" is just a label, min/max sets the boundary on values in the slider, and value is the starting value.  Step is how fast the slider moves
                     min(df$date), 
                     max(df$date),
                     value = min(df$date),
                     step=1,
                     animate=T),
         selectizeInput("tag", "Tag",
                        choices = c("tag" = "", 
                                    levels(as.factor(df$tag))))
      ),
      mainPanel(
         leafletOutput("mymap", height = 1000) 
      )
   )
)


server <- function(input, output, session) {
   
   
   
   
   # Reactive expression for the data subsetted to what the user selected(input )
   points <- reactive({
      df %>%
         filter(date == input$time & tag == input$tag) #%>% 
   })

   # history <- reactive({
   #    df %>%
   #       filter(date == input$time)
   # })

  # colorpal <- reactive({
  #  colorNumeric(input$time, df$date)
  #  })
   # but this is not doing what I thought it was doing - it links to the SelectInput("colors" ....) above which creates a dropdown menu which is not what we want.  Need a sequence of colours for the number of steps.
   
   output$mymap <- renderLeaflet({
      leaflet(df) %>%
         #setView(lng = -55.28,lat=48.99,zoom=12) %>%
         addTiles(tilesURL) %>%
         fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
   })
   
   
   observe ({
      
      leafletProxy("mymap", data = points()) %>%
         #clearShapes() %>%
         addCircles(lng = ~long,
                    lat = ~lat,
                   # data = points(),
                    #weight = 10,
                   color = ~colr,
                  fillOpacity = 0.7,
         )
     })
}

### shiny ----
shinyApp(ui, server)



