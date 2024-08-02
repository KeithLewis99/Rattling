# Data file for Rattling Brook project.  Emily Geissinger has done some great Rmd summaries.  Need to establish what the questions are.  

# Set up a project - see the below link fir directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - VErsion Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data") # best to keep the data centralized
if(!dir.exists("data/derived"))dir.create("data/derived") # best to keep the data centralized
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report
if(!dir.exists("refs"))dir.create("refs") #for rmd report


#packages
library(tidyverse)
library(lubridate)

# source
# this is not a source but I think this renders the file and produces the R objects.
# might be easier just to write it in Rmd
rmarkdown::render(input = "../Rcode/Rattling-summary2022.Rmd")


ratl2020 <- read.csv("../data/2020/rattling2020-detections.csv") |> 
   mutate_at(vars(ends_with("Time")), lubridate::ymd_hms)
str(ratl2020)


bio.data$LS
# makes some corrections in the data
bio.data$LS[38] <- "Kelt"
bio.data$LS[39] <- "Kelt"

table(as.factor(bio.data$LS))
count(bio.data, as.factor(LS))

ggplot(bio.data, aes(Weight.g)) +
   geom_histogram() + 
   facet_wrap(vars(as.factor(LS)), 3, 1)

# OK - so grilse and kelts are all about the same weight but where do they hang out?

tmp <- R22[,c("Tag.ID", "SRX", "Site", "Location", "Long", "Lat")]

# Emilie's code in full
leaflet()%>%
   setView(lng = -55.28,lat=48.99,zoom=11)%>%addTiles()%>%
   addCircleMarkers(data=tmp, lng=tmp$Long, lat=tmp$Lat,
                    popup = ~Location,
                    label = ~Location,
                    radius = 6,
                    color = 'black',
                    fillColor = 'blue',
                    stroke = TRUE,
                    weight = 1,
                    fillOpacity = 0.5) 

library(leaflet)
data(quakes)
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
   addCircleMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
