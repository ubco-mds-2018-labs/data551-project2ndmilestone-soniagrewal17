library(tidyverse) ; library(leaflet)
crimes <- read.csv("crimedata.csv", header = T)
levels(crimes$category)

# Plot the top 10 repeat locations using the leaflet package
locs <- crimes %>% 
  filter(category == "Violence and sexual offences") %>% 
  group_by(long, lat, location, town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>% 
  slice(1:10)

popup <- paste0("<strong>Frequency: </strong>", locs$n,
                "<br><strong>Location: </strong>", locs$location,
                "<br><strong>Borough: </strong>", locs$town)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(data = locs, 
                   ~long, ~lat, 
                   fillColor = "white", color = "red",  
                   radius = ~n, # this may need to be controlled e.g. radius = ~n*0.1
                   popup = ~popup) 
