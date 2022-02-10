# https://d4tagirl.com/2017/05/how-to-fetch-twitter-users-with-r

############### 1. Installing, authorisation to Twitter REST API and retrieve the dataset of users
# package for package development and anable to install packages from Githab
library(devtools)
# get the current development version of rtweet from Github
# devtools::install_github("mkearney/rtweet")
setwd('/Users/ksenia/Documents/Work/July/Twitter_users_analysis')
# Intro to rtweet https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html
# Documentation http://rtweet.info/reference/index.html
library(rtweet)

# Twitter API authentication
source("Twitter_auth.R") 

# q - query to the Twitter's REST AP: ask for n = 1 000 Twitter users (the maximum from a single search), who mentioned 'T-labs'
# and parse = TRUE transforms the output to DataFrame of 1 000 rows of users, with 36 variables regarding them
# token is set up by default 
# https://developer.twitter.com/en/docs/accounts-and-users/follow-search-get-users/api-reference/get-users-search
users <- rtweet::search_users(q = 'T-Labs',
                      n = 1000,
                      parse = TRUE,
                      verbose = TRUE)

# package DT provides an R interface to the JavaScript library DataTables.
# R data objects (matrices or data frames) can be displayed as tables on HTML pages, 
# and DataTables provides filtering, pagination, sorting, and many other features in the tables.
# devtools::install_github('rstudio/DT')
library(DT)
datatable(users[, c("name", "screen_name", "location", "description", 'text')], 
          rownames = FALSE, options = list(pageLength = 7))

# View(users[, c("name", "screen_name", "location", "description", 'text',"created_at")])
# retrieves the user if q matches the user’s description, name or screen_name (handle), 
# and also if it matches something they tweeted
# unique users' share:
paste("Share of unique users= ", round(length(unique(users$name))/nrow(users),2))

############### 2. Cleaning and format the data

library(lubridate) # formattind time variables
library(dplyr) # for pipline %>% syntax and mutate() - computes and adds new variable(s). Preserves existing variables.
library(stringr) 
library(data.table)
library(tidyr)

users <- as.data.table(users)[!(is.na(created_at)) & !location == '']

# filter out the users' names starting with Abbott using regexp
# format 'created_at' from %Y-%m-%d %h:%m:%s to the age in days: age_days
# and created_at_month - neglect the days and replace them with 01 but add one month if days>15 

tlabs <- as.data.table(unique(users) %>% 
      filter(!str_detect(screen_name, '^(Abbott).*')) %>%
      mutate(created_at = format(as.Date(created_at), format = '%Y-%m-%d'),
             created_at_month = ifelse(day(as.Date(created_at))<15, 
                                       paste(format(as.Date(created_at), format = '%Y-%m'),"-01",sep=""), 
                                       paste(format(as.Date(created_at) %m+% months(1),"%Y-%m"),"-01",sep="")),
              age_days = as.integer(difftime(created_at, min(created_at), unit = 'days')),
              age_months = interval(ymd(min(created_at_month)), ymd(created_at_month)) %/% months(1)) %>%
      select(screen_name, location, created_at, created_at_month, followers = followers_count, age_days, age_months))

tlabs[, c('created_at','created_at_month')] <- data.frame('created_at' = as.Date(tlabs$created_at),
                                                          'created_at_month'= as.Date(tlabs$created_at_month))

datatable(tlabs,rownames = FALSE, options = list(pageLength = 7))

############### 2. obtain the latitude and longitude for each user
#install.packages('purrr')
# devtools::install_github("dkahle/ggmap") # the latest ggmap v2.7 allows the user to specify a Google Maps API key
library(ggmap) # interacts with Google Maps to retrieve the coordinates from the location
library(purrr) # map - function for capturing both latitude and longitude in a single column of the dataframe

# alternative to ggmap but requires api-key with billing account
# library(googleway) # https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html#api-key

# get both latitude and longitude from Google Maps (ggmap::geocode) of tlabs$location 
# to one new column 'longlat' using purrr::map function for just one pass
tlabs <- tlabs %>% 
  mutate(longlat = purrr::map(.$location, ggmap::geocode)) 

#geocodeQueryCheck() # check the number of remaining queries
# there are a lot of missing values, if desired, can be cleaned manually

# split list-columns into the several columns - each element from the list has its own column
# filter out the missing geodata

tlabs <- tlabs %>% 
  tidyr::unnest() %>% filter(!(is.na(lon)))
# datatable(tlabs,rownames = FALSE, options = list(pageLength = 7))

# save dataset to the disk
write.table(tlabs, 'tlabs.csv', append = FALSE, quote = FALSE, sep = ";",
            na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

############### 2. Data Vizualization at the World map
tlabs <- read.csv(file = 'tlabs.csv', header=TRUE, stringsAsFactors = FALSE, sep = ';')

library(plotly)
library(gganimate)
library(ggthemes)

### 1st way - 3,6 MB
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()
# add the extra (and unofficial) text aesthetic for ggplotly. 
# As ggplot2 doesn’t have a text aesthetic it ignores it, but ggplotly recognizes it and displays it in the tooltip.
map <- world +
  geom_point(data = tlabs, 
             aes(x = lon, y = lat,
                 text = paste('city: ', location,
                              '<br /> created : ', created_at),
                 size = followers),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(2000, 5000, 10000, 20000, 35000)) +
  labs(size = 'Followers')

# how to save graphs - http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html

# par(mar=c(5,3,2,2)+0.1) are the number of lines of text reserved on the bottom, left, top and right.
# The default is = 4.1 lines.

# save pic in a vector-based format (scale-independent) 
# png(file="mygraphic.png",width=800)
# par(mar=c(5,3,2,2)+0.1) # To remove the space reserved for labels

# dev.off()

# save pic in a pixel-based format instead - width=800 is a good choice for a full-screen graphic; 
# more than 1200 pixels in either direction - high resolution
# use height= OR width= but not both, to preserve aspect ratio
par(mar=c(5,3,2,2)+0.1)
map
ggsave("pictures/map.png", map, device = png(), units = "in", width=13, dpi = 300)

ggplotly(map, tooltip = c('text', 'size'))

### 2nd way - optimized for HTML

g <- list(showframe = FALSE,
          coastlinecolor = toRGB("white"),
          showland = TRUE,
          landcolor = toRGB("gray80"),
          showcountries = TRUE,
          countrycolor = toRGB("white"),
          countrywidth = 0.2,
          projection = list(type = 'Mercator'))

map_optimized <- plot_geo(tlabs,
         marker = list(color = toRGB("purple"),
                       opacity = 0.5,
                       line = list(color = toRGB("purple"),
                                   width = 1.5))) %>%
add_markers(x = ~lon,
              y = ~lat,
              sizes = c(1, 450),
              size = ~followers,
              hoverinfo = "text",
              text = ~paste('city: ', location,
                            '<br /> created: ', created_at,
                            '<br /> followers: ', followers)) %>%
layout(geo = g)

map_optimized_plotly <- ggplotly(map_optimized, tooltip = c('text', 'size'))
map_optimized_plotly
# api_create(map_optimized_plotly, filename = "World_map_optimized")
# https://plot.ly/~Ksyula/1/#/

############### 3. Data Animation
# add an empty frame at the beginning so that the first frame you see is just the empty map
# add frame=created_at aesthetic with cumulative = TRUE to the ggplot; it ignotes it, but gganimate reads it
# add some empty frames at the end as well to see the final composition a bit longer

tlabs$created_at_month <- as.Date(tlabs$created_at_month)
tlabs$created_at <- as.Date(tlabs$created_at)

ghost_points_ini <- data.frame(
  created_at_month = seq(as.Date('2010-02-01'),
                   as.Date('2010-02-01'),
                   by = 'month'),
  followers = 0, 
  lon = 0, lat = 0)

ghost_points_fin <- data.frame(
  created_at_month = seq(as.Date('2018-07-01'),
                   as.Date('2019-07-01'),
                   by = 'month'),
  followers = 0, lon = 0, lat = 0)

map1 <- world +
  geom_point(data = tlabs, aes(x = lon, y = lat, size = followers, 
                 frame = created_at_month, cumulative = TRUE),
             colour = 'purple', alpha = .5) +
  geom_point(data = ghost_points_ini, aes(x = lon, y = lat, size = followers, # this is the init transparent frame
                 frame = created_at_month, cumulative = TRUE),
             alpha = 0) +
  geom_point(data = ghost_points_fin, aes(x = lon, y = lat, size = followers, # this is the final transparent frames
                 frame = created_at_month, cumulative = TRUE),
              alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(2000, 5000, 10000, 20000, 35000)) +
  labs(size = 'Followers') 

gganimate(map1, interval = 0.2, "pictures/output_appears_per_months.gif", ani.width = 1000)

############### 3. Linear growth of number of followers over time by months (animation)

months <- data.frame(value = seq(min(tlabs$created_at_month), max(tlabs$created_at_month), by='month'))

# expand dataset
tlabs_frames <- tlabs %>%
  select(screen_name) %>%
  expand(screen_name, date = months$value) %>%
  right_join(tlabs, by = 'screen_name') %>%
  filter(date > created_at_month) %>%
  mutate(age_total_month = as.numeric(age_months, units = 'months'),
         age_at_month = as.numeric(interval(ymd(created_at_month), ymd(date)) %/% months(1)),
         est_followers_month = ((followers - 1) / age_total_month) * age_at_month)

ghost_points_ini <-  ghost_points_ini %>%
  mutate(date = created_at_month,
         est_followers_month = 0)

ghost_points_fin <-  ghost_points_fin %>%
  expand(date = created_at_month, tlabs) %>%
  select(date, est_followers_month = followers, lon, lat)

map_frames <- world +
  geom_point(data = tlabs_frames, aes(x = lon, y = lat, size = est_followers_month, 
                 frame = date),
              colour = 'purple', alpha = .5) +
  geom_point(data = ghost_points_ini,aes(x = lon, y = lat, size = est_followers_month,
                 frame = date),
              alpha = 0) +
  geom_point(data = ghost_points_fin, aes(x = lon, y = lat, size = est_followers_month,
                 frame = date),
              colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(2000, 5000, 10000, 20000, 35000)) +
  labs(size = 'Followers')

gganimate(map_frames, interval = .25, "pictures/output_growth_per_months.gif", ani.width = 1000)

