#add library processing data 
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(httr)
library(rvest)
library(stringi)

# Aplikasi
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)

#visualisasi
library(ggplot2)
library(plotly)
library(scales)
library(glue)
library(leaflet)
library(maps)
# install.packages("geojsonio")
library(geojsonio)
library(viridis)

#import data
player_data_6 <- read_csv("data/players_20.csv")
team <- read_csv("data/teams_and_leagues.csv")
player_data_2019 <- read_csv("data/FIFA 19 Player DB.csv")


# Bind seperate data
player_data <- rbind(player_data_1)

# data wrangling 

encode <- function(x) {
  Encoding(x) <- 'latin1'
  return(stri_trans_general(x, 'Latin-ASCII'))
}

league <- player_data_2019 %>% 
  mutate(club = encode(Club),
         league = encode(League)) %>% 
  select(league,club) %>%
  group_by(league) %>% 
  unique()

getFirstInt <- function(x) {
  #check condition
  ifelse(is.na(x),return(NA),x)
  ifelse(is.null(x),return(NA),x)
  #function
  x <- strsplit(as.character(x), "") %>% sapply(head, 2)
  return (as.integer(glue('{x[1,]}{x[2,]}')))
}

goalkeeper_position <- c("GK")
defender_position <- c("RCB","RB","LB","CB","SW","RWB","LWB")
midfielder_position <- c("RCM","LCM","CM","CDM","DM","RW","LW","RM","LM","AM","CAM")
forward_position <- c("ST","CF","RF","LF")

player_data <- player_data %>%
  filter(is.na(short_name) == F,
         is.na(long_name) == F,
         is.na(club) == F
         ) %>% 
  mutate(player_positions = strsplit(as.character(player_positions), ",")  %>% sapply(tail, 1) %>% str_replace_all(" ",repl="")) %>%
  mutate(position = case_when(
    player_positions %in% goalkeeper_position ~ "goalkeeper",
    player_positions %in% defender_position ~ "defender",
    player_positions %in% forward_position ~ "forward",
    player_positions %in% midfielder_position ~ "midfielder"
  )) %>% 
  mutate_at(c(45:73), getFirstInt) %>% 
  mutate(short_name = as.character(short_name),
         long_name = as.character(long_name),
         dob = ymd(dob),
         joined = ymd(joined),
         club = stri_trans_general(club, 'Latin-ASCII'))

player_data <- left_join(player_data, league) %>%
  filter(!is.na(league))

player_league <- player_data %>%
  select(league) %>%
  distinct(league)




#scrapping 
# url <- "https://sofifa.com/teams?type=national"
# url_text <- read_html(url)
# country_code <- html_nodes(url_text, ".opt.bp3") 
# # %>% html_attr("value")
# # country_name <- html_nodes(url_text, ".opt.bp3") %>% html_text()
# # 
# # country <- cbind(country_code,country_name)
# # country
# 