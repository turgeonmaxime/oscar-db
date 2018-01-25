library(tidyverse)
library(magrittr)
library(rvest)

# From Kaggle 1927-2015----
oscar_data <- read_csv("database.csv")

# Add 2016----
oscar_data2016 <- read_html("https://en.wikipedia.org/wiki/89th_Academy_Awards") %>% 
    html_nodes(xpath=".//h3[span/@id = 'Awards']/following-sibling::table[1]") %>%
    html_nodes("td") %>%
    map_df(function(x) {
        category <- html_nodes(x, "div") %>% html_text()
        tmp <- if (grepl("Act[or|ress]", category) |
                   grepl("Director", category) |
                   grepl("Song", category)) {
            html_nodes(x, "li") %>%
                map_df(function(y) {
                    movie <- html_nodes(y, xpath = "./b/i/a") %>% html_text
                    nominee <- html_nodes(y, xpath = "./b/a") %>% html_text
                    if (length(movie) == 0) {
                        movie <- html_nodes(y, xpath = ".//i/a[not(ancestor::b)]") %>% html_text
                        nominee <- html_nodes(y, xpath = ".//a[not(ancestor::b)]") %>% html_text
                        win <- 0
                    } else {
                        win <- 1
                    }
                    data_frame(Movie = movie, Name = nominee[1], Winner = win)
                })
        } else {
            html_nodes(x, "li") %>%
                map_df(function(y) {
                    movie <- html_nodes(y, xpath = "./b/i/a") %>% html_text
                    if (length(movie) == 0) {
                        movie <- html_nodes(y, xpath = "./i/a") %>% html_text
                        win <- 0
                    } else {
                        win <- 1
                    }
                    data_frame(Movie = movie[1], Name = NA, Winner = win)
                })
        } 
        tmp %>%
            mutate(Award = category)
    }) %>% 
    mutate(Year = "2016", Ceremony = 89) 

oscar_data %<>% bind_rows(oscar_data2016)

oscar_data %>% 
    saveRDS("cache/oscar_historical.rds")

# Add nominees for 2017----
oscar_nominee_2017 <- read_html("https://en.wikipedia.org/wiki/90th_Academy_Awards") %>% 
    html_nodes(xpath=".//h2[span/@id = 'Nominees']/following-sibling::table[1]") %>%
    html_nodes("td") %>%
    map_df(function(x) {
        category <- html_nodes(x, "div") %>% html_text()
        tmp <- if (grepl("Act[or|ress]", category) |
                   grepl("Director", category) |
                   grepl("Song", category)) {
            html_nodes(x, "li") %>%
                map_df(function(y) {
                    movie <- html_nodes(y, xpath = "./i/a") %>% html_text
                    nominee <- html_nodes(y, xpath = "./a") %>% html_text
                    data_frame(Movie = movie, Name = nominee[1])
                })
        } else {
            html_nodes(x, "li") %>%
                map_df(function(y) {
                    movie <- html_nodes(y, xpath = "./i/a") %>% html_text
                    data_frame(Movie = movie[1], Name = NA)
                })
        } 
        tmp %>%
            mutate(Award = category)
    }) %>% 
    mutate(Year = "2018", Ceremony = 90)

oscar_nominee_2017 %>% 
    saveRDS("cache/oscar_nominees.rds")
