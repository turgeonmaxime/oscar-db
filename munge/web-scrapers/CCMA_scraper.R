library(tidyverse)
library(magrittr)
library(rvest)

# The format of the wiki tables changes over time...
data_ccma1 <- read_html("https://en.wikipedia.org/wiki/23nd_Critics%27_Choice_Awards") %>% 
    html_nodes("table") %>% 
    extract2(2) %>%
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
                    data_frame(Movie = movie, Nominee = nominee[1], Winner = win)
                })
        } else {
            html_nodes(x, "li") %>%
                map_df(function(y) {
                    movie <- html_nodes(y, xpath = "./b/i/a") %>% html_text
                    if(all(length(movie) == 0)) {
                        movie <- html_nodes(y, xpath = "./i/b/a") %>% html_text
                    }
                    if (length(movie) == 0) {
                        movie <- html_nodes(y, xpath = "./i/a") %>% html_text
                        win <- 0
                    } else {
                        win <- 1
                    }
                    data_frame(Movie = movie[1], Nominee = NA, Winner = win)
                })
        } 
        tmp %>%
            mutate(Category = category)
    })