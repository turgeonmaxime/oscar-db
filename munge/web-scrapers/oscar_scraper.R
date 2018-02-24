########################
# Web scraper for the 
#   Academy Awards
#
# Created:  2017-02-24
# Modified: 2018-01-25
########################
library(rvest)
library(tidyverse)
# See http://stackoverflow.com/a/42296675

# Current year-nominees----
pg <- read_html("https://en.wikipedia.org/wiki/90th_Academy_Awards")

html_nodes(pg, xpath=".//h2[span/@id = 'Nominees']/following-sibling::table[1]") %>%
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
          data_frame(Movie = movie, Nominee = nominee[1])
        })
    } else {
      html_nodes(x, "li") %>%
        map_df(function(y) {
          movie <- html_nodes(y, xpath = "./i/a") %>% html_text
          data_frame(Movie=movie[1], Nominee = NA)
        })
    } 
    tmp %>%
      mutate(Category = category)
  }) %>% 
  mutate(Year = "2018") %>%
  select(Year, Category, Movie, Nominee) %>% View


# Previous year-nominees and winners----
results <- map_df(1987:2017, function(year) {
  award <- year - 1928
  
  if (award %in% c(1, 1 + seq(20, 100, by = 10))) award <- paste0(award, "st")
  if (award %in% c(2, 2 + seq(20, 100, by = 10))) award <- paste0(award, "nd")
  if (award %in% c(3, 3 + seq(20, 100, by = 10))) award <- paste0(award, "rd")
  if (award %in% c(4:19, outer(c(0, 4:9), 
                               seq(20, 100, by = 10), 
                               FUN = `+`))) award <- paste0(award, "th")
  
  url <- paste0("https://en.wikipedia.org/wiki/",
                award, "_Academy_Awards")
  pg <- read_html(url)
  
  html_nodes(pg, xpath=".//h3[span/@id = 'Awards']/following-sibling::table[1]") %>%
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
              win <- "No"
            } else {
              win <- "Yes"
            }
            data_frame(Movie = movie, Nominee = nominee[1], Winner = win)
          })
      } else {
        html_nodes(x, "li") %>%
          map_df(function(y) {
            movie <- html_nodes(y, xpath = "./b/i/a") %>% html_text
            if (length(movie) == 0) {
              movie <- html_nodes(y, xpath = "./i/a") %>% html_text
              win <- "No"
            } else {
              win <- "Yes"
            }
            data_frame(Movie = movie[1], Nominee = NA, Winner = win)
          })
      } 
      tmp %>%
        mutate(Category = category)
    }) %>% 
    mutate(Year = as.character(year)) %>%
    select(Year, Category, Movie, Nominee, Winner)
})

# The web scraper will need to be adapted to different years, because the layout
# is not consistent throughout
# The code above works for the following years:
# 1. 1987 to 2016
# The issue with other years is in how the table is coded (e.g. use of divs or not)
