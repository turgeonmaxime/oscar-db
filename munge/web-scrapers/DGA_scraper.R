library(rvest)
library(tidyverse)
library(magrittr)
is_empty_str <- function(str) str == ""

# Best feature film----
parsed_doc <- read_html("https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_%E2%80%93_Feature_Film")

winners <- parsed_doc %>% 
  html_nodes(xpath = "//ul/li/b") %>% 
  html_text() %>% 
    stringr::str_split("(:| \u2013) ") %>% 
    purrr::map_df(function(str) {
        tibble(
            Year = str[1],
            Winner = 1,
            Director = str[2],
            Movie = str[3]
        )
    })

nominees <- parsed_doc %>% 
  html_nodes(xpath = "//ul/li/ul") %>% 
  html_text() %>% 
  stringr::str_split("\n") %>% 
  .[-1] 

nominees <- lapply(seq_len(length(nominees)), 
                   function(i) nominees[[i]] <- c(nominees[[i]], 1947 + i)) %>% 
  purrr::map_df(function(foo) {
    Filter(purrr::negate(is_empty_str), foo) %>% 
      stringr::str_replace("\u2013", "-") %>% 
      stringr::str_replace("(\u2021|\u2020)", "") %>% 
      trimws %>% 
      stringr::str_split(" - ") %>% 
      .[-length(.)] %>% 
      purrr::map_df(function(foo) tibble(Director = foo[1], 
                                         Movie = foo[2])) %>% 
      mutate(Year = foo[length(foo)],
             Winner = 0)
  })

dataset_best_dir <- winners %>% 
    bind_rows(nominees) %>% 
    arrange(Year, desc(Winner))

dataset_post80 <- parsed_doc %>% 
    html_nodes("table.wikitable") %>% 
    html_table %>% 
    bind_rows %>% 
    rename(Movie = Film,
           Director = `Winners and nominees`) %>% 
    group_by(Year) %>% 
    mutate(Winner = as.numeric(Movie == first(Movie)),
           Director = stringr::str_replace(Director, "(\u2021|\u2020)", ""))

dataset_best_dir <- dataset_best_dir %>% 
    mutate(Year = as.numeric(Year)) %>% 
    bind_rows(dataset_post80)

# Best documentary----
parsed_doc <- read_html("https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_–_Documentaries")

winners <- parsed_doc %>% 
    html_nodes(xpath = "//ul/li/b") %>% 
    html_text() %>% 
    stringr::str_split("(:| \u2013) ") %>% 
    purrr::map_df(function(str) {
        tibble(
            Year = str[1],
            Winner = 1,
            Director = str[2],
            Movie = str[3]
        )
    })

nominees <- parsed_doc %>% 
    html_nodes(xpath = "//ul/li/ul") %>% 
    html_text() %>% 
    stringr::str_split("\n") %>% 
    .[-1] 

nominees <- lapply(seq_len(length(nominees)), 
                   function(i) nominees[[i]] <- c(nominees[[i]], 1990 + i)) %>% 
    purrr::map_df(function(foo) {
        Filter(purrr::negate(is_empty_str), foo) %>% 
            stringr::str_replace("\u2013", "-") %>% 
            stringr::str_replace("(\u2021|\u2020)", "") %>% 
            trimws %>% 
            stringr::str_split(" - ") %>% 
            .[-length(.)] %>% 
            purrr::map_df(function(foo) tibble(Director = foo[1], 
                                               Movie = foo[2])) %>% 
            mutate(Year = foo[length(foo)],
                   Winner = 0)
    })

dataset_best_doc <- winners %>% 
    bind_rows(nominees) %>% 
    arrange(Year, desc(Winner)) %>% 
    mutate(Year = as.numeric(Year))

# Merge the two---
bind_rows(
    mutate(dataset_best_dir, Award = "Feature Film"),
    mutate(dataset_best_doc, Award = "Documentary")
) %>% 
    saveRDS("cache/dga_historical.rds")