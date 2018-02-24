library(tidyverse)
library(magrittr)
library(rvest)

# Best feature film----
dataset_best_feature <- read_html("https://en.wikipedia.org/wiki/Producers_Guild_of_America_Award_for_Best_Theatrical_Motion_Picture") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    html_table() %>% 
    bind_rows() %>% 
    mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    group_by(Year) %>% 
    mutate(Winner = case_when(
        Film == first(Film) ~ 1,
        grepl("TIE", Film) ~ 1,
        TRUE ~ 0
    ),
    Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    Film = trimws(Film)) %>% 
    rename(Producers = `Producer(s)`)

# Best animated film----
dataset_best_animated <- read_html("https://en.wikipedia.org/wiki/Producers_Guild_of_America_Award_for_Best_Animated_Motion_Picture") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    html_table() %>% 
    .[1:2] %>% 
    bind_rows() %>% 
    mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    group_by(Year) %>% 
    mutate(Winner = case_when(
        Film == first(Film) ~ 1,
        grepl("TIE", Film) ~ 1,
        TRUE ~ 0
    ),
    Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    Film = trimws(Film)) %>% 
    rename(Producers = `Producer(s)`) %>% 
    select(-`Studio(s)`)

# Best documentary----
tables_best_doc <- read_html("https://en.wikipedia.org/wiki/Producers_Guild_of_America_Award_for_Best_Documentary_Motion_Picture") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") 

data1 <- tables_best_doc[[1]] %>%
    html_nodes("tr") %>% 
    lapply("html_nodes", xpath = ".//td|.//th") %>% 
    lapply(html_text, trim = TRUE) %>% 
    lapply(function(vect) if (length(vect) == 3) vect else c(NA, vect)) %>%
    .[-1] %>% 
    purrr::map_df(function(str) {
        tibble(
            Year = str[1],
            Film = str[2],
            Producers = str[3]
        )
    }) %>% 
    fill(Year)

data2 <- tables_best_doc[[2]] %>% 
    html_table() %>% 
    rename(Producers = `Producer(s)`)

dataset_best_doc <- data1 %>% 
    bind_rows(data2) %>% 
    mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    group_by(Year) %>% 
    mutate(Winner = case_when(
        Film == first(Film) ~ 1,
        grepl("TIE", Film) ~ 1,
        TRUE ~ 0
    ),
    Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    Film = trimws(Film))

# Merge the three---
bind_rows(
    mutate(dataset_best_feature, Award = "Feature Film"),
    mutate(dataset_best_animated, Award = "Animated Film"),
    mutate(dataset_best_doc, Award = "Documentary")
) %>% 
    ungroup() %>% 
    mutate(Year = stringr::str_sub(Year, 1, 4),
           Year = as.numeric(Year)) %>% 
    saveRDS("cache/pga_historical.rds")