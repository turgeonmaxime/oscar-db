library(tidyverse)
library(magrittr)
library(rvest)

# Best ensemble----
dataset_best_ensemble <- read_html("https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Cast_in_a_Motion_Picture") %>% 
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
    rename(Name = `Cast members`)

# Best actor----
dataset_best_actor <- read_html("https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Male_Actor_in_a_Leading_Role") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    .[-length(.)] %>% 
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
    rename(Name = Nominee) %>% 
    select(-`Role(s)`)

# Best actress----
dataset_best_actress <- read_html("https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Female_Actor_in_a_Leading_Role") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    .[-length(.)] %>% 
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
    rename(Name = Nominee) %>% 
    select(-`Role(s)`)

# Best supporting actor----
dataset_best_supactor <- read_html("https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Male_Actor_in_a_Supporting_Role") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    .[-length(.)] %>% 
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
    rename(Name = Nominee) %>% 
    select(-Role)

# Best supporting actress----
dataset_best_supactress <- read_html("https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Female_Actor_in_a_Supporting_Role") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    .[-length(.)] %>% 
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
    rename(Name = Nominee) %>% 
    select(-Role)


# Put everything together----
bind_rows(
    mutate(dataset_best_ensemble, Award = "Ensemble"),
    mutate(dataset_best_actor, Award = "Lead Actor"),
    mutate(dataset_best_actress, Award = "Lead Actress"),
    mutate(dataset_best_supactor, Award = "Supporting Actor"),
    mutate(dataset_best_supactress, Award = "Supporting Actress")
) %>% 
    ungroup %>% 
    mutate(Year = stringr::str_sub(Year, 1, 4),
           Year = as.numeric(Year),
           Name = stringr::str_replace_all(Name, "(\u2020|\u2021)", "")) %>% 
    saveRDS("cache/sga_historical.rds")
    