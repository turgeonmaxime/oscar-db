library(tidyverse)
library(magrittr)
library(rvest)

# Best feature film----
data_pic <- read_html("https://en.wikipedia.org/wiki/Chicago_Film_Critics_Association_Award_for_Best_Film") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    html_table() %>% 
    lapply(function(df){
        df <- if ("Winner" %in% names(df)){
            rename(df, Film = Winner)
        } else {
            rename(df, Film = `Winner and nominees`)
        }
    }) %>% 
    bind_rows() %>% 
    mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    group_by(Year) %>% 
    mutate(Winner = case_when(
        Film == first(Film) ~ 1,
        grepl("TIE", Film) ~ 1,
        TRUE ~ 0
    ),
    Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    Film = trimws(Film),
    Award = "Picture") %>% 
    rename(Name = `Director(s)`)

# Best director----
data_director <- read_html("https://en.wikipedia.org/wiki/Chicago_Film_Critics_Association_Award_for_Best_Director") %>% 
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
    Film = trimws(Film),
    Award = "Director") %>% 
    rename(Name = `Winner and nominees`) 

# Best actor----
data_actor <- read_html("https://en.wikipedia.org/wiki/Chicago_Film_Critics_Association_Award_for_Best_Actor") %>% 
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
    Film = trimws(Film),
    Award = "Actor") %>% 
    rename(Name = `Winner and nominees`) %>% 
    select(-Role)

# Best actress----
data_actress <- read_html("https://en.wikipedia.org/wiki/Chicago_Film_Critics_Association_Award_for_Best_Actress") %>% 
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
    Film = trimws(Film),
    Award = "Actress") %>% 
    rename(Name = `Winner and nominees`) %>% 
    select(-Role)

# Best supporting actor----
data_supactor <- read_html("https://en.wikipedia.org/wiki/Chicago_Film_Critics_Association_Award_for_Best_Supporting_Actor") %>% 
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
    Film = trimws(Film),
    Award = "Supporting Actor") %>% 
    rename(Name = `Winner and nominees`) %>% 
    select(-Role)

# Best supporing actress----
data_supactress <- read_html("https://en.wikipedia.org/wiki/Chicago_Film_Critics_Association_Award_for_Best_Supporting_Actress") %>% 
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
    Film = trimws(Film),
    Award = "Supporing Actress") %>% 
    rename(Name = `Winner and nominees`) %>% 
    select(-Role)

# Put everything together----
bind_rows(
    data_actor,
    data_actress,
    data_supactor,
    data_supactress,
    data_pic,
    data_director,
    # data_foreign,
    # data_doc,
    # data_animated,
    # data_screenplay
) %>% 
    saveRDS("cache/cfca_historical.rds")
