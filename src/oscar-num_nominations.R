# Create extra variables for prediction
library(tidyverse)
library(magrittr)

data_oscar <- readRDS("cache/oscar_historical.rds") %>% 
    bind_rows(readRDS("cache/oscar_nominees.rds") %>% 
                  rename(Film = Movie)) %>% 
    mutate(Award = case_when(
        Award %in% c("Actor", "Actor in a Leading Role") ~ "Best Actor",
        Award == "Actor in a Supporting Role" ~ "Best Supporting Actor",
        Award %in% c("Actress", "Actress in a Leading Role") ~ "Best Actress",
        Award == "Actress in a Supporting Role" ~ "Best Supporting Actress",
        TRUE ~ Award
    ),
    Award = stringr::str_replace_all(Award, "Best ", ""),
    Film = case_when(
        Year < 2016 & Award == "Foreign Language Film" ~ Name,
        !Ceremony %in% c(1,2,89) & grepl("Cinematography", Award) ~ Name,
        Year < 2016 & grepl("Costume Design", Award) ~ Name,
        grepl("Music", Award) ~ Name,
        !Ceremony %in% c(1,2,89) & grepl("Writing", Award) ~ Name,
        !Ceremony %in% c(4,5,89) & grepl("Sound", Award) ~ Name,
        grepl("Special Effects", Award) ~ Name,
        grepl("Visual Effects", Award) ~ Name,
        grepl("Picture", Award) ~ Name,
        Award == "Outstanding Production" ~ Name,
        Award == "Makeup" ~ Name,
        !Ceremony %in% 7:10 & Award == "Assitant Director" ~ Name,
        !Ceremony %in% c(1,2) & grepl("Art Direction", Award) ~ Name,
        Year < 2016 & Award == "Film Editing" ~ Name,
        TRUE ~ Film
    ),
    Name2 = if_else(Award == "Directing", Film, Name),
    Film = if_else(Award == "Directing", Name, Film),
    Name = Name2) %>% 
    dplyr::select(-Name2) %>% 
    filter(!grepl("short", Award, ignore.case = TRUE),
           Award != "Special Award")

# Need to find a way to add the best song nominations...
num_nominations <- data_oscar %>% 
    filter(!is.na(Film)) %>% 
    group_by(Year, Film) %>% 
    summarise(num_nom = n())

saveRDS(num_nominations, "cache/data_num_nominations.rds")
