library(tidyverse)
library(magrittr)
library(rvest)

# Best feature film----
data_pic <- read_html("https://en.wikipedia.org/wiki/Satellite_Award_for_Best_Film") # %>% 
    # html_nodes(xpath = "//table[@class='wikitable']") %>% 
    # html_table() %>% 
    # bind_rows() %>% 
    # mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    # mutate(Winner = 1,
    # Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    # Film = trimws(Film),
    # Award = "Picture") %>% 
    # rename(Name = Director)

# Best director----
data_director <- read_html("https://en.wikipedia.org/wiki/Satellite_Award_for_Best_Director") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    html_table(fill = TRUE) %>% 
    lapply(function(df) df %>% 
               mutate(Year = stringr::str_sub(Year, 1, 4),
                      Year = as.numeric(Year))
           ) %>% 
    bind_rows() %>% 
    group_by(Year) %>% 
    mutate(Winner = if_else(Film == first(Film),
                            1, 0)) %>% 
    mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", ""),
           Film = trimws(Film),
           Award = "Director",
           Winner = if_else(Film == "Flags of Our Fathers",
                            1, Winner)) %>% 
    rename(Name = `Winners and nominees`) %>% 
    ungroup

# Best actor----
data_actor <- read_html("https://en.wikipedia.org/wiki/Satellite_Award_for_Best_Actor_%E2%80%93_Motion_Picture") # %>% 
    # html_nodes(xpath = "//table[@class='wikitable']") %>% 
    # html_table() %>% 
    # lapply(function(df){
    #     df <- if ("Winner(s)" %in% names(df)){
    #         rename(df, Name = `Winner(s)`)
    #     } else {
    #         rename(df, Name = Winner)
    #     }
    # }) %>% 
    # bind_rows() %>% 
    # mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    # mutate(Winner = 1,
    # Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    # Film = trimws(Film),
    # Award = "Actor") %>% 
    # select(-Role)

# Best actress----
data_actress <- read_html("https://en.wikipedia.org/wiki/Satellite_Award_for_Best_Actress_%E2%80%93_Motion_Picture") # %>% 
    # html_nodes(xpath = "//table[@class='wikitable']") %>% 
    # html_table() %>% 
    # lapply(function(df){
    #     df <- if ("Winner(s)" %in% names(df)){
    #         rename(df, Name = `Winner(s)`)
    #     } else {
    #         rename(df, Name = Winner)
    #     }
    # }) %>% 
    # bind_rows() %>% 
    # mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
    # mutate(Winner = 1,
    #        Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
    #        Film = trimws(Film),
    # Award = "Actress") %>% 
    # select(-Role)

# Best supporting actor----
# data_supactor <- read_html("https://en.wikipedia.org/wiki/Los_Angeles_Film_Critics_Association_Award_for_Best_Supporting_Actor") %>% 
#     html_nodes(xpath = "//table[@class='wikitable']") %>% 
#     html_table() %>% 
#     bind_rows() %>% 
#     rename(Name = Winner) %>% 
#     mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
#     mutate(Winner = 1,
#            Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
#            Film = trimws(Film),
#            Award = "Supporing Actor") %>% 
#     select(-Role)
# 
# # Best supporing actress----
# data_supactress <- read_html("https://en.wikipedia.org/wiki/Los_Angeles_Film_Critics_Association_Award_for_Best_Supporting_Actress") %>% 
#     html_nodes(xpath = "//table[@class='wikitable']") %>% 
#     html_table() %>% 
#     bind_rows() %>% 
#     rename(Name = Winner) %>% 
#     mutate(Film = stringr::str_replace_all(Film, "(\u2020|\u2021)", "")) %>% 
#     mutate(Winner = 1,
#     Film = stringr::str_replace_all(Film, "\\(TIE\\)", ""),
#     Film = trimws(Film),
#     Award = "Supporing Actress") %>% 
#     select(-Role)

# Put everything together----
bind_rows(
    data_actor,
    data_actress,
    # data_supactor,
    # data_supactress,
    data_pic,
    data_director,
    # data_foreign,
    # data_doc,
    # data_animated,
    # data_screenplay
) %>% 
    saveRDS("cache/satellite_historical.rds")
