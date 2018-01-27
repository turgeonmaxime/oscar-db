library(tidyverse)
library(magrittr)
library(rvest)

# Best picture----
data_best_pic <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Film") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    html_table() %>% 
    bind_rows() %>% 
    select(-`Director(s)`) %>% 
    mutate(Award = 'Picture', Film = NA_character_)

# Best director----
data_best_dir <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Director") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>% 
    html_table() %>% 
    bind_rows() %>% 
    mutate(Award = 'Director')

# Best actor----
html_table2 <- function(node) {
    years <- node %>% 
        html_nodes("tr") %>% 
        html_node(xpath = "td[@style='text-align:center;']") %>% 
        html_text %>% 
        extract(-1)
    actors <- node %>% 
        html_nodes("tr") %>% 
        html_node("td span a") %>% 
        html_text() %>% 
        extract(-1)
    movies <- node %>% 
        html_nodes("tr") %>% 
        html_node("td i a") %>% 
        html_text() %>% 
        extract(-1)
    
    output <- data.frame(
        Year = years, 
        Winner = actors, 
        Film = movies,
        stringsAsFactors = FALSE
    ) %>% 
        fill(Year, Winner)
    return(output)
}

data_best_actor <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Actor") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>%
    map_df(html_table2) %>% 
    mutate(Award = 'Lead Actor',
           Year= as.numeric(Year))

# Best Actress----
data_best_actress <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Actress") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>%
    html_table() %>% 
    bind_rows() %>% 
    mutate(Award = 'Lead Actress') %>% 
    select(-Role)

# Best supprorting actor----
data_best_supactor <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Supporting_Actor") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>%
    html_table() %>% 
    bind_rows() %>% 
    mutate(Award = 'Supporting Actor') %>% 
    select(-Role)

# Best Actress----
data_best_supactress <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Supporting_Actress") %>% 
    html_nodes(xpath = "//table[@class='wikitable']") %>%
    html_table() %>% 
    bind_rows() %>% 
    mutate(Award = 'Supporting Actress') %>% 
    select(-Role)

# Best screenplay----
data_screenplay <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Screenplay") %>% 
    html_nodes("table.wikitable") %>% 
    html_table() %>% 
    bind_rows() %>% 
    rename(Winner = `Screenwriter(s)`) %>% 
    mutate(Award = 'Screenplay')

# Best documentary----
data_doc <- read_html("https://en.wikipedia.org/wiki/National_Society_of_Film_Critics_Award_for_Best_Non-Fiction_Film") %>% 
    html_nodes("table.wikitable") %>% 
    html_table(fill = TRUE) %>% 
    bind_rows() %>% 
    rename(Year = `Year (ceremony)`) %>% 
    mutate(Award = 'Documentary',
           Year = stringr::str_sub(Year, 1, 4),
           Year = as.numeric(Year),
           Winner = Film, 
           Film = NA_character_) %>% 
    select(-`Director(s)`)

bind_rows(
    data_best_pic,
    data_best_dir,
    data_best_actor,
    data_best_actress,
    data_best_supactor,
    data_best_supactress,
    data_screenplay,
    data_doc
) %>% 
    saveRDS("cache/nsfc_historical.rds")
