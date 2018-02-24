library(omdbapi)
library(tidyverse)
library(magrittr)

list_films <- c("Moonlight",
                "Arrival",
                "Fences",
                "Hacksaw Ridge",
                "Hell or High Water",
                "Hidden Figures",
                "La La Land",
                "Lion",
                "Manchester by the Sea")

foo <- purrr::map_df(list_films, function(film_title) {
    tmp <- search_by_title(film_title, type = "movie") %>%
        mutate(Title = stringr::str_replace(Title, "^'", "")) %>% 
        filter(grepl(paste0("^", film_title, "$"), Title),
               Poster != "N/A") %>%
        group_by(Title) %>%
        summarise(imdbID = first(imdbID)) %>%
        pull(imdbID) %>%
        find_by_id
    if ("Ratings" %in% names(tmp)) tmp %<>% select(-Ratings)
        
    tmp %>%
        distinct %>%
        select(Title, Year, Rated, Released, Runtime, Genre, imdbRating) %>%
        mutate(Runtime = as.numeric(stringr::str_replace(Runtime, " min", "")),
               imdbRating = as.numeric(imdbRating),
               Year = as.numeric(Year),
               Genre = stringr::str_split(Genre, ", ")) %>%
        unnest()
})

