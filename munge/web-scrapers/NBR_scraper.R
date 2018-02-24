library(tidyverse)
library(magrittr)
library(rvest)

dataset <- purrr::map_df(seq(1932, 2017), function(year) {
    parsed_doc <- read_html(paste0("http://www.nationalboardofreview.org/award-years/",
                                   year))
    
    data_pic <- tibble(
        Award = "Picture",
        Movie = parsed_doc %>% 
            html_node("article.award-names-best-film") %>% 
            html_node("p.films") %>% 
            html_text %>% 
            trimws,
        Winner = NA_character_
    )
    # There was only best picture until 1945
    if (year < 1945) {
        data_full <- data_pic
    } else{
        data_director <- tibble(
            Award = "Director",
            Movie = parsed_doc %>% 
                html_node("article.award-names-best-director") %>% 
                html_node("p.films") %>% 
                html_text %>% 
                trimws,
            Winner = parsed_doc %>% 
                html_node("article.award-names-best-director") %>% 
                html_node("p.recipients") %>% 
                html_text %>% 
                trimws
        )
        
        data_actor <- parsed_doc %>% 
            html_nodes("article.award-names-best-actor") %>% 
            purrr::map_df(function(x) tibble(
                Award = "Actor",
                Movie = x %>% html_node("p.films") %>% 
                    html_nodes("a") %>% 
                    html_text %>% 
                    paste(collapse = ", "),
                Winner = x %>% 
                    html_node("p.recipients") %>% 
                    html_text %>% 
                    trimws
            ))
        
        data_actress <- if (year %in% c(1949, 1981)) {
            # There was no winner in 1949 and 1981
            tibble(Award = "Actress",
                   Movie = NA_character_,
                   Winner = NA_character_
            )
        } else if (year == 1982) {
            # For 1982, the formatting is different
            tibble(
                Award = "Actress",
                Movie = parsed_doc %>% 
                    html_node("article#post-1958") %>% 
                    html_node("p.films") %>% 
                    html_nodes("a") %>% 
                    html_text %>% 
                    paste(collapse = ", "),
                Winner = parsed_doc %>% 
                    html_node("article#post-1958") %>% 
                    html_node("p.recipients") %>% 
                    html_text %>% 
                    trimws
            )
        } else {
            tibble(
                Award = "Actress",
                Movie = parsed_doc %>% 
                    html_node("article.award-names-best-actress") %>% 
                    html_node("p.films") %>% 
                    html_nodes("a") %>% 
                    html_text %>% 
                    paste(collapse = ", "),
                Winner = parsed_doc %>% 
                    html_node("article.award-names-best-actress") %>% 
                    html_node("p.recipients") %>% 
                    html_text %>% 
                    trimws
            )
        }
        
        if (year >= 2003) {
            # The distinction between original and adapted started in 2003
            data_original <- tibble(
                Award = "Original Screenplay",
                Movie = parsed_doc %>% 
                    html_node("article.award-names-best-original-screenplay") %>% 
                    html_node("p.films") %>% 
                    html_nodes("a") %>% 
                    html_text %>% 
                    paste(collapse = ", "),
                Winner = parsed_doc %>% 
                    html_node("article.award-names-best-original-screenplay") %>% 
                    html_node("p.recipients") %>% 
                    html_text %>% 
                    trimws
            )
            
            data_adapted <- tibble(
                Award = "Adapted Screenplay",
                Movie = parsed_doc %>% 
                    html_node("article.award-names-best-adapted-screenplay") %>% 
                    html_node("p.films") %>% 
                    html_nodes("a") %>% 
                    html_text %>% 
                    paste(collapse = ", "),
                Winner = parsed_doc %>% 
                    html_node("article.award-names-best-adapted-screenplay") %>% 
                    html_node("p.recipients") %>% 
                    html_text %>% 
                    trimws
            )
            data_screenplay <- data_original %>% 
                bind_rows(data_adapted)
        } else if (year >= 1998) {
            # Awards for screenplay started in 1998
            data_screenplay <- tibble(
                Award = "Screenplay",
                Movie = parsed_doc %>% 
                    html_node("article.award-names-best-screenplay") %>% 
                    html_node("p.films") %>% 
                    html_nodes("a") %>% 
                    html_text %>% 
                    paste(collapse = ", "),
                Winner = parsed_doc %>% 
                    html_node("article.award-names-best-screenplay") %>% 
                    html_node("p.recipients") %>% 
                    html_text %>% 
                    trimws
            )
        }
        
        data_full <- bind_rows(
            data_pic,
            data_director,
            data_actor,
            data_actress,
        ) 
        
        # Animated features started in 2000
        if (year >= 2000) data_full %<>% bind_rows(
            tibble(
                Award = "Animated",
                Movie = parsed_doc %>% 
                    html_node("article.award-names-best-animated-feature") %>% 
                    html_node("p.films") %>% 
                    html_text %>% 
                    trimws,
                Winner = NA_character_
            )
        )
        # Screenplay awards started in 1998
        if (year >= 1998) data_full %<>% bind_rows(data_screenplay)
        # Documentary features started in 1992
        if (year >= 1992) data_full %<>% bind_rows(
            tibble(
                Award = "Documentary",
                Movie = parsed_doc %>% 
                    html_node("article.award-names-best-documentary") %>% 
                    html_node("p.films") %>% 
                    html_text %>% 
                    trimws,
                Winner = NA_character_
            )
        )
        # Supporting roles started in 1954
        if (year >= 1954) {
            data_supactor <- parsed_doc %>% 
                html_nodes("article.award-names-best-supporting-actor") %>% 
                purrr::map_df(function(x) tibble(
                    Award = "Supporting Actor",
                    Movie = x %>% html_node("p.films") %>% 
                        html_nodes("a") %>% 
                        html_text %>% 
                        paste(collapse = ", "),
                    Winner = x %>% 
                        html_node("p.recipients") %>% 
                        html_text %>% 
                        trimws
                ))
            
            data_supactress <- if (year %in% c(1973, 1982)) {
                # There was no award in 1973 and 1982
                tibble(Award = "Supporting Actress",
                       Movie = NA_character_,
                       Winner = NA_character_
                )
            } else {
                tibble(
                    Award = "Supporting Actress",
                    Movie = parsed_doc %>% 
                        html_node("article.award-names-best-supporting-actress") %>% 
                        html_node("p.films") %>% 
                        html_nodes("a") %>% 
                        html_text %>% 
                        paste(collapse = ", "),
                    Winner = parsed_doc %>% 
                        html_node("article.award-names-best-supporting-actress") %>% 
                        html_node("p.recipients") %>% 
                        html_text %>% 
                        trimws
                )
            }
            data_full %<>% bind_rows(data_supactor,
                                     data_supactress)
        }
        # Foreign features started in 1950
        if (year >= 1950) {
            data_full %<>% bind_rows(
                data_foreign <- tibble(
                    Award = "Foreign",
                    Movie = parsed_doc %>% 
                        html_node("article.award-names-best-foreign-language-film") %>% 
                        html_node("p.films") %>% 
                        html_text %>% 
                        trimws,
                    Winner = NA_character_
                )
            )
        }
    }
    
    data_full %>% 
        mutate(Year = year)
})

dataset %>% 
    mutate(Winner = trimws(Winner), 
           Movie = trimws(Movie),
           Award = if_else(grepl("Sylvia Sidney", Winner),
                           "Supporting Actress", Award)) %>% 
    saveRDS("cache/nbr_historical.rds")

# Winner == "Sylvia Sidney" --> Award = "Supporting Actress"
