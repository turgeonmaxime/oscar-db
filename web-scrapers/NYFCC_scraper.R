library(tidyverse)
library(magrittr)
library(rvest)

# Best actor----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=2")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_actor <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Actor")

# Best actress----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=3")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_actress <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Actress")

# Best Supporting actor----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=8")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_supactor <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Supporting Actor")

# Best Supporting actress----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=9")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_supactress <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Supporting Actress")

# Best picture----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=1")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_pic <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Picture")

# Best director----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=4")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_director <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Director")

# Best documentary----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=11")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=13")

years2 <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners2 <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names2 <- winners2[(seq_len(length(winners2)) %% 2) == 1]
films2 <- winners2[(seq_len(length(winners2)) %% 2) == 0]

data_doc <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>%
    bind_rows(
        tibble(
            Year = as.numeric(years2),
            Winner = names2,
            Movie = films2
        )
    ) %>% 
    mutate(Award = "Documentary")

# Best animated----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=16")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_animated <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Animated")

# Best foreign----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=5")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_foreign <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Foreign")

# Best screenplay----
parsed_doc <- read_html("http://www.nyfcc.com/awards/?cat=7")

years <- parsed_doc %>% 
    html_nodes("div.award dl dt") %>% 
    html_text()

winners <- parsed_doc %>% 
    html_nodes("div.award dl dd") %>% 
    html_text()

names <- winners[(seq_len(length(winners)) %% 2) == 1]
films <- winners[(seq_len(length(winners)) %% 2) == 0]

data_screenplay <- tibble(
    Year = as.numeric(years),
    Winner = names,
    Movie = films
) %>% 
    mutate(Award = "Screenplay")

# Put everything together----
bind_rows(
    data_actor,
    data_actress,
    data_supactor,
    data_supactress,
    data_pic,
    data_director,
    data_foreign,
    data_doc,
    data_animated,
    data_screenplay
) %>% 
    saveRDS("cache/nyfcc_historical.rds")
