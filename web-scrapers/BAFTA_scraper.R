library(rvest)
is_empty_str <- function(str) str == ""

dataset <- purrr::map_df(1949:2017, function(year) {
	parsed_doc <- read_html(paste0("http://awards.bafta.org/award/",
                                year, "/film"))
  
  data_foo <- parsed_doc %>% 
    html_nodes(xpath = "//div[@class='view-content']/ul") %>% 
    html_nodes("li") %>% 
    purrr::map_df(function(node) {
    cat_name <-  node %>% 
      html_node("h2") %>% 
      html_text() %>% 
      stringr::str_replace_all(
        pattern = paste(" in", year), 
        replacement = ""
      ) %>% 
      (function(str) stringr::str_split(str,
                                        pattern = " \\| ")[[1]][2])
    
    nominee_name <- node %>% 
      html_nodes("div.search-result-subtitle") %>% 
      html_text() %>% 
      trimws %>% 
      Filter(purrr::negate(is_empty_str), .)
    
    nominee_info <- node %>% 
      html_nodes("div.search-result-headline") %>% 
      html_text() %>% 
      trimws %>% 
      Filter(purrr::negate(is_empty_str), .)
    
    if (length(nominee_name) == 0 | length(nominee_info) > length(nominee_name)) {
      nominee_name <- rep(NA, length(nominee_info))
    }
    if (length(nominee_info) == 0 | length(nominee_info) < length(nominee_name)) {
      nominee_info <- rep(NA, length(nominee_name))
    }
    
    winner <- node %>% 
      html_nodes("div.search-result-winner") %>% 
      html_text() %>% 
      trimws
    
    
    if (length(nominee_info) == 0 && length(nominee_name) == 0) {
      data.frame(
        Year = year,
        Category = cat_name,
        Winner = NA,
        Name = NA,
        Movie = NA,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Year = year,
        Category = cat_name,
        Winner = as.numeric(nominee_name %in% winner |
                              nominee_info %in% winner),
        Name = nominee_name,
        Movie = nominee_info,
        stringsAsFactors = FALSE
      )
    }
  })
})

dataset %>% 
    saveRDS("cache/bafta_historical.rds")