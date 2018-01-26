library(rvest)

dataset <- purrr::map_df(1944:2018, function(year) {
  parsed_doc <- read_html(paste0("https://www.goldenglobes.com/winners-nominees/", year))
  
  list_cat <- parsed_doc %>% 
    html_nodes("ul.dropdown-menu") %>% 
    .[[2]] %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    stringr::str_extract("category-[0-9]*$")
  
  parsed_doc <- read_html(paste0("https://www.goldenglobes.com/winners-nominees/", year, 
                                 "/all#", list_cat[length(list_cat)]))
  
  list_cat %>% 
    purrr::map_df(function(cat_str) {
      xpath_prefix <- paste0("//div[@id='", cat_str, "']")
      # Category name
      cat_name <- parsed_doc %>% 
        html_node(xpath = xpath_prefix) %>% 
        html_text()
      
      # Attribute
      winner_nom <- parsed_doc %>% 
        html_node(xpath = paste0(xpath_prefix, "/ancestor::div[@class='view-grouping']/descendant::div[@class='view-grouping-content']")) %>% 
        html_nodes("div.field-content") %>% 
        html_text
      
      # Primary
      prim_nom <- parsed_doc %>% 
        html_node(xpath = paste0(xpath_prefix, "/ancestor::div[@class='view-grouping']/descendant::div[@class='view-grouping-content']")) %>% 
        html_nodes(".primary-nominee") %>% 
        html_text
      
      # Secondary
      sec_nom <- parsed_doc %>% 
        html_node(xpath = paste0(xpath_prefix, "/ancestor::div[@class='view-grouping']/descendant::div[@class='view-grouping-content']")) %>% 
        html_nodes(".secondary-nominee") %>% 
        html_text
      if (length(sec_nom) == 0 |
          length(sec_nom) != length(prim_nom)) sec_nom <- rep(NA, length(prim_nom))
      
      data.frame(
        Year = year,
        Category = cat_name,
        Winner = as.numeric(winner_nom == "Winner"),
        Name = prim_nom,
        Movie = sec_nom,
        stringsAsFactors = FALSE
      )
    })
})

dataset %>% 
    saveRDS("cache/goldenGlobes_historical.rds")
