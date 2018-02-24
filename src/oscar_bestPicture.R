# Construct dataset for best picture prediction
library(tidyverse)
library(magrittr)
data_oscar <- readRDS("cache/oscar_historical.rds") %>% 
    bind_rows(readRDS("cache/oscar_nominees.rds") %>% 
                  rename(Film = Movie) %>% 
                  mutate(Winner = -1)) %>% 
    mutate(Award = if_else(
        Award %in% c("Best Motion Picture",
                     "Best Picture",
                     "Outstanding Motion Picture",
                     "Outstanding Picture"),
        "Best Picture", Award
    ),
    Name = if_else(
        Award == "Best Picture" & Year %in% 2016:2017, 
        Film, Name
    ),
    Winner = if_else(Name == "MA*S*H", 0, Winner))

# 1. Number of nominations----
num_nominations <- readRDS("cache/data_num_nominations.rds")

# 2. Winner at Golden Globes----
data_gg <- readRDS("cache/goldenGlobes_historical.rds") %>% 
    filter(Category %in% c("Best Motion Picture - Drama",
                           "Best Motion Picture - Musical or Comedy",
                           "Picture", "Picture - Comedy", "Picture - Musical")) %>% 
    mutate(Category = case_when(
        Category == "Picture" ~ "Best Motion Picture - Drama",
        Category %in% c("Picture - Comedy", "Picture - Musical") ~ "Best Motion Picture - Musical or Comedy",
        TRUE ~ Category
    ),
    Title = if_else(is.na(Movie), Name, Movie)) %>% 
    select(Year, Category, Winner, Title)

winner_Gdr <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_gg, 
                     Winner == 1,
                     Category == "Best Motion Picture - Drama"), 
              by = c("Name" = "Title")) %>% 
    select(Year, Name) %>% 
    mutate(Gdr = 1)
winner_Gmc <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_gg, 
                     Winner == 1,
                     Category == "Best Motion Picture - Musical or Comedy"), 
              by = c("Name" = "Title")) %>% 
    select(Year, Name) %>% 
    mutate(Gmc = 1)

# 3. Winner at BAFTA----
data_bafta <- readRDS("cache/bafta_historical.rds") %>% 
    filter(Category %in% c("Best Film",
                           "Film",
                           "Film And British Film"))
winner_bafta <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_bafta, 
                     Winner == 1), 
              by = c("Name" = "Movie")) %>% 
    select(Year, Name) %>% 
    mutate(Bafta = 1)

# 4. Winner at PGA, DGA, and SGA----
data_pga <- readRDS("cache/pga_historical.rds") %>% 
    filter(Award == "Feature Film")
winner_pga <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_pga, 
                     Winner == 1), 
              by = c("Name" = "Film")) %>% 
    select(Year, Name) %>% 
    mutate(PGA = 1)

data_dga <- readRDS("cache/dga_historical.rds") %>% 
    filter(Award == "Feature Film")
winner_dga <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_dga, 
                     Winner == 1), 
              by = c("Name" = "Movie")) %>% 
    select(Year, Name) %>% 
    mutate(DGA = 1)

data_sga <- readRDS("cache/sga_historical.rds") %>% 
    filter(Award == "Ensemble")
winner_sga <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_sga, 
                     Winner == 1), 
              by = c("Name" = "Film")) %>% 
    select(Year, Name) %>% 
    mutate(SGA = 1)

# 5. Winner at CFCA, LAFCA, NBR, NSFC, and NYFCC----
data_cfca <- readRDS("cache/cfca_historical.rds") %>% 
    ungroup %>% 
    filter(Award == "Picture")
winner_cfca <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_cfca, 
                     Winner == 1), 
              by = c("Name" = "Film")) %>% 
    select(Year, Name) %>% 
    mutate(CFCA = 1)

data_lafca <- readRDS("cache/lafca_historical.rds") %>% 
    filter(Award == "Picture")
winner_lafca <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(filter(data_lafca, 
                     Winner == 1), 
              by = c("Name" = "Film")) %>% 
    select(Year, Name) %>% 
    mutate(LAFCA = 1)

data_nbr <- readRDS("cache/nbr_historical.rds") %>% 
    filter(Award == "Picture") %>% 
    mutate(Movie = stringr::str_replace_all(Movie, "\u00A0", ""),
           Movie = trimws(Movie))
winner_nbr <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(data_nbr,
              by = c("Name" = "Movie")) %>% 
    select(Year, Name) %>% 
    mutate(NBR = 1)

data_nsfc <- readRDS("cache/nsfc_historical.rds") %>% 
    filter(Award == "Picture")
winner_nsfc <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(data_nsfc, 
              by = c("Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NSFC = 1)

data_nyfcc <- readRDS("cache/nyfcc_historical.rds") %>% 
    filter(Award == "Picture")
winner_nyfcc <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    semi_join(data_nyfcc, 
              by = c("Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NYFCC = 1)

# Combine everything together----
data_bestpic <- data_oscar %>% 
    filter(Award == "Best Picture") %>% 
    left_join(num_nominations, c("Year", "Name" = "Film")) %>% 
    left_join(winner_Gdr) %>% 
    left_join(winner_Gmc) %>% 
    left_join(winner_bafta) %>% 
    left_join(winner_pga) %>% 
    left_join(winner_dga) %>% 
    left_join(winner_sga) %>% 
    left_join(winner_cfca) %>% 
    left_join(winner_lafca) %>% 
    left_join(winner_nbr) %>% 
    left_join(winner_nsfc) %>% 
    left_join(winner_nyfcc) %>% 
    mutate_at(c("Winner", "Gdr", "Gmc", "Bafta", "PGA", "DGA",
                "SGA", "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC"),
              function(col) if_else(is.na(col), 0, col))
saveRDS(data_bestpic, "cache/data_bestpic_pred.rds")

# PREDICTION----
library(survival)

pred_pic <- c("num_nom", "Gdr", "Gmc", "Bafta", "PGA", "DGA",
              "SGA", "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC")

fit_pic <- clogit(formula(paste("Winner ~", paste(pred_pic, collapse = "+"), 
                                "+ strata(Year)")),
                  data = data_bestpic, subset = Winner != -1)
coef_pic <- summary(fit_pic)$coefficients[,1]

current_pic <- data_bestpic %>% 
    filter(Winner == -1)
exp_pic <- exp(as.matrix(current_pic[, colnames(current_pic) %in% pred_pic]) %*% coef_pic)
predict_pic <- exp_pic/sum(exp_pic)
rownames(predict_pic) <- current_pic %>% pull(Name)
predict_pic
round(100 * predict_pic, 2)

# Bootstrap
n.boot <- 10000

cov_pic <- fit_pic$var
samples_pic <- MASS::mvrnorm(n = n.boot, mu = coef_pic, Sigma = cov_pic)
boot_prob_pic <- purrr::map_df(seq_len(n.boot), function(row){
    pred_log <- as.matrix(current_pic[, colnames(current_pic) %in% pred_pic]) %*% samples_pic[row,] %>% 
        exp %>% 
        t
    colnames(pred_log) <- current_pic$Name
    pred <- pred_log/sum(pred_log)
    mode(pred) <- "list"
    return(as.data.frame(pred))
}) %>% mutate_all(as.numeric)

boot_prob_pic %>% 
    sample_n(1000) %>% 
    rename(`The Shape of\nWater` = `The Shape of Water`,
           `Three Billboards\nOutside Ebbing, Missouri` = `Three Billboards Outside Ebbing, Missouri`) %>% 
    gather(Movie, pred) %>% 
    ggplot(aes(y = pred, x = Movie)) + geom_boxplot() + 
    ylab("Probability") + expand_limits(y = 0) +
    scale_y_continuous(lim = c(0, 1), expand = c(0, 0))

ci_95_pic <- summarise_all(boot_prob_pic, function(col) list(quantile(col, probs = c(0.025, 0.975)))) %>% 
    unnest %>%
    mutate(Type = c("LCL", "UCL")) %>% 
    gather(Movie, Value, -Type) %>% 
    spread(Type, Value)

save(predict_pic,
     boot_prob_pic,
     ci_95_pic,
     file = "results/pred_pic.RData")