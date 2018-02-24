# Construct dataset for best director prediction
library(tidyverse)
library(magrittr)
library(stringr)
data_oscar <- readRDS("cache/oscar_historical.rds") %>% 
    bind_rows(readRDS("cache/oscar_nominees.rds") %>% 
                  rename(Film = Movie) %>% 
                  mutate(Winner = -1)) %>% 
    mutate(Award = if_else(
        Award %in% c("Best Director", 
                     "Directing", 
                     "Directing (Comedy Picture)", 
                     "Directing (Dramatic Picture)"),
        "Best Director", Award
    ),
    Name = if_else(Ceremony %in% 4:88, Film, Name),
    Name = str_replace_all(Name, " \\[(Second|Third)\\]", ""),
    Name = str_replace_all(Name, "\u00f3", "o"),
    Name = str_replace_all(Name, "\u00f1", "n"),
    Name = str_replace_all(Name, "\u00e1", "a"),
    Name = str_replace_all(Name, "\u00fc", "ue"),
    Name = str_replace_all(Name, "\u00e9", "e"),
    Name = str_replace_all(Name, "\u00f6", "o"),
    Name = str_replace_all(Name, "[A-Z]\\. ", ""),
    Year = as.numeric(if_else(nchar(Year) > 4,
                              str_sub(Year, 6, 9),
                              Year)))

# 1. Winner at Golden Globes----
data_gg <- readRDS("cache/goldenGlobes_historical.rds") %>% 
    filter(Category == "Best Director - Motion Picture") %>% 
    select(Year, Category, Winner, Name) %>% 
    mutate(Name = str_replace_all(Name, "\u00f3", "o"),
           Name = str_replace_all(Name, "\u00f1", "n"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Year = Year - 1)

winner_gg <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(filter(data_gg, 
                     Winner == 1,
                     Category == "Best Director - Motion Picture"), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(GG = 1)

# 2. Winner at BAFTA----
data_bafta <- readRDS("cache/bafta_historical.rds") %>% 
    filter(Category %in% c("Achievement in Direction",
                           "David Lean Award for Achievement in Direction",
                           "Direction",
                           "Director")) %>% 
    mutate(Name = if_else(Year %in% c(1987:1991, 2008:2017),
                          Movie, Name),
           Year = Year - 1,
           Name = str_replace_all(Name, "\u00f3", "o"),
           Name = str_replace_all(Name, "\u00f1", "n"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "\u00f6", "o"),
           Name = str_replace_all(Name, "\u00e7", "c"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""))

winner_bafta <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(filter(data_bafta, 
                     Winner == 1), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(Bafta = 1)

# 3. Winner at DGA----
data_dga <- readRDS("cache/dga_historical.rds") %>% 
    filter(Award == "Feature Film") %>% 
    mutate(Director = trimws(Director),
           Director = str_replace_all(Director, "\u00f3", "o"),
           Director = str_replace_all(Director, "\u00f1", "n"),
           Director = str_replace_all(Director, "\u00e1", "a"),
           Director = str_replace_all(Director, "\u00fc", "ue"),
           Director = str_replace_all(Director, "\u00e9", "e"),
           Director = str_replace_all(Director, "\u00f6", "o"),
           Director = str_replace_all(Director, "\u00e7", "c"),
           Director = str_replace_all(Director, "\u0161", "s"),
           Director = str_replace_all(Director, "\u0159", "r"),
           Director = str_replace_all(Director, "\u00ed", "i"),
           Director = str_replace_all(Director, "[A-Z]\\. ", ""))
winner_dga <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(filter(data_dga, 
                     Winner == 1), 
              by = c("Year", "Name" = "Director")) %>% 
    select(Year, Name) %>% 
    mutate(DGA = 1)

# 5. Winner at CFCA, LAFCA, NBR, NSFC, and NYFCC----
data_cfca <- readRDS("cache/cfca_historical.rds") %>% 
    ungroup %>% 
    filter(Award == "Director") %>% 
    mutate(Name = trimws(Name),
           Name = str_replace_all(Name, "\u00f3", "o"),
           Name = str_replace_all(Name, "\u00f1", "n"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "\u00f6", "o"),
           Name = str_replace_all(Name, "\u00e7", "c"),
           Name = str_replace_all(Name, "\u0161", "s"),
           Name = str_replace_all(Name, "\u0159", "r"),
           Name = str_replace_all(Name, "\u00ed", "i"),
           Name = str_replace_all(Name, "\u015b", "s"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""))
winner_cfca <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(filter(data_cfca, 
                     Winner == 1), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(CFCA = 1)

data_lafca <- readRDS("cache/lafca_historical.rds") %>% 
    filter(Award == "Director") %>% 
    mutate(Name = trimws(Name),
           Name = str_replace_all(Name, "\u00f3", "o"),
           Name = str_replace_all(Name, "\u00f1", "n"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "\u00f6", "o"),
           Name = str_replace_all(Name, "\u00e7", "c"),
           Name = str_replace_all(Name, "\u0161", "s"),
           Name = str_replace_all(Name, "\u0159", "r"),
           Name = str_replace_all(Name, "\u00ed", "i"),
           Name = str_replace_all(Name, "\u015b", "s"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""))
winner_lafca <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(filter(data_lafca, 
                     Winner == 1), 
              by = c("Year", "Name")) %>% 
    select(Year, Name) %>% 
    mutate(LAFCA = 1)

data_nbr <- readRDS("cache/nbr_historical.rds") %>% 
    filter(Award == "Director") %>% 
    mutate(Winner = str_replace_all(Winner, "\u00a0", ""),
           Winner = trimws(Winner),
           Winner = str_replace_all(Winner, "[A-Z]\\. ", ""))
winner_nbr <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(data_nbr,
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NBR = 1)

data_nsfc <- readRDS("cache/nsfc_historical.rds") %>% 
    filter(Award == "Director") %>% 
    mutate(Winner = str_replace_all(Winner, "[A-Z]\\. ", ""),
           Winner = str_replace_all(Winner, "\u00f1", "n"),
           Winner = str_replace_all(Winner, "\u00e7", "c"))
winner_nsfc <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(data_nsfc, 
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NSFC = 1)

data_nyfcc <- readRDS("cache/nyfcc_historical.rds") %>% 
    filter(Award == "Director") %>% 
    mutate(Winner = str_replace_all(Winner, "[A-Z]\\. ", ""),
           Winner = str_replace_all(Winner, "\u00f1", "n"),
           Winner = str_replace_all(Winner, "\u00e7", "c"))
winner_nyfcc <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    semi_join(data_nyfcc, 
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NYFCC = 1)

# Combine everything together----
data_bestdir <- data_oscar %>% 
    filter(Award == "Best Director") %>% 
    left_join(winner_gg) %>% 
    left_join(winner_bafta) %>% 
    left_join(winner_dga) %>% 
    left_join(winner_cfca) %>% 
    left_join(winner_lafca) %>% 
    left_join(winner_nbr) %>% 
    left_join(winner_nsfc) %>% 
    left_join(winner_nyfcc) %>% 
    mutate_at(c("Winner", "GG", "Bafta", "DGA",
                "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC"),
              function(col) if_else(is.na(col), 0, col))
saveRDS(data_bestdir, "cache/data_bestdir_pred.rds")

# PREDICTION----
library(survival)

pred_dir <- c("GG", "Bafta", "DGA",
              "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC")
fit_dir <- clogit(formula(paste("Winner ~", paste(pred_dir, collapse = "+"), 
                                "+ strata(Year)")),
                  data = data_bestdir, subset = Winner != -1)
coef_dir <- summary(fit_dir)$coefficients[,1]

current_dir <- data_bestdir %>% 
    filter(Winner == -1)
exp_dir <- exp(as.matrix(current_dir[, colnames(current_dir) %in% pred_dir]) %*% coef_dir)
predict_dir <- exp_dir/sum(exp_dir)
rownames(predict_dir) <- current_dir %>% pull(Name)
predict_dir
round(100 * predict_dir, 2)

# Bootstrap
n.boot <- 10000

cov_dir <- fit_dir$var
samples_dir <- MASS::mvrnorm(n = n.boot, mu = coef_dir, Sigma = cov_dir)
boot_prob_dir <- purrr::map_df(seq_len(n.boot), function(row){
    pred_log <- as.matrix(current_dir[, colnames(current_dir) %in% pred_dir]) %*% samples_dir[row,] %>% 
        exp %>% 
        t
    colnames(pred_log) <- current_dir$Name
    pred <- pred_log/sum(pred_log)
    mode(pred) <- "list"
    return(as.data.frame(pred))
}) %>% mutate_all(as.numeric)

boot_prob_dir %>% 
    sample_n(1000) %>% 
    gather(Director, pred) %>% 
    ggplot(aes(y = pred, x = Director)) + geom_boxplot() + 
    ylab("Probability") + expand_limits(y = 0) +
    scale_y_continuous(lim = c(0, 1), expand = c(0, 0))

ci_95_dir <- summarise_all(boot_prob_dir, function(col) list(quantile(col, probs = c(0.025, 0.975)))) %>% 
    unnest %>%
    mutate(Type = c("LCL", "UCL")) %>% 
    gather(Director, Value, -Type) %>% 
    spread(Type, Value)

save(predict_dir,
     boot_prob_dir,
     ci_95_dir,
     file = "results/pred_dir.RData")
