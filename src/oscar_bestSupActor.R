# Construct dataset for best supporting actor prediction
library(tidyverse)
library(magrittr)
library(stringr)
data_oscar <- readRDS("cache/oscar_historical.rds") %>% 
    bind_rows(readRDS("cache/oscar_nominees.rds") %>% 
                  rename(Film = Movie) %>% 
                  mutate(Winner = -1)) %>% 
    mutate(Award = if_else(
        Award %in% c("Best Supporting Actor", 
                     "Supporting Actor", 
                     "Actor in a Supporting Role"),
        "Best Supporting Actor", Award
    ),
    Name = str_replace_all(Name, "\u00e9", "e"),
    Name = str_replace_all(Name, "\u00e1", "a"),
    Name = str_replace_all(Name, "[A-Z]\\. ", ""),
    Year = as.numeric(if_else(nchar(Year) > 4,
                              str_sub(Year, 6, 9),
                              Year))) %>% 
    filter(Award == "Best Supporting Actor")

# 1. Winner at Golden Globes----
data_gg <- readRDS("cache/goldenGlobes_historical.rds") %>% 
    filter(Category == "Best Performance by an Actor in a Supporting Role in any Motion Picture") %>% 
    select(Year, Category, Winner, Name) %>% 
    mutate(Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""),
           Year = Year - 1)

winner_gg <- data_oscar %>% 
    semi_join(filter(data_gg, 
                     Winner == 1), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(GG = 1)

# 2. Winner at BAFTA----
data_bafta <- readRDS("cache/bafta_historical.rds") %>% 
    filter(Category %in% c("Supporting Actor",
                           "Actor in a Supporting Role")) %>% 
    mutate(Name = Movie,
           Year = Year - 1,
           Name = str_replace_all(Name, "\\(Posthumous\\)", ""),
           Name = str_replace_all(Name, "\u00e7", "c"),
           Name = str_replace_all(Name, "\u00ed", "i"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""),
           Winner = if_else(Name == "Woody Harrelson" & Year == 2017,
                            0, Winner))

winner_bafta <- data_oscar %>% 
    semi_join(filter(data_bafta, 
                     Winner == 1), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(Bafta = 1)

# 3. Winner at SGA----
data_sga <- readRDS("cache/sga_historical.rds") %>% 
    filter(Award == "Supporting Actor") %>% 
    mutate(Name = str_replace_all(Name, "\\(posthumous\\)", ""),
           Name = trimws(Name),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""),
           Winner = if_else(Name == "Woody Harrelson" & Year == 2017,
                            0, Winner))
winner_sga <- data_oscar %>% 
    semi_join(filter(data_sga, 
                     Winner == 1), 
              by = c("Year", "Name")) %>% 
    select(Year, Name) %>% 
    mutate(SGA = 1)

# 5. Winner at CFCA, LAFCA, NBR, NSFC, and NYFCC----
data_cfca <- readRDS("cache/cfca_historical.rds") %>% 
    ungroup %>% 
    filter(Award == "Supporting Actor") %>% 
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
    semi_join(filter(data_cfca, 
                     Winner == 1), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(CFCA = 1)

data_lafca <- readRDS("cache/lafca_historical.rds") %>% 
    filter(Award == "Supporing Actor") %>% 
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
    semi_join(filter(data_lafca, 
                     Winner == 1), 
              by = c("Year", "Name")) %>% 
    select(Year, Name) %>% 
    mutate(LAFCA = 1)

data_nbr <- readRDS("cache/nbr_historical.rds") %>% 
    filter(Award == "Supporting Actor") %>% 
    mutate(Winner = str_split(Winner, "(\n|,)")) %>% 
    unnest() %>% 
    mutate(Winner = str_replace_all(Winner, "\u00a0", ""),
           Winner = trimws(Winner),
           Winner = str_replace_all(Winner, "\u00b4", "'"),
           Winner = str_replace_all(Winner, "\u00f6", "o"),
           Winner = str_replace_all(Winner, "[A-Z]\\. ", ""))
winner_nbr <- data_oscar %>% 
    semi_join(data_nbr,
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NBR = 1)

data_nsfc <- readRDS("cache/nsfc_historical.rds") %>% 
    filter(Award == "Supporting Actor") %>% 
    mutate(Winner = str_replace_all(Winner, "[A-Z]\\. ", ""),
           Winner = str_replace_all(Winner, "\u00e9", "e"))
winner_nsfc <- data_oscar %>% 
    semi_join(data_nsfc, 
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NSFC = 1)

data_nyfcc <- readRDS("cache/nyfcc_historical.rds") %>% 
    filter(Award == "Supporting Actor") %>% 
    mutate(Winner = str_replace_all(Winner, "[A-Z]\\. ", ""),
           Winner = str_replace_all(Winner, "\u00e9", "e"))
winner_nyfcc <- data_oscar %>% 
    semi_join(data_nyfcc, 
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NYFCC = 1)

# Combine everything together----
data_bestsupactor <- data_oscar %>% 
    left_join(winner_gg) %>% 
    left_join(winner_bafta) %>% 
    left_join(winner_sga) %>% 
    left_join(winner_cfca) %>% 
    left_join(winner_lafca) %>% 
    left_join(winner_nbr) %>% 
    left_join(winner_nsfc) %>% 
    left_join(winner_nyfcc) %>% 
    mutate_at(c("Winner", "GG", "Bafta","SGA", 
                "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC"),
              function(col) if_else(is.na(col), 0, col))
saveRDS(data_bestsupactor, "cache/data_bestsupactor_pred.rds")

# PREDICTION----
library(survival)

pred_supactor <- c("GG", "Bafta","SGA", 
                   "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC")
fit_supactor <- clogit(formula(paste("Winner ~", paste(pred_supactor, collapse = "+"), 
                                "+ strata(Year)")),
                  data = data_bestsupactor, subset = Winner != -1)
coef_supactor <- summary(fit_supactor)$coefficients[,1]

current_supactor <- data_bestsupactor %>% 
    filter(Winner == -1)
exp_supactor <- exp(as.matrix(current_supactor[, colnames(current_supactor) %in% pred_supactor]) %*% coef_supactor)
predict_supactor <- exp_supactor/sum(exp_supactor)
rownames(predict_supactor) <- current_supactor %>% pull(Name)
predict_supactor
round(100 * predict_supactor, 2)

# Bootstrap
n.boot <- 10000

cov_supactor <- fit_supactor$var
samples_supactor <- MASS::mvrnorm(n = n.boot, mu = coef_supactor, Sigma = cov_supactor)
boot_prob_supactor <- purrr::map_df(seq_len(n.boot), function(row){
    pred_log <- as.matrix(current_supactor[, colnames(current_supactor) %in% pred_supactor]) %*% samples_supactor[row,] %>% 
        exp %>% 
        t
    colnames(pred_log) <- current_supactor$Name
    pred <- pred_log/sum(pred_log)
    mode(pred) <- "list"
    return(as.data.frame(pred))
}) %>% mutate_all(as.numeric)

boot_prob_supactor %>% 
    sample_n(1000) %>% 
    gather(SupActor, pred) %>% 
    ggplot(aes(y = pred, x = SupActor)) + geom_boxplot() + 
    ylab("Probability") + expand_limits(y = 0) +
    xlab("Supporting Actor") +
    scale_y_continuous(lim = c(0, 1), expand = c(0, 0))

ci_95_supactor <- summarise_all(boot_prob_supactor, function(col) list(quantile(col, probs = c(0.025, 0.975)))) %>% 
    unnest %>%
    mutate(Type = c("LCL", "UCL")) %>% 
    gather(SupActor, Value, -Type) %>% 
    spread(Type, Value)

save(predict_supactor,
     boot_prob_supactor,
     ci_95_supactor,
     file = "results/pred_supactor.RData")

