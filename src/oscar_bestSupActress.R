# Construct dataset for best supporting actress prediction
library(tidyverse)
library(magrittr)
library(stringr)
data_oscar <- readRDS("cache/oscar_historical.rds") %>% 
    bind_rows(readRDS("cache/oscar_nominees.rds") %>% 
                  rename(Film = Movie) %>% 
                  mutate(Winner = -1)) %>% 
    mutate(Award = if_else(
        Award %in% c("Best Supporting Actress", 
                     "Supporting Actress", 
                     "Actress in a Supporting Role"),
        "Best Supporting Actress", Award
    ),
    Name = str_replace_all(Name, "\u00e9", "e"),
    Name = str_replace_all(Name, "\u00e1", "a"),
    Name = str_replace_all(Name, "[A-Z]\\. ", ""),
    Year = as.numeric(if_else(nchar(Year) > 4,
                              str_sub(Year, 6, 9),
                              Year))) %>% 
    filter(Award == "Best Supporting Actress")

# 1. Winner at Golden Globes----
data_gg <- readRDS("cache/goldenGlobes_historical.rds") %>% 
    filter(Category == "Best Performance by an Actress in a Supporting Role in any Motion Picture") %>% 
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
    filter(Category %in% c("Supporting Actress",
                           "Actress in a Supporting Role")) %>% 
    mutate(Name = Movie,
           Year = Year - 1,
           Name = str_replace_all(Name, "\\(Posthumous\\)", ""),
           Name = str_replace_all(Name, "\u00e7", "c"),
           Name = str_replace_all(Name, "\u00ed", "i"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""),
           Winner = if_else((Name == "Queen Latifah" & Year == 2002) |
                                (Name == "Jessica Chastain" & Year == 2011),
                            0, Winner))

winner_bafta <- data_oscar %>% 
    semi_join(filter(data_bafta, 
                     Winner == 1), 
              by = c("Name", "Year")) %>% 
    select(Year, Name) %>% 
    mutate(Bafta = 1)

# 3. Winner at SGA----
data_sga <- readRDS("cache/sga_historical.rds") %>% 
    filter(Award == "Supporting Actress") %>% 
    mutate(Name = str_replace_all(Name, "\\(posthumous\\)", ""),
           Name = trimws(Name),
           Name = str_replace_all(Name, "\u00e9", "e"),
           Name = str_replace_all(Name, "\u00fc", "ue"),
           Name = str_replace_all(Name, "\u00e1", "a"),
           Name = str_replace_all(Name, "[A-Z]\\. ", ""),
           Winner = if_else((Name == "Queen Latifah" & Year == 2002) |
                                (Name == "Amy Adams" & Year == 2010)|
                                (Name == "Jessica Chastain" & Year == 2011),
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
    filter(Award == "Supporing Actress") %>% 
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
    filter(Award == "Supporing Actress") %>% 
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
    filter(Award == "Supporting Actress") %>% 
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
    filter(Award == "Supporting Actress") %>% 
    mutate(Winner = str_replace_all(Winner, "[A-Z]\\. ", ""),
           Winner = str_replace_all(Winner, "\u00e9", "e"))
winner_nsfc <- data_oscar %>% 
    semi_join(data_nsfc, 
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NSFC = 1)

data_nyfcc <- readRDS("cache/nyfcc_historical.rds") %>% 
    filter(Award == "Supporting Actress") %>% 
    mutate(Winner = str_replace_all(Winner, "[A-Z]\\. ", ""),
           Winner = str_replace_all(Winner, "\u00e9", "e"))
winner_nyfcc <- data_oscar %>% 
    semi_join(data_nyfcc, 
              by = c("Year", "Name" = "Winner")) %>% 
    select(Year, Name) %>% 
    mutate(NYFCC = 1)

# Combine everything together----
data_bestsupactress <- data_oscar %>% 
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
saveRDS(data_bestsupactress, "cache/data_bestsupactress_pred.rds")

# PREDICTION----
library(survival)

pred_supactress <- c("GG", "Bafta","SGA", 
                   "CFCA", "LAFCA", "NBR", "NSFC", "NYFCC")
fit_supactress <- clogit(formula(paste("Winner ~", paste(pred_supactress, collapse = "+"), 
                                "+ strata(Year)")),
                  data = data_bestsupactress, subset = Winner != -1)
coef_supactress <- summary(fit_supactress)$coefficients[,1]

current_supactress <- data_bestsupactress %>% 
    filter(Winner == -1)
exp_supactress <- exp(as.matrix(current_supactress[, colnames(current_supactress) %in% pred_supactress]) %*% coef_supactress)
predict_supactress <- exp_supactress/sum(exp_supactress)
rownames(predict_supactress) <- current_supactress %>% pull(Name)
predict_supactress
round(100 * predict_supactress)

# Bootstrap
n.boot <- 10000

cov_supactress <- fit_supactress$var
samples_supactress <- MASS::mvrnorm(n = n.boot, mu = coef_supactress, Sigma = cov_supactress)
boot_prob_supactress <- purrr::map_df(seq_len(n.boot), function(row){
    pred_log <- as.matrix(current_supactress[, colnames(current_supactress) %in% pred_supactress]) %*% samples_supactress[row,] %>% 
        exp %>% 
        t
    colnames(pred_log) <- current_supactress$Name
    pred <- pred_log/sum(pred_log)
    mode(pred) <- "list"
    return(as.data.frame(pred))
}) %>% mutate_all(as.numeric)

boot_prob_supactress %>% 
    sample_n(1000) %>% 
    gather(SupActress, pred) %>% 
    ggplot(aes(y = pred, x = SupActress)) + geom_boxplot() + 
    ylab("Probability") + expand_limits(y = 0) +
    xlab("Supporting Actress") +
    scale_y_continuous(lim = c(0, 1), expand = c(0, 0))

ci_95_supactress <- summarise_all(boot_prob_supactress, function(col) list(quantile(col, probs = c(0.025, 0.975)))) %>% 
    unnest %>%
    mutate(Type = c("LCL", "UCL")) %>% 
    gather(SupActress, Value, -Type) %>% 
    spread(Type, Value)

save(predict_supactress,
     boot_prob_supactress,
     ci_95_supactress,
     file = "results/pred_supactress.RData")
