library(tidyverse)
library(ggthemes)
library(scales) # Nødvendigt for at få procent på y akse i bar plot

## Specify numeric output
options(scipen = 999)
## Set seed for sampling
set.seed(42)
## Set number of respondents
df_n <- 1200
## Create id for each respondent
id <- seq(1, df_n, by = 1)

## Define treatments
K1 <- "Ufaglært"
K2 <- "Advokat"
Econ_0 <- "Ingen policy"
Econ_1 <- "Bedre vilkår for arbejderklassen"

## Add treatments
create_df <- id %>%
  tibble(id) %>% 
  mutate(econ_pol = as.factor(sample(c(Econ_0, Econ_1),
                                     as.numeric(length(id)), replace = TRUE)),
         job = as.factor(sample(c(K1, K2),
                                as.numeric(length(id)), replace = TRUE))) %>% 
  mutate(dummy_k = if_else(job == K2, 1, 0),
         dummy_econ = if_else(econ_pol == Econ_0, 0, 1)) %>% 
  mutate(treatment = factor(paste(job, "og", econ_pol)))

## Change levels
create_df$econ_pol <- fct_rev(create_df$econ_pol)
levels(create_df$econ_pol)
create_df$job <- fct_rev(create_df$job)
levels(create_df$job)


head(create_df)
summary(create_df)
table(create_df$job, create_df$econ_pol)

## Specify treatment groups
T1 <- paste(K1, "og", Econ_0)
T2 <- paste(K2, "og", Econ_0)
T3 <- paste(K1, "og", Econ_1)
T4 <- paste(K2, "og", Econ_1)

## Add dependent variable
head(create_df)
unique(create_df$treatment)

## Specify dependent variable options
dep_label <- c("Ingen sympati", "Næsten ingen sympati", "Indifferent", "Sympati for", "Stor sympati for")

## Define probalities for treatment groups
T1_prob <- c(0.05, 0.30, 0.3, 0.25, 0.1)
T2_prob <- c(0.08, 0.15, 0.35, 0.28, 0.14)
T3_prob <- c(0.05, 0.15, 0.36, 0.29, 0.15)
T4_prob <- c(0.05, 0.15, 0.36, 0.29, 0.15)
paste(sum(T1_prob), "    ", mean(T1_prob))
paste(sum(T2_prob), "    ", mean(T2_prob))
paste(sum(T3_prob), "    ", mean(T3_prob))
paste(sum(T4_prob), "    ", mean(T4_prob))

## Assign dependent variables
df_rand_sample <- create_df %>% 
  mutate(sympati = ifelse(treatment == T1, sample(-2:2, df_n, replace = TRUE, prob = T1_prob),
                          ifelse(treatment == T2, sample(-2:2, df_n, replace = TRUE, prob = T2_prob), 
                                 ifelse(treatment == T3, sample(-2:2, df_n, replace = TRUE, prob = T3_prob),
                                        ifelse(treatment == T4, sample(-2:2, df_n, replace = TRUE, prob = T4_prob), NA)))))

T1_df <- create_df[create_df$treatment == T1, ]
T2_df <- create_df[create_df$treatment == T2, ]
T3_df <- create_df[create_df$treatment == T3, ]
T4_df <- create_df[create_df$treatment == T4, ]

## Assign values for each df
T1_df <- T1_df %>% 
  mutate(df_id = seq(1, length(id), by = 1)) %>% 
  mutate(sympati = ifelse(df_id <= T1_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (T1_prob[2] * length(df_id)) + (T1_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (T1_prob[3] * length(df_id)) + (T1_prob[2] * length(df_id)) + (T1_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (T1_prob[4] * length(df_id)) + (T1_prob[3] * length(df_id)) + (T1_prob[2] * length(df_id)) + (T1_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (T1_prob[5] * length(df_id)) + (T1_prob[4] * length(df_id)) + (T1_prob[3] * length(df_id)) + (T1_prob[2] * length(df_id)) + (T1_prob[1] * length(df_id)), 2, NA))))))
T2_df <- T2_df %>% 
  mutate(df_id = seq(1, length(id), by = 1)) %>% 
  mutate(sympati = ifelse(df_id <= T2_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (T2_prob[2] * length(df_id)) + (T2_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (T2_prob[3] * length(df_id)) + (T2_prob[2] * length(df_id)) + (T2_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (T2_prob[4] * length(df_id)) + (T2_prob[3] * length(df_id)) + (T2_prob[2] * length(df_id)) + (T2_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (T2_prob[5] * length(df_id)) + (T2_prob[4] * length(df_id)) + (T2_prob[3] * length(df_id)) + (T2_prob[2] * length(df_id)) + (T2_prob[1] * length(df_id)), 2, NA))))))
T3_df <- T3_df %>% 
  mutate(df_id = seq(1, length(id), by = 1)) %>% 
  mutate(sympati = ifelse(df_id <= T3_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (T3_prob[2] * length(df_id)) + (T3_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (T3_prob[3] * length(df_id)) + (T3_prob[2] * length(df_id)) + (T3_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (T3_prob[4] * length(df_id)) + (T3_prob[3] * length(df_id)) + (T3_prob[2] * length(df_id)) + (T3_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (T3_prob[5] * length(df_id)) + (T3_prob[4] * length(df_id)) + (T3_prob[3] * length(df_id)) + (T3_prob[2] * length(df_id)) + (T3_prob[1] * length(df_id)), 2, NA))))))
T4_df <- T4_df %>% 
  mutate(df_id = seq(1, length(id), by = 1)) %>% 
  mutate(sympati = ifelse(df_id <= T4_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (T4_prob[2] * length(df_id)) + (T4_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (T4_prob[3] * length(df_id)) + (T4_prob[2] * length(df_id)) + (T4_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (T4_prob[4] * length(df_id)) + (T4_prob[3] * length(df_id)) + (T4_prob[2] * length(df_id)) + (T4_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (T4_prob[5] * length(df_id)) + (T4_prob[4] * length(df_id)) + (T4_prob[3] * length(df_id)) + (T4_prob[2] * length(df_id)) + (T4_prob[1] * length(df_id)), 2, NA))))))

# T1_df %>% ggplot(aes(x = sympati)) + geom_histogram()
# T2_df %>% ggplot(aes(x = sympati)) + geom_histogram()
# T3_df %>% ggplot(aes(x = sympati)) + geom_histogram()
# T4_df %>% ggplot(aes(x = sympati)) + geom_histogram()
## Bind dfs together
T1_ncol <- ncol(T1_df)
T2_ncol <- ncol(T2_df)
T3_ncol <- ncol(T3_df)
T4_ncol <- ncol(T4_df)
surveydata <- rbind(T1_df, T2_df, T3_df, T4_df)

## Define min and max for treatment variable for use in plots
tr_min <- min(surveydata$sympati) - 1
tr_max <- max(surveydata$sympati) + 1

surveydata %>% ggplot(aes(x = sympati)) +
  geom_bar(aes(fill = treatment, y = ((after_stat(count))/sum(after_stat(count)))*4)) +
  xlim(tr_min, tr_max) +
  facet_grid(econ_pol~job) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  labs(x = "Sympati for kandidat", y = "Procent")
