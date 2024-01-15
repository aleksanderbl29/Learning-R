library(tidyverse)
library(ggthemes)
library(scales) # Nødvendigt for at få procent på y akse i bar plot
library(cowplot)

## Specify numeric output
options(scipen = 999)
## Set seed for sampling
set.seed(42)
## Set number of respondents
df_n <- 1200
## Create id for each respondent
id <- seq(1, df_n, by = 1)

## Define treatments
k1 <- "Ufaglært"
k2 <- "Revisor"
econ_0 <- "Ingen policy"
econ_1 <- "Skattelettelse"

## Add treatments
init_df <- id %>%
  tibble(id) %>%
  mutate(econ_pol = as.factor(sample(c(econ_0, econ_1),
                                     as.numeric(length(id)), replace = TRUE)),
         job = as.factor(sample(c(k1, k2),
                                as.numeric(length(id)), replace = TRUE))) %>%
  mutate(dummy_k = if_else(job == k2, 1, 0),
         dummy_econ = if_else(econ_pol == econ_0, 0, 1)) %>%
  mutate(treatment = factor(paste(job, "og", econ_pol)))

## Change levels
# init_df$econ_pol <- fct_rev(init_df$econ_pol)
levels(init_df$econ_pol)
init_df$job <- fct_rev(init_df$job)
levels(init_df$job)


head(init_df)
summary(init_df)
table(init_df$job, init_df$econ_pol)

## Specify treatment groups
t1 <- paste(k1, "og", econ_0)
t2 <- paste(k2, "og", econ_0)
t3 <- paste(k1, "og", econ_1)
t4 <- paste(k2, "og", econ_1)

## Add dependent variable
head(init_df)
unique(init_df$treatment)

## Specify dependent variable options
dep_label <- c("Ingen sympati", "Næsten ingen sympati", "Indifferent", "Sympati for", "Stor sympati for")

## Define probalities for treatment groups
t1_prob <- c(0.08, 0.24, 0.3, 0.28, 0.1)
t2_prob <- c(0.08, 0.15, 0.38, 0.25, 0.14)
t3_prob <- c(0.05, 0.20, 0.33, 0.27, 0.15)
t4_prob <- c(0.05, 0.205, 0.34, 0.27, 0.145)
paste(sum(t1_prob), "    ", mean(t1_prob))
paste(sum(t2_prob), "    ", mean(t2_prob))
paste(sum(t3_prob), "    ", mean(t3_prob))
paste(sum(t4_prob), "    ", mean(t4_prob))


t4_prob - t2_prob

## Assign dependent variables
df_rand_sample <- init_df %>%
  mutate(sympati = ifelse(treatment == t1, sample(-2:2, df_n, replace = TRUE, prob = t1_prob),
                          ifelse(treatment == t2, sample(-2:2, df_n, replace = TRUE, prob = t2_prob),
                                 ifelse(treatment == t3, sample(-2:2, df_n, replace = TRUE, prob = t3_prob),
                                        ifelse(treatment == t4, sample(-2:2, df_n, replace = TRUE, prob = t4_prob), NA)))))

t1_df <- init_df[init_df$treatment == t1, ]
t2_df <- init_df[init_df$treatment == t2, ]
t3_df <- init_df[init_df$treatment == t3, ]
t4_df <- init_df[init_df$treatment == t4, ]

## Assign values for each df
t1_df <- t1_df %>%
  mutate(df_id = seq(1, length(id), by = 1)) %>%
  mutate(sympati = ifelse(df_id <= t1_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (t1_prob[2] * length(df_id)) + (t1_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (t1_prob[3] * length(df_id)) + (t1_prob[2] * length(df_id)) + (t1_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (t1_prob[4] * length(df_id)) + (t1_prob[3] * length(df_id)) + (t1_prob[2] * length(df_id)) + (t1_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (t1_prob[5] * length(df_id)) + (t1_prob[4] * length(df_id)) + (t1_prob[3] * length(df_id)) + (t1_prob[2] * length(df_id)) + (t1_prob[1] * length(df_id)), 2, NA))))))
t2_df <- t2_df %>%
  mutate(df_id = seq(1, length(id), by = 1)) %>%
  mutate(sympati = ifelse(df_id <= t2_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (t2_prob[2] * length(df_id)) + (t2_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (t2_prob[3] * length(df_id)) + (t2_prob[2] * length(df_id)) + (t2_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (t2_prob[4] * length(df_id)) + (t2_prob[3] * length(df_id)) + (t2_prob[2] * length(df_id)) + (t2_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (t2_prob[5] * length(df_id)) + (t2_prob[4] * length(df_id)) + (t2_prob[3] * length(df_id)) + (t2_prob[2] * length(df_id)) + (t2_prob[1] * length(df_id)), 2, NA))))))
t3_df <- t3_df %>%
  mutate(df_id = seq(1, length(id), by = 1)) %>%
  mutate(sympati = ifelse(df_id <= t3_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (t3_prob[2] * length(df_id)) + (t3_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (t3_prob[3] * length(df_id)) + (t3_prob[2] * length(df_id)) + (t3_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (t3_prob[4] * length(df_id)) + (t3_prob[3] * length(df_id)) + (t3_prob[2] * length(df_id)) + (t3_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (t3_prob[5] * length(df_id)) + (t3_prob[4] * length(df_id)) + (t3_prob[3] * length(df_id)) + (t3_prob[2] * length(df_id)) + (t3_prob[1] * length(df_id)), 2, NA))))))
t4_df <- t4_df %>%
  mutate(df_id = seq(1, length(id), by = 1)) %>%
  mutate(sympati = ifelse(df_id <= t4_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (t4_prob[3] * length(df_id)) + (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (t4_prob[4] * length(df_id)) + (t4_prob[3] * length(df_id)) + (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (t4_prob[5] * length(df_id)) + (t4_prob[4] * length(df_id)) + (t4_prob[3] * length(df_id)) + (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), 2, NA))))))


control_df <- t4_df %>%
  mutate(df_id = seq(1, length(id), by = 1)) %>%
  mutate(sympati = ifelse(df_id <= t4_prob[1] * length(df_id), -2,
                          ifelse(df_id <= (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), -1,
                                 ifelse(df_id <= (t4_prob[3] * length(df_id)) + (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), 0,
                                        ifelse(df_id <= (t4_prob[4] * length(df_id)) + (t4_prob[3] * length(df_id)) + (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), 1,
                                               ifelse(df_id <= (t4_prob[5] * length(df_id)) + (t4_prob[4] * length(df_id)) + (t4_prob[3] * length(df_id)) + (t4_prob[2] * length(df_id)) + (t4_prob[1] * length(df_id)), 2, NA))))))
# t1_df %>% ggplot(aes(x = sympati)) + geom_histogram()
# t2_df %>% ggplot(aes(x = sympati)) + geom_histogram()
# t3_df %>% ggplot(aes(x = sympati)) + geom_histogram()
# t4_df %>% ggplot(aes(x = sympati)) + geom_histogram()
## Bind dfs together
t1_ncol <- ncol(t1_df)
t2_ncol <- ncol(t2_df)
t3_ncol <- ncol(t3_df)
t4_ncol <- ncol(t4_df)
surveydata <- rbind(t1_df, t2_df, t3_df, t4_df)

## Define min and max for treatment variable for use in plots
tr_min <- min(surveydata$sympati) - 1
tr_max <- max(surveydata$sympati) + 1

experiment_plot <- surveydata %>% mutate(sympati = sympati + 3) %>% 
  ggplot(aes(x = sympati)) +
  geom_bar(aes(fill = treatment, y = ((after_stat(count))/sum(after_stat(count)))*4)) +
  xlim(0, 6) +
  # xlim(tr_min, tr_max) +
  facet_grid(econ_pol~job) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5))+
  labs(subtitle = "Eksperiment", x = "Sympati", y = "Procent") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20))

experiment_plot

## Plot sympathy for control group
control_plot <- surveydata %>% mutate(sympati = sympati + 3) %>% 
  ggplot(aes(x = sympati)) +
  geom_bar(aes(y = ((after_stat(count))/sum(after_stat(count))))) +
  xlim(0, 6) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5))+
  labs(subtitle = "Kontrolgruppe", x = "Sympati", y = "Procent") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 20))

mean(t1_df$sympati)
mean(t2_df$sympati)
mean(t3_df$sympati)
mean(t4_df$sympati)
mean(surveydata$sympati)

plot_grid(control_plot, experiment_plot)
ggsave(filename = "./private/graph.png", dpi = "print", width = 15, height = 7.5)

# boxgraph <- surveydata %>% mutate(sympati = sympati + 3) %>% 
#   ggplot(aes(x = sympati, fill = treatment)) +
#   geom_boxplot() +
#   facet_grid(econ_pol~job) +
#   labs(subtitle = "Kontrolgruppe", x = "Sympati") +
#   theme(legend.position = "none")
# boxgraph

