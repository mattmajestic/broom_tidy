library(dplyr)
library(tibble)
library(forcats)
library(broom)
library(tidyr)
library(purrr)
library(stringr)
library(knitr)
library(ggfortify)
library(plotly)
library(ggplot2)
data(iris)

### Using dplyr + broom to fit regressions and predict into tidy tibble ###
## Sources include: ##
# https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr #
# http://varianceexplained.org/r/broom-intro/ #

######### Do a single Regression on the dataset #########

# adds the lm function as a column to your tibble as fit #
iris_lms <- 
  iris %>% 
  do(fit = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = .))

# gives a tibble of term, estimate,std.error,statistic, & p value of variables in model #
iris_coefs <- tidy(iris_lms,fit,conf.int = TRUE)

# runs the predict function across the dataset wth residuals + other things #
irisPred <- augment(iris_lms,fit)

# gives r squared, degrees of freedom and some other stuff # 
irisSumm <- glance(iris_lms,fit)

# graph estimate #
ggplot(iris_coefs, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) 




######### Do a regression by group (Species) for grouped regression models ######### 

# adds the lm function as a column to your tibble as fitSpecies #
iris_lms_Species <- 
  iris %>% group_by(Species) %>% 
  do(fitSpecies = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = .))

# gives a tibble of term, estimate,std.error,statistic, & p value of variables in model #
iris_coefs_Species <- tidy(iris_lms_Species,fitSpecies,conf.int = TRUE)

# runs the predict function across the dataset wth residuals + other things #
irisPred_Species <- augment(iris_lms_Species,fitSpecies)

# gives r squared, degrees of freedom and some other stuff # 
irisSumm_Species <- glance(iris_lms_Species,fitSpecies)

# graph by facet of species and look at CIs #
ggplot(iris_coefs_Species , aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + 
  facet_grid(cols = vars(Species))




###### PCA with broom #######

USArrests %>% head() %>% knitr::kable()

us_arrests <- USArrests %>% 
  rownames_to_column(var = "state") %>% 
  rename_all(tolower) %>% 
  as_tibble()

us_arrests %>% 
  mutate(state = factor(state), 
         state = fct_reorder(state, murder) %>% fct_rev()) %>% 
  ggplot(aes(state, murder)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  labs(y = "Murder Arrest Rate per 100,000 people",
       x = "State",
       title = "Murder rate in each state of the USA")

plot <- us_arrests %>% 
  gather(key = crime, value = rate, c(murder, assault, rape)) %>% 
  ggplot(aes(urbanpop, rate, color = crime)) + 
  facet_wrap(~crime, scales = "free_y", ncol = 1) +
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() + 
  labs(x = "Percentage Urban Population",
       y = "Arrest Rate per 100,000 people",
       title = "Arrest rate vs percentage urban population") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggplotly(plot)

us_arrests_pca <- us_arrests %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-state), 
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

us_arrests_pca


var_exp <- us_arrests_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

var_exp

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

us_arrests_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = TRUE,
                 label.label = "state",
                 label.repel = TRUE) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on USArrests dataset")
    )
  ) %>%
  pull(pca_graph)

##### Factor Analysis ######

fit <- factanal(us_arrests %>% select(-state),factors = 1,rotation = "varimax")

us_factor <- 
  us_arrests %>% select(-state) %>% 
  do(fit = factanal(.,factors = 1,rotation = "varimax"))

us_factor_tibble <- tidy(us_lms,fit)

us_Pred <- glance(us_factor,fit)

### Basketball Data ###

library(SportsAnalytics)
library(dplyr)
library(forcats)

nba_data <- as_tibble(fetch_NBAPlayerStatistics()) %>% dplyr::select(-League) %>% arrange(-TotalMinutesPlayed)
glimpse(nba_data)
nba_positions <- nba_data %>% dplyr::select(-Name,-Team) %>% group_by(Position)
nba_positions_100 <- nba_positions[1:(nrow(nba_data)/2),]
nba_positions_100 <- nba_positions_100 %>% dplyr::mutate(PointsPerGame = TotalPoints/GamesPlayed, AssistsPerGame = Assists/GamesPlayed)
esquisser()
