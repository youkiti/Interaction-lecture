
# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "data.table",
             "readxl",
             "tidyverse",
             "ggplot2",
             "effects",
             "margins",
             "RColorBrewer",
             "plotly",
             "cowplot")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#use_git_config(user.name = "AkiShiroshita", user.email = "akihirokun8@gmail.com")
#git_sitrep()
#create_github_token()
#gitcreds::gitcreds_set()

# Data preparation --------------------------------------------------------

set.seed(1116)
expit<-function(z){1/(1+exp(-(z)))}


n<-100000
data <- tibble(
  subgroup = rbinom(n, 1, .25),
  treatment = rbinom(n, 1, .5),
  age = round(70 + rnorm(n, 0, 5)),
  death = rbinom(n,1, expit(.01-.2*treatment+.08*subgroup+.01*age))
)

# Logistic regression -----------------------------------------------------

fit <- glm(death ~ treatment, 
           family = binomial(link = "logit"),
           data = data)

prob <- predict(fit, type = "response")
pred.class <- ifelse(prob > 0.7, "positive", "negative") # just for demo
pred.class %>% glimpse()
dat_n <- data %>% 
  select_if(is.numeric)
predictors <- colnames(dat_n)
dat_n <- dat_n %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
dat_n %>% glimpse()
ggplot(dat_n, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
