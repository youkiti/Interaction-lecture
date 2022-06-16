
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
             "modmarg",
             "broom",
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

data %>% glimpse()

# Logistic regression -----------------------------------------------------

fit <- glm(death ~ treatment + subgroup + age, 
           family = binomial(link = "logit"),
           data = data)

tidy(fit, exponentiate = FALSE, conf.int = TRUE)
tidy(fit, exponentiate = TRUE, conf.int = TRUE)

fit_sub <- glm(death ~ treatment*subgroup + age,
               family = binomial(link = "logit"),
               data = data)
tidy(fit, exponentiate = FALSE, conf.int = TRUE)
tidy(fit_sub, exponentiate = TRUE, conf.int = TRUE)

fit_sub2 <- glm(death ~ treatment*age + subgroup,
                family = binomial(link = "logit"),
                data = data)
tidy(fit, exponentiate = FALSE, conf.int = TRUE)
tidy(fit_sub2, exponentiate = TRUE, conf.int = TRUE)


# Marginal effect ---------------------------------------------------------

updated_data <- data.frame(subgroup = rep(mean(data$subgroup), 2),
                           age = rep(mean(data$age), 2),
                           treatment = c(0,1))

updated_data %>% glimpse()

updated_data <- cbind(updated_data, predict(fit,
                                            newdata = updated_data,
                                            type = "response",
                                            se.fit = TRUE))

updated_data <- updated_data %>% 
  rename(probability = "fit",
         se_probability = "se.fit") %>% 
  mutate(lower = probability - 1.96*se_probability,
         upper = probability + 1.96*se_probability,
         treatment = as.factor(treatment),
         subgroup = as.factor(subgroup))

updated_data %>% glimpse()

theme_set(theme_cowplot())
ggplot(updated_data, aes(x = as.factor(treatment), y = probability)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, lty=1, lwd=1, col="blue") +
  geom_point(shape=21, size=3, fill="black") + 
  labs(title= " Predicted probabilities", x="Treatment", y="Pr(y=1)") +
  theme(plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
        axis.title = element_text(family = "sans", size=9),
        plot.caption = element_text(family = "sans", size=5))

## Average marginal effect

ame <- margins(fit_sub, variables = "treatment")
summary(ame)

## Marginal effects at the mean

mem <- margins(fit_sub,
               at=list(subgroup=mean(as.numeric(data$treatment)),
                      age=mean(data$age)),
               variables= "treatment")

summary(mem)

## plots

marg <- as.data.frame(summary(margins(fit_sub, at=list(age=seq(60,80,2)), variables="treatment")))

marg <- marg %>% 
  mutate(AME = -AME,
         lower = -lower,
         upper = -upper)

ggplot(marg, aes(x=age, y=AME)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0, col="black", lty=2) + 
  scale_y_continuous(limits=c(0,.1), breaks=seq(0,.1,.01)) + 
  scale_x_continuous(limits=c(60,80), breaks=seq(60,80,2))

# Effect modification -----------------------------------------------------

marg <- marg(fit_sub,
             var_interest="treatment",
             at=list(treatment=c(0, 1), subgroup=c(0, 1)))
marg <- as.data.frame(matrix(unlist(marg),
                             nrow=4,
                             byrow=T))
marg %>% colnames()
colnames(marg) <- c("label", "margin", "se", "z", "pvalue", "lower", "upper")
marg$treatment <- c(0,1,0,1)
marg$subgroup <- c(0,0,1,1)

marg <- marg %>% 
  mutate(across(margin:upper, .fns = ~{as.numeric(.)}))

ggplot(marg, aes(x=as.factor(treatment), y=margin, colour=as.factor(subgroup))) + 
  geom_point() + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.05) 

ggplot(marg, aes(x=as.factor(treatment), y=margin, fill=as.factor(subgroup))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(0.9), width=.05)
