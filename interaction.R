
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
             "plotly")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data preparation --------------------------------------------------------

set.seed(1116)
expit<-function(z){1/(1+exp(-(z)))}

n<-10000
data <- tibble(
  subgroup = rbinom(n, 1, .25),
  treatment = rbinom(n, 1, .5),
  age = round(70 + rnorm(n, 0, 5)),
  death = rbinom(n,1,.1+.1*treatment+.05*subgroup)
)

data <- data %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("Non-treatment", "Treatment")),
         subgroup = factor(subgroup, levels = c(0, 1), labels = c("Subgroup-", "Subgroup+")),
         death = factor(death, levels = c(0, 1), labels = c("Alive", "Death")))

data %>% glimpse()
str(data)
lapply(list(data$treatment, data$subgroup, data$death), function(x){levels(x)})

# Logistic regression -----------------------------------------------------

fit <- glm(death ~ treatment + subgroup + age, 
           family = binomial(link = "logit"),
           data = data)

cbind(Estimate=round(coef(fit),4),
      OR=round(exp(coef(fit)),4))

fit_sub <- glm(death ~ treatment*subgroup + age,
               family = binomial(link = "logit"),
               data = data)
cbind(Estimate=round(coef(fit_sub),4),
      OR=round(exp(coef(fit_sub)),4))


# Marginal effect ---------------------------------------------------------

updated_data <- data.frame(subgroup = as.factor(c("Subgroup-", "Subgroup+")),
                          age = rep(mean(data$age), 2),
                          treatment = as.factor(c("Non-treatment", "Treatment")))

updated_data %>% glimpse()
  
updated_data <- cbind(update_data, predict(fit,
                                          newdata = update_data,
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

ggplot(updated_data, aes(x = treatment, y = probability)) +
         geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, lty=1, lwd=1, col="red") +
         geom_point(shape=18, size=5, fill="black") + 
         scale_x_discrete(limits = c("Non-treatment","Treatment")) + 
         labs(title= " Predicted probabilities", x="Treatment", y="Pr(y=1)") +
          theme(plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
                axis.title = element_text(family = "sans", size=9),
                plot.caption = element_text(family = "sans", size=5))

## Average marginal effect

ame <- margins(fit_sub, variables = "treatment")
summary(ame)

## Marginal effects at the mean

mem <- margins(fit_sub,
               at=list(treatment=mean(as.integer(as.character(as.integer(data$treatment)))),
                       subgroup=mean(as.integer(as.character(as.integer(data$subgroup)))),
                       age=mean(data$age)),
               variables= c("treatment", "subgroup", "age"))

mem <- margins(fit_sub,
               at=list(age=mean(data$age)),
               variables= c("treatment"))

summary(mem)

## plots

marg <- as.data.frame(summary(margins(fit_sub, at=list(age=seq(60,80,2)), variables="treatment")))

ggplot(marg, aes(x=age, y=AME)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0, col="black", lty=2) + 
  scale_y_continuous(limits=c(0,.2), breaks=seq(0,.2,.05)) + 
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

ggplot(marg, aes(x=as.factor(treatment), y=margin, colour=as.factor(subgroup))) + 
  geom_point() + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.05)
