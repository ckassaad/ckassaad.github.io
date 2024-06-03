#' ---
#' title: Causal estimation - TP 
#' author: David Hajage \texttt{david.hajage@aphp.fr}
#' institute: Département de Santé Publique
#' date: Juin 2024

#' 
#' 
#' # Presentation of the data
#' 
#' ### Context
#' 
#' - Patients with liver disease hospitalized for encephalopathy
#' - Treatment of interest: fecal transplantation
#' - Cohort study that prospectively included `r nrow(df)` patients
#' - Primary objective: to evaluate the effectiveness of fecal transplantation for the treatment of hepatic encephalopathy
#' - Primary endpoint: mortality
#' 
#' ### Packages needed
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## install.packages(c("gtsummary", "Hmisc", "ggplot2",
##                    "survey", "cobalt", "marginaleffects"))

#' 
#' ### Data import
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
load("df.RData")
head(df)

#' 
#' ### Definitions 
#' 
#' | Variable  | Label             | Unit/Coding                           |
#' |-----------|-------------------|---------------------------------------|
#' | `id`      | Identification Number |                                   |
#' | `age`     | Age               | Year                                  |
#' | `sex`    | Sex               | `0`: Male, `1`: Female                |
#' | `tobacco`   | Tobacco           | `0`: Smoker, `1`: Non-smoker          |
#' | `alcohol`  | Alcohol           | `0`: Never, `1`: Moderate, `2`: Excessive |
#' | `bili`    | Bilirubin         | $\mu mol/l^{-1}$                      |
#' | `hiv`     | HIV               | `0`: No, `1`: Yes                     |
#' | `status`  | Vital Status      | `0`: Alive, `1`: Deceased             |
#' 
#' Categorical variables have already been recoded as `factor` (`.f` at the end of the variable name).
#' 
#' ### Description of the data
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(df[, c("age", "sex.f", "tobacco.f", "alcohol.f", 
               "bili", "hiv.f", "trt.f", "status.f")])

#' 
#' ### Description of the data
#' 
#' 

#' 
#' ---------
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# install.packages("gtsummary")
library(gtsummary)
tbl_summary(df[, c("age", "sex.f", "tobacco.f", "alcohol.f", 
                   "bili", "hiv.f", "trt.f", "status.f")], 
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
#' # Naive analysis of the mortality
#' 
#' ### Effect of the treatment on mortality
#' 
#' - Compare mortality between the 2 treatment groups (using description followed by a simple logistic regression)
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("trt.f", "status.f")], 
            by = trt.f,
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(glm(status ~ trt.f, data = df, family = "binomial"))

#' 
#' 
#' ### Effect of the treatment on mortality
#' 
#' - Explore the effect of other variables on mortality
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("age", "status.f")], 
            by = "status.f",
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(glm(status ~ age, data = df, family = "binomial"))

#' 
#' The interpretation of this log odds ratio is: for each additional year, the log odds ratio increases by `r round(glm(status ~ age, data = df, family = "binomial")$coef[2], 3)`.

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("bili", "status.f")], 
            by = "status.f",
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(glm(status ~ bili, data = df, family = "binomial"))

#' 
#' The interpretation of this log odds ratio is: for each additional unit of bilirubin, the risk of death increases by `r round(glm(status ~ bili, data = df, family = "binomial")$coef[2], 3)`.

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("tobacco.f", "status.f")], 
            by = "status.f",
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(glm(status ~ tobacco.f, data = df, family = "binomial"))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("alcohol.f", "status.f")], 
            by = "status.f",
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(glm(status ~ alcohol.f, data = df, family = "binomial"))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("hiv.f", "status.f")], 
            by = "status.f",
            statistic = all_continuous() ~ "{mean} ({sd})"
)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(glm(status ~ hiv.f, data = df, family = "binomial"))

#' 
#' # Comparison of treated and untreated
#' 
#' ### Quick analysis
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_summary(df[, c("age", "sex.f", "tobacco.f", "alcohol.f", 
                   "bili", "hiv.f", "trt.f")], 
            statistic = all_continuous() ~ "{mean} ({sd})", 
            by = "trt.f"
) %>% add_p()

#' 
#' - Several variables are significantly unbalanced between treated and untreated subjects 
#' - The `hiv` variable is extreme in this regard: there are no HIV-positive subjects treated with fecal transplantation
#' - What would you do? 
#' - Being HIV-positive seems to be a contraindication for transplantation
#'   $\rightarrow$ positivity problem
#'   $\rightarrow$ exclusion of HIV-positive patients
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
df <- df[df$hiv == 0, ]

#' 
#' ### Standardized differences
#' 
#' - Example for age (quantitative variable) 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
m0 <- with(df[df$trt == 0, ], mean(age)) # moy chez les non exposés
m1 <- with(df[df$trt == 1, ], mean(age)) # moy chez les exposés
v0 <- with(df[df$trt == 0, ], var(age)) # var chez les non exposés
v1 <- with(df[df$trt == 1, ], var(age)) # var chez les exposés
(m1 - m0)/sqrt((v1 + v0)/2)

#' 
#' ### Standardized differences
#' 
#' - Example for sex (categorical variable) 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
m0 <- with(df[df$trt == 0, ], mean(sex)) # moy chez les non exposés
m1 <- with(df[df$trt == 1, ], mean(sex)) # moy chez les exposés
v0 <- with(df[df$trt == 0, ], var(sex)) # var chez les non exposés
v1 <- with(df[df$trt == 1, ], var(sex)) # var chez les exposés
(m1 - m0)/sqrt((v1 + v0)/2)

#' 
#' ### Standardized differences
#' 
#' - Load the `cobalt` library for an easier calculation :
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
library(cobalt)
diffs <- bal.tab(trt ~ age + sex.f + tobacco.f + alcohol.f + bili, data = df, 
                 binary = "std")
diffs

#' 
#' ### Standardized differences
#' 
#' - Some variables have severe imbalance (`bili` for example), others have moderate imbalance (`alcohol.f`) 
#' - In fact, the imbalance between treated and untreated groups can be inspected across the entire distribution
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)
ggplot(data = df, aes(x = age, fill = trt.f)) + geom_density(alpha = 0.5) + ggtitle("age") + theme(legend.position="bottom")
ggplot(data = df, aes(x = bili, fill = trt.f)) + geom_density(alpha = 0.5) + ggtitle("bili") + theme(legend.position="bottom")
ggplot(data = df, aes(x = log(bili), fill = trt.f)) + geom_density(alpha = 0.5) + ggtitle("log(bili)") + theme(legend.position="bottom")

#' 
#' (We observe that bilirubin has a log-normal distribution)
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
df$logbili <- log(df$bili)

#' 
#' 
#' ### Standardized differences
#' 
#' - It's common practice to display standardized differences on a plot
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
love.plot(diffs, thresholds = c(m = 0.1))

#' 
#' ### Conclusion
#' 
#' - Some variables are imbalanced (`bili`, and `alcohol` in particular)
#' - All variables appear to be prognostic of mortality
#' - Which variables will you consider?
#' 
#' # Standardization
#' 
#' ### Estimating a prognostic model by treatment group
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## model among trt = 1
mod1 <- glm(status ~ age + sex.f + tobacco.f + alcohol.f + logbili,
            data = subset(df, trt == 1), family = binomial)
## model among trt = 0
mod0 <- glm(status ~ age + sex.f + tobacco.f + alcohol.f + logbili,
            data = subset(df, trt == 0), family = binomial)

#' 
#' ### ATE estimation
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## status predicted if trt = 1
y1ate <- predict(mod1, newdata = df, type = "response")
## status predicted if trt = 0
y0ate <- predict(mod0, newdata = df, type = "response")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
mean(y1ate) - mean(y0ate) ## Risk difference ATE
log(mean(y1ate) / mean(y0ate)) ## log(RR) ATE
log((mean(y1ate)/(1-mean(y1ate))) / (mean(y0ate)/(1-mean(y0ate)))) ## log(OR) ATE

#' 
#' ### ATT estimation
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## status predicted if trt = 1
y1att <- predict(mod1, newdata = subset(df, trt == 1), type = "response")
## status predicted if trt = 0
y0att <- predict(mod0, newdata = subset(df, trt == 1), type = "response")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
mean(y1att) - mean(y0att) ## Risk difference ATT
log(mean(y1att) / mean(y0att)) ## log(RR) ATT
log((mean(y1att)/(1-mean(y1att))) / (mean(y0att)/(1-mean(y0att)))) ## log(OR) ATT

#' 
#' ### With the `marginaleffects` package
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
library(marginaleffects)
mod <- glm(status ~ trt * (age + sex.f + tobacco.f + alcohol.f + logbili),
           data = df, family = binomial) ## model with interactions

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## Risk difference ATE
avg_comparisons(mod, variables = "trt")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log(RR) ATE
avg_comparisons(mod, variables = "trt", comparison = "lnratioavg")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log(OR) ATE
avg_comparisons(mod, variables = "trt", comparison = "lnoravg")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## Risk difference ATT
avg_comparisons(mod, variables = "trt", 
                newdata = subset(df, trt == 1))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log(RR) ATT
avg_comparisons(mod, variables = "trt", comparison = "lnratioavg", 
                newdata = subset(df, trt == 1))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log(OR) ATT
avg_comparisons(mod, variables = "trt", comparison = "lnoravg", 
                newdata = subset(df, trt == 1))

#' 
#' # Propensity score weighting
#' 
#' ### Estimation of the propensity model
#' 
#' - The exposure is binary, so we can estimate the propensity score using logistic regression
#' - Since all variables appear to be prognostic, we include all variables in the model
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
mod.trt <- glm(trt ~ age + sex.f + tobacco.f + alcohol.f + logbili, 
               data = df, family = "binomial")
summary(mod.trt)

#' 
#' ### Estimation of the propensity score
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
df$score <- predict(mod.trt, type = "response")

#' 
#' ### Comparison of the propensity score distribution between Treated and Untreated
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = df, aes(x = score, color = trt.f, fill = trt.f)) +
  geom_density(alpha = 0.5) +
  xlab("Propensity score") + 
  theme(legend.position="bottom")
ggplot(data = df, aes(x = score, color = trt.f, fill = trt.f)) +
  geom_histogram(alpha = 0.5) +
  xlab("Propensity score") + 
  theme(legend.position="bottom") 

#' 
#' ### Comparison of the propensity score distribution between Treated and Untreated
#' 
#' - The distributions of propensity scores for treated and untreated subjects are not perfectly overlapping
#' - However, their overlap appears adequate, which is reassuring regarding the positivity assumption
#' 
#' ### Weights estimation
#' 
#' - We will create two types of weights: an "ATE" weight and an "ATT" weight
#' 
#' - "ATE" weight
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
df$wate <- ifelse(df$trt == 1, 1/df$score, 1/(1 - df$score))

#' 
#' 
#' - "ATT" weight : no weighting for the treated, untreated weighted by $score/(1-score)$
#' 
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
df$watt <- ifelse(df$trt == 1, 1, df$score/(1 - df$score))

#' 
#' 
#' - Examine the balance after applying the different weights (you should use the `std.diff` function with the option `weights = "wate"` or `"wate"`)
#' 
#' ### Balance inspection
#' 

## -------------------------------------------------------------------------------------------------------------------------------------------------------------
diffs <- bal.tab(trt ~ age + sex.f + tobacco.f + alcohol.f + bili, 
                 data = df, 
                 weights = "wate", binary = "std")
love.plot(diffs, thresholds = c(m = 0.1))

## -------------------------------------------------------------------------------------------------------------------------------------------------------------
diffs <- bal.tab(trt ~ age + sex.f + tobacco.f + alcohol.f + bili,
                 data = df, weights = "watt", 
                 binary = "std")
love.plot(diffs, thresholds = c(m = 0.1))

#' 
#' ### Treatment effect
#' 
#' - ATE
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
m1 <- with(subset(df, trt == 1), weighted.mean(status, wate))
m0 <- with(subset(df, trt == 0), weighted.mean(status, wate))
m1 - m0 ## Risk difference ATE
log(m1/m0) ## log RR ATE
log(((m1)/(1-m1))/((m0)/(1-m0))) ## log RR ATE

#' 
#' - ATT
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
m1 <- with(subset(df, trt == 1), weighted.mean(status, watt))
m0 <- with(subset(df, trt == 0), weighted.mean(status, watt))
m1 - m0 ## Risk difference ATE
log(m1/m0) ## log RR ATE
log(((m1)/(1-m1))/((m0)/(1-m0))) ## log RR ATE

#' 
#' 
#' ### With the `survey` package
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# install.packages("survey")
library(survey)
## Risk difference ATE
summary(svyglm(status ~ trt.f, family = binomial(link = "identity"),
               svydesign(data = df, ids = ~ 1, weight = ~ wate)))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log RR ATE
summary(svyglm(status ~ trt.f, family = binomial(link = "log"),
               svydesign(data = df, ids = ~ 1, weight = ~ wate)))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log OR ATE
summary(svyglm(status ~ trt.f, family = binomial(link = "logit"),
               svydesign(data = df, ids = ~ 1, weight = ~ wate)))

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## Risk difference ATT
summary(svyglm(status ~ trt.f, family = binomial(link = "identity"),
               svydesign(data = df, ids = ~ 1, weight = ~ watt)))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log RR ATT
summary(svyglm(status ~ trt.f, family = binomial(link = "log"),
               svydesign(data = df, ids = ~ 1, weight = ~ watt)))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
## log OR ATE
summary(svyglm(status ~ trt.f, family = binomial(link = "logit"),
               svydesign(data = df, ids = ~ 1, weight = ~ watt)))

