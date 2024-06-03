## Simulation d'une base de données utilisée pour le TP

set.seed(123)

expit <- function(x) exp(x)/(1+exp(x))
logit <- function(x) log(x/(1-x))

## Paramètres
n <- 10^6 # nb de sujets de la population
nn <- 2000 # nb de l'échantillon
alpha0 <- -2 # intercept du modèle de propension qui controle la prévalence du traitement
alpha.age <- log(1)
alpha.sexe <- log(1)
alpha.tabac <- log(1.25)
alpha.alcool.1 <- log(1/1.3)
alpha.alcool.2 <- log(1/1.6)
alpha.bili <- log(1.3)

beta0 <- -7
beta.age <- log(1.1)
beta.sexe <- log(0.7)
beta.tabac <- log(1.7)
beta.alcool.1 <- log(1.5)
beta.alcool.2 <- log(2)
beta.bili <- log(1.008)
beta.vih <- log(3)

beta.trt <- log(1/1.3)

## 1) simulation des covariables

age = round(rnorm(n, mean = 60, sd = 7), 2)                                       # facteur pronostique
sexe <- sample(0:1, n, replace = TRUE, prob = c(0.45, 0.55))                      # facteur pronostique
sexe.f <- factor(sexe, 0:1, c("Male", "Female"))
tabac <- sample(0:1, n, replace = TRUE, prob = c(0.4, 0.60))                      # facteur de confusion
tabac.f <- factor(tabac, 0:1, c("Non-smoker", "Smoker")) 
alcool <- sample(0:2, n, replace = TRUE, prob = c(0.30, 0.5, 0.2))                # facteur de confusion
alcool.f <- factor(alcool, 0:2, c("Never", "Moderate", "Excessive"))
bili <- round(rlnorm(n, 3.4, 1.5), 3)                                             # facteur de confusion

dfn <- data.frame(age = age, 
                  sexe = sexe,
                  sexe.f = sexe.f, 
                  tabac = tabac, 
                  tabac.f = tabac.f, 
                  alcool = alcool, 
                  alcool.f = alcool.f, 
                  bili = bili)

## 2) simulation de l'exposition au traitement
pT <- expit(
  alpha0 +
    alpha.age*dfn$age +
    alpha.sexe*dfn$sexe +
    alpha.tabac*dfn$tabac +
    alpha.alcool.1*(dfn$alcool == 1) +
    alpha.alcool.2*(dfn$alcool == 2) +
    alpha.bili*log(dfn$bili))

dfn$trt <- rbinom(n, 1, pT)
dfn$trt.f <- factor(dfn$trt, 0:1, c("Unexposed", "Exposed"))
# table(dfn$trt)

## La variable VIH est ajoutée à part, juste pour illustrer la notion de positivité dans le TP
dfn$vih <- ifelse(dfn$trt == 1, 0, NA)
dfn$vih[is.na(dfn$vih)] <- sample(0:1, sum(is.na(dfn$vih)), TRUE, c(0.95, 0.05))
dfn$vih.f <- factor(dfn$vih, 0:1, c("No", "Yes"))

## 3) simulation de l'évènement
lp <- beta0 +
  beta.age*dfn$age +
  beta.sexe*dfn$sexe +
  beta.tabac*dfn$tabac +
  beta.alcool.1*(dfn$alcool == 1) +
  beta.alcool.2*(dfn$alcool == 2) +
  beta.bili*dfn$bili +
  beta.vih*dfn$vih +
  beta.trt*dfn$trt

U <- runif(n)

dfn$status <- ifelse(expit(lp) < U, 0, 1)

lp1 <- beta0 +
  beta.age*dfn$age +
  beta.sexe*dfn$sexe +
  beta.tabac*dfn$tabac +
  beta.alcool.1*(dfn$alcool == 1) +
  beta.alcool.2*(dfn$alcool == 2) +
  beta.bili*dfn$bili +
  beta.vih*dfn$vih +
  beta.trt*1

lp0 <- beta0 +
  beta.age*dfn$age +
  beta.sexe*dfn$sexe +
  beta.tabac*dfn$tabac +
  beta.alcool.1*(dfn$alcool == 1) +
  beta.alcool.2*(dfn$alcool == 2) +
  beta.bili*dfn$bili +
  beta.vih*dfn$vih +
  beta.trt*0

dfn$status1 <- ifelse(expit(lp1) < U, 0, 1)
dfn$status0 <- ifelse(expit(lp0) < U, 0, 1)

## 4) chtite vérification
mean(dfn$trt)
mean(dfn$status)

mean(dfn$status1) - mean(dfn$status0)
mean(dfn$status1)/mean(dfn$status0)
(mean(dfn$status1)/(1-mean(dfn$status1)))/(mean(dfn$status0)/(1-mean(dfn$status0)))

# mod.trt <- glm(trt ~ age + sexe.f + tabac.f + alcool.f + log(bili), data = dfn, family = "binomial")
# summary(glm(status ~ age + sexe.f + tabac.f + alcool.f + bili + trt.f, data = dfn, family = "binomial"))
# summary(glm(status ~ trt.f, data = dfn, family = "binomial"))

## 
dfn$status.f <- factor(dfn$status, 0:1, c("Alive", "Deceased"))
df <- dfn[sample(n, nn), ]
df$id <- 1:nn
df <- df[, c("id", "age", "sexe", "sexe.f", "tabac", "tabac.f", "alcool", "alcool.f", "bili", "vih", "vih.f", "trt", "trt.f", "status", "status.f")]
## En fait diapo et données en anglais :
names(df) <- c("id", "age", "sex", "sex.f", "tobacco", "tobacco.f", "alcohol", "alcohol.f", "bili", "hiv", "hiv.f", "trt", "trt.f", "status", "status.f")

summary(glm(trt ~ age + sex.f + tobacco.f + alcohol.f + log(bili), data = df, family = "binomial"))
summary(glm(status ~ age + sex.f + tobacco.f + alcohol.f + bili + trt.f, data = df, family = "binomial"))
summary(glm(status ~ trt.f, data = df, family = "binomial"))

save(list = "df", file = "df.RData")
save(list = "dfn", file = "dfn.RData")

