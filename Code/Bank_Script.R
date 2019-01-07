
# Librer??as ---------------------------------------------------------------
library(tidyverse)
library(outliers)
library(gridExtra)
library(caret)
library(readr)
library(pscl)
library(ROCR)

# Lectura de los datos ----------------------------------------------------

datos <- read_delim("Downloads/data/bank-additional-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# An??lisis exploratorio ---------------------------------------------------


# Distribuci??n de las variables y NA's ------------------------------------

summary(datos[c("age", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")])
sapply(lapply(datos[c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome", "y")], as.factor), summary)

datos <- datos %>% 
  filter(default != "unknown") %>% 
  filter(education != "unknown") %>% 
  filter(loan != "unknown") %>% 
  filter(housing != "unknown") %>% 
  select(-duration, -pdays)

# Outliers ----------------------------------------------------------------

a <- lapply(datos[c("age", "campaign", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")], rosnerTest, k = 10)

data.frame(do.call(rbind, lapply(a, function(v){v$n.outliers}))[,1],
                do.call(rbind, lapply(a, function(v){ min(v$all.stats$Value)}))[, 1]) %>%
  mutate(Variable = row.names(.)) %>% 
  rename(Outliers = do.call.rbind..lapply.a..function.v..., Corte = do.call.rbind..lapply.a..function.v....1) %>% 
  filter(Outliers > 0) %>% 
  select(Variable, Outliers, Corte)

datos  <- datos %>% 
  filter(age < 87) %>% 
  filter(campaign < 34) %>% 
  filter(previous < 4)

# Datos a utilizar --------------------------------------------------------

datos <- datos %>% 
  mutate(marital = if_else(marital == "unknown", "married", marital),
         job = if_else(job == "unknown", "unemployed", job))
datos[, c(2:10, 13, 19)] <- map(datos[, c(2:10, 13, 19)], as.factor)
datos <- datos %>% 
  mutate(housing = if_else(housing == "no", 0, 1),
         default = if_else(default == "no", 0, 1),
         loan = if_else(loan == "no", 0, 1),
         y = if_else(y == "no", 0, 1))
summary(datos)
plot_histogram(datos)


# An??lisis de correlaci??n -------------------------------------------------

a <- lapply(datos[c("age", "campaign", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")], cor.test, datos$y)

Coeficiente <- vector()
p_Value <- vector()
Variable <- vector()

for (i in seq_along(a)) {
  Coeficiente[i] <- round(a[[i]]$estimate, 2)
  p_Value[i] <- round(a[[i]]$p.value, 18)
  Variable[i] <- names(a)[i]
}

data.frame(Variable, Coeficiente, p_Value)

a <- map(lapply(datos[c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome")], table, datos$y), chisq.test)

p_Value <- vector()
Variable <- vector()

for (i in seq_along(a)) {
  p_Value[i] <- round(a[[i]]$p.value, 18)
  Variable[i] <- names(a)[i]
}
data.frame(Variable, p_Value)


# Normalizaci??n -----------------------------------------------------------

datos <- datos %>% 
  select(-loan, -default, -housing)
grid.arrange(
  datos %>% 
    ggplot() +
    geom_density(aes(age, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(campaign, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(previous, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(emp.var.rate, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(cons.price.idx, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(cons.conf.idx, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(euribor3m, fill = y), alpha = 0.4),
  datos %>% 
    ggplot() +
    geom_density(aes(nr.employed, fill = y), alpha = 0.4),
  ncol = 3
  )

grid.arrange(datos %>% 
               ggqqplot("age"),
             datos %>% 
               ggqqplot("campaign"),
             datos %>% 
               ggqqplot("previous"),
             datos %>% 
               ggqqplot("emp.var.rate"),
             datos %>% 
               ggqqplot("cons.price.idx"),
             datos %>% 
               ggqqplot("cons.conf.idx"),
             datos %>% 
               ggqqplot("euribor3m"),
             datos %>% 
               ggqqplot("nr.employed"),
             ncol = 3)

datos <- datos %>% 
  mutate(age = scale(age),
         campaign = scale(campaign),
         previous = scale(previous),
         emp.var.rate = scale(emp.var.rate),
         cons.price.idx = scale(cons.price.idx),
         cons.conf.idx = scale(cons.conf.idx),
         euribor3m = scale(euribor3m),
         nr.employed = scale(nr.employed))

# Modelado ----------------------------------------------------------------

datos %>% 
  ggplot() +
  geom_bar(aes(x = y))

set.seed(100)
trainDataIndex <- createDataPartition(datos$y, p=0.7, list = F)  # 70% training data
trainData <- datos[trainDataIndex, ]
testData <- datos[-trainDataIndex, ]

down_train <- downSample(x = trainData[, colnames(trainData) %in% "y"],
                         y = trainData$y)

mod_fit <- train(y ~ .,  data=trainData, method="glm", family="binomial")
summary(mod_fit)

datos <- datos %>% 
  select(-age, -job, -education, -previous, -marital)

trainDataIndex <- createDataPartition(datos$y, p=0.7, list = F)  # 70% training data
trainData <- datos[trainDataIndex, ]
testData <- datos[-trainDataIndex, ]

mod_fit_1 <- train(y ~ .,  data=trainData, method="glm", family="binomial")
summary(mod_fit_1)


# Diagn??stico -------------------------------------------------------------

lmtest::lrtest(mod_fit, mod_fit_1)
mod_1 <- glm(y~., data= trainData, family = "binomial")
mod_2 <- glm(y~., data= trainData, family = "binomial")
anova(mod_1, mod_2)
lmtest::lrtest(mod_1, mod_2)
pR2(mod_1)
varImp(mod_1)



# Validaci??n --------------------------------------------------------------

pred <-  predict(mod_fit, newdata=testData, type = "raw")
accuracy <- table(pred$pred, testData$y)
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data=as.factor(round(pred$pred, 0)), as.factor(testData$y))

# Conclusiones ------------------------------------------------------------

exp(coef(mod_fit$finalModel))

write.table(datos, "Downloads/bank-clean-data.csv", sep = ";", row.names = FALSE)
