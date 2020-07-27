## -------------------------------------------------------------------------------------------------------------------------------
library(plyr)
library(dplyr)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(caret)
library(caretEnsemble)
library(foreach)
library(doParallel)
library(chron)
library(hms)
library(tidyr)
library(tidyverse)
library(mice)
library(lubridate)
library(randomForest)
library(gbm)
library(effects)
library(pdp)
library(cvms)
library(broom)
library(modelplotr)
library(visreg)
library(margins)
library(data.table)


## -------------------------------------------------------------------------------------------------------------------------------
dataset <- read.csv("~/project/Own assignment/dataset_assignment_v02.csv", sep=";", na.strings = "na")
str(dataset)
class(dataset)
n_distinct(dataset)
summary(dataset)


## -------------------------------------------------------------------------------------------------------------------------------
anyNA(dataset)
sapply(dataset, {function(x)
  any(is.na(x))})


## -------------------------------------------------------------------------------------------------------------------------------
glimpse(dataset)


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
group_by(giving_product) %>%
tally(sort = TRUE) %>%
mutate(percent = n/sum(n) * 100) 


## -------------------------------------------------------------------------------------------------------------------------------
dataset$nps <- as.character(dataset$nps)
dataset$nps <- as.numeric(dataset$nps)
hist(dataset$nps, main= "NPS distribution", xlab = "NPS score", ylab = "Count")


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, nps) %>%
  filter(nps >= 0) %>%
  group_by(giving_product) %>%
  count(nps) %>%
  ggplot(aes(x=giving_product, y=nps)) +
  geom_point(aes(size=n)) +
  scale_y_continuous(breaks = seq(0,10, 1), 
                     limit=c(0,10))


## -------------------------------------------------------------------------------------------------------------------------------
nps <- dataset$nps
nps <- na.omit(dataset$nps)
mean(nps, na.rm = TRUE)
str(nps)
length(nps)


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, nps) %>%
  filter(nps >= 0) %>%
  group_by(giving_product) %>%
  summarize(mean_nps = mean(nps))


## -------------------------------------------------------------------------------------------------------------------------------
promotors <-dataset %>%
  select(giving_product, nps) %>%
  filter(nps >=9) %>%
  group_by(giving_product) %>%
  tally() 
promotors


## -------------------------------------------------------------------------------------------------------------------------------
non_promotors <- dataset %>%
  select(giving_product, nps) %>%
  filter(nps >= 0) %>%
  group_by(giving_product) %>%
  tally() 
non_promotors


## -------------------------------------------------------------------------------------------------------------------------------
promotors_non_promotors <- merge(promotors, non_promotors, by = "giving_product", all = TRUE, suffix = c(".promotors", ".non_promotors")) %>% mutate(total_percent_promotors = n.promotors / n.non_promotors *100)
promotors_non_promotors


## -------------------------------------------------------------------------------------------------------------------------------
lp <- as.tbl(promotors_non_promotors)
lp <- list(promotors_non_promotors)
names(lp) <- c("promotors_non_promotors")
blp <- rbindlist(lp, id="id", use.names = TRUE)
ggplot() +
  geom_bar(data = blp, aes(x= giving_product, y=total_percent_promotors, fill = giving_product), stat = 'identity')



## -------------------------------------------------------------------------------------------------------------------------------
str(dataset$city)
head(dataset$city, n=50)
length(dataset$city)


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(city) %>%
  mutate(as.character(city)) %>%
  filter(city == "(not set)") %>%
  count(city)


## -------------------------------------------------------------------------------------------------------------------------------
dataset$city [dataset$city == "(not set)"] <- NA


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(city) %>%
  na.omit(city) %>%
  count(city, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  slice(1:10) %>%
  mutate(city = fct_reorder(city, n, .desc = TRUE)) %>%
  ggplot() +
  geom_bar(aes( x= city, y = n), stat = "identity")


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(city, giving_product) %>%
  group_by(giving_product) %>%
  filter(giving_product == "Yes") %>%
  na.omit(city) %>%
  count(city, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  slice(1:10) %>%
  mutate(city = fct_reorder(city, n, .desc = TRUE)) %>%
  ggplot() +
  geom_bar(aes( x= city, y = percentage), stat = "identity")


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(city, giving_product) %>%
  filter(giving_product == "No") %>%
  na.omit(city) %>%
  count(city, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  slice(1:10) %>%
  mutate(city = fct_reorder(city, n, .desc = TRUE)) %>%
  ggplot() +
  geom_bar(aes( x= city, y = percentage), stat = "identity")


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, pages_session) %>%
  group_by(giving_product) %>%
  summarise(mean_pages_session = mean(pages_session))


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, pages_session) %>%
  group_by(giving_product) %>%
  count(pages_session) %>%
  ggplot(aes(x = giving_product, y= pages_session))+
  geom_count()


## -------------------------------------------------------------------------------------------------------------------------------
dataset$avg_session_dur <- as.character(dataset$avg_session_dur)
dataset$avg_session_dur <- hms(dataset$avg_session_dur)
dataset$avg_session_dur <- as.numeric(dataset$avg_session_dur)
str(dataset)


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>% 
  select(giving_product, avg_session_dur) %>% 
  group_by(giving_product) %>% 
  summarize(average_min = mean(avg_session_dur / 60))


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>% 
  select(giving_product, avg_session_dur) %>% 
  group_by(giving_product) %>% 
  count(avg_session_dur) %>% 
  ggplot(aes(x=giving_product, y=avg_session_dur)) +
  geom_boxplot()


## -------------------------------------------------------------------------------------------------------------------------------
dataset$avg_time_page <- as.character(dataset$avg_time_page)
dataset$avg_time_page <- hms(dataset$avg_time_page)
dataset$avg_time_page <- as.numeric(dataset$avg_time_page)
str(dataset)


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, avg_time_page) %>% 
  group_by(giving_product) %>% 
  summarize(mean_time_page_seconds = mean(avg_time_page ))


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, avg_session_quality) %>%
  group_by(giving_product) %>%
  summarise(mean = mean(avg_session_quality))


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, avg_session_quality) %>%
  group_by(giving_product) %>%
  count(avg_session_quality) %>% 
  ggplot(aes(x=giving_product, y=avg_session_quality)) +
  geom_boxplot()


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(page_type, giving_product) %>%
  group_by(giving_product) %>%
  count(page_type, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100) %>%
ggplot(aes(x = giving_product, y= percentage, color = page_type))+
  geom_point()


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, source.used) %>%
  filter(giving_product == "Yes") %>%
  group_by(giving_product) %>%
  count(source.used, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)


## -------------------------------------------------------------------------------------------------------------------------------
dataset %>%
  select(giving_product, source.used) %>%
  filter(giving_product == "No") %>%
  group_by(giving_product) %>%
  count(source.used, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)


## -------------------------------------------------------------------------------------------------------------------------------
dataset_ml <- dataset
dataset_ml$registration_completed <- NULL
dataset_ml$city <- NULL
dataset_ml$userid <- NULL
str(dataset_ml)


## -------------------------------------------------------------------------------------------------------------------------------
unclass_sources <- unclass(dataset_ml$source.used)
dataset_ml$source.used <- as.numeric(unclass_sources)
str(dataset_ml)


## -------------------------------------------------------------------------------------------------------------------------------
unclass_pages <- unclass(dataset_ml$page_type)
dataset_ml$page_type <- as.numeric(unclass_pages)
str(dataset_ml)


## -------------------------------------------------------------------------------------------------------------------------------
dataset_na <- mice(dataset_ml, m=1, maxit = 5, method = "mean", seed = 500)
full_na <- complete(dataset_na, 1)
dim(full_na)
str(full_na)


## -------------------------------------------------------------------------------------------------------------------------------
(na_count_full_na <- data.frame(sapply(full_na, function(y)sum(length(which(is.na(y)))))))


## -------------------------------------------------------------------------------------------------------------------------------
options(digits = 2)
prop.table(table(dataset$giving_product))


## -------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
y <-full_na$giving_product
my_index <- createDataPartition(y, times = 1, p = 0.75, list = FALSE)
x_train <- full_na[my_index, ]
x_test <- full_na[-my_index, ]
str(x_train)
class(y)


## -------------------------------------------------------------------------------------------------------------------------------
registerDoParallel(3)
getDoParWorkers()
set.seed(123)


## -------------------------------------------------------------------------------------------------------------------------------
my_control <- trainControl(method = "repeatedcv", repeats = 5, number = 10, savePredictions = 'final', classProbs = TRUE, allowParallel = TRUE, sampling = "up", index = createResample(x_train$giving_product))


## -------------------------------------------------------------------------------------------------------------------------------
modeltypes <- list(multinom = caretModelSpec(method = "multinom"), rf = caretModelSpec(method = "rf"), svmRadial = caretModelSpec(method = "svmRadial"), adaboost = caretModelSpec(method = "adaboost"), xgbDart = caretModelSpec(method = "xgbDART"))

models <- caretList(giving_product ~ ., data = x_train, tuneList = modeltypes, metric = "Kappa", continue_on_fail = FALSE, preProcess = c("center", "scale"), trControl = my_control)


## -------------------------------------------------------------------------------------------------------------------------------
models


## -------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(predict(model_list$svmRadial, x_test, type = "raw", return_table = TRUE, dnn = c("Predicted", "Target")),  x_test$giving_product)


## -------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(predict(model_list$multinom, x_test, type = "raw"), x_test$giving_product)


## -------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(predict(model_list$rf, x_test, type = "raw"), x_test$giving_product)


## -------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(predict(model_list$adaboost, x_test, type = "raw"), x_test$giving_product)


## -------------------------------------------------------------------------------------------------------------------------------
confusionMatrix(predict(model_list$xgbDART, x_test, type = "raw"), x_test$giving_product)


## -------------------------------------------------------------------------------------------------------------------------------
model_overview <- list(multinim = model_list$multinom, rf = model_list$rf, svmradial = model_list$svmRadial, adaboost = model_list$adaboost, xgbDART = model_list$xgbDART)
res <- resamples(model_overview)
scales <- list(x =list(relation="free"), y=list(relation="free"))
bwplot(res, scales=scales)


## -------------------------------------------------------------------------------------------------------------------------------
library(MLeval)
evalm(list(model_list$multinom, model_list$rf, model_list$svmRadial, model_list$adaboost, model_list$xgbDART), gnames = c('multinom', 'rf', 'svmradial', 'adaboost', 'xgbdart'))


## -------------------------------------------------------------------------------------------------------------------------------
stackcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE)
stack_model <- caretStack(model_list, method="glm", metric="Kappa", trControl = stackcontrol)
print(stack_model)


## -------------------------------------------------------------------------------------------------------------------------------
evalm(list( stack_model$ens_model, model_list$xgbDART), gnames = c('stack', 'xgb'))


## -------------------------------------------------------------------------------------------------------------------------------
plot(varImp(object= model_list$xgbDART), main="xgbDart - variable importance")


## -------------------------------------------------------------------------------------------------------------------------------
model_x <- train(giving_product ~ ., data = x_train, method = "xgbDART", metric = "Kappa", trControl = my_control)


## -------------------------------------------------------------------------------------------------------------------------------
plot_modelx <- partial(model_list$xgbDART, pred.var = c("nps"), probs = T, which.class = 2, plot = TRUE, rug = TRUE)
plot_modelx


## -------------------------------------------------------------------------------------------------------------------------------
plot_modelx <- partial(model_list$xgbDART, pred.var = "avg_session_quality", probs = T, trim.outliers = TRUE, which.class = 2, plot = TRUE, rug = TRUE)
plot_modelx


## -------------------------------------------------------------------------------------------------------------------------------
plot_modelx <- partial(model_list$xgbDART, pred.var = "avg_time_page", probs = T, trim.outliers = TRUE, which.class = 2, plot = TRUE, rug = TRUE)
plot_modelx


## -------------------------------------------------------------------------------------------------------------------------------
library(knitr)
purl("predict_buying_behavior.rmd")

