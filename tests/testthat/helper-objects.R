library("randomForest")
library("xgboost")
library("titanic")
library("DALEX")

HR_glm_model <- glm(status == "fired" ~ ., data = HR, family = "binomial")
explainer_glm <- explain(HR_glm_model, data = HR,  y = HR$status == "fired")

HR_rf_model <- randomForest(status ~ ., data = HR, ntree = 100)
explainer_HR_rf  <- explain(HR_rf_model, data = HR, y = HR$status)

loss_cross_entropy <- function (observed, predicted, p_min = 0.0001) {
  p <- sapply(seq_along(observed), function(i) predicted[i, observed[i]])
  sum(-log(pmax(p, p_min)))
}


# Random Forest
# Example of a model built using a data frame
titanic_small <- titanic_train[,c("Survived", "Pclass", "Sex", "Age",
                                  "SibSp", "Parch", "Fare", "Embarked")]
titanic_small$Survived <- factor(titanic_small$Survived)
titanic_small$Sex <- factor(titanic_small$Sex)
titanic_small$Embarked <- factor(titanic_small$Embarked)
titanic_small <- na.omit(titanic_small)
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                         data = titanic_small)
explainer_rf <- explain(rf_model, data = titanic_small,
                        y = titanic_small$Survived == "1", label = "RF")

# xgboost (using matrix object)
# Example of a model that relies on a numeric matrix
titanic_small_mat <- titanic_small
titanic_small_survived <- 0 + (titanic_small$Survived==1)
titanic_small_mat$Survived <- NULL # remove outcome from dataset
titanic_small_mat$Embarked <- NULL # skip embarked because it is messy to convert into numeric)
titanic_small_mat$Sex <- 0 + (titanic_small_mat$Sex=="male")
titanic_small_mat <- as.matrix(titanic_small_mat)
xgb_model <- xgboost(data=titanic_small_mat, label=titanic_small_survived,
                     nrounds=2, verbose=FALSE)
explainer_xgb <- explain(xgb_model,
                         data=titanic_small_mat,
                         y=titanic_small_survived, label="xgboost")

# helper objects for aspect_importance tests
titanic_data <- na.omit(titanic)
titanic_glm_model <- glm(survived == "yes" ~ class+gender+age+sibsp+parch+fare+embarked,
                         titanic_data, family = "binomial")

titanic_new_observation <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew",
                                   "engineering crew", "restaurant staff",
                                   "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8,
  sibsp = 0,
  parch = 0,
  fare = 72,
  embarked = factor("Southampton", levels = c("Belfast","Cherbourg",
                                              "Queenstown","Southampton"))
)

titanic_aspects <- list(wealth = c("class", "fare"),
                        family = c("gender", "sibsp", "parch"),
                        age = "age",
                        embarked = "embarked")


apartments_lm_model <- lm(m2.price ~ ., data = apartments)

apartments_aspects <- list(space = c("surface", "no.rooms"),
                           construction.year = "construction.year",
                           floor = "floor",
                           district = "district")

apartments_new_observation <- apartments_test[2,-1]

apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]

apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)

apartments_num_new_observation <- apartments_num[2,-1]

