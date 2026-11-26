
install.packages("arules")
library(arules)

df <- data.frame(
  Items = c("a,d,e","a,b,c,e","a,b,d,e","a,c,d,e",
            "b,c,e","b,d,e","c,d","a,b,c","a,d,e","a,b,e")
)

trans_list <- strsplit(df$Items, ",")
transactions <- as(trans_list, "transactions")

inspect(transactions)

rules <- apriori(transactions,
                 parameter=list(supp=0.3,conf=0.5,minlen=2))

inspect(rules)

freq <- eclat(transactions,
              parameter=list(supp=0.3,minlen=1))

inspect(freq)

rules_fp <- ruleInduction(freq,confidence=0.5)
inspect(rules_fp)


install.packages("arules")
library(arules)

df <- data.frame(
  Items = c(
    "Milk,Beer,Diapers",
    "Bread,Butter,Milk",
    "Milk,Diapers,Cookies",
    "Bread,Butter,Cookies",
    "Beer,Cookies,Diapers",
    "Milk,Diapers,Bread,Butter",
    "Bread,Butter,Diapers",
    "Beer,Diapers",
    "Milk,Diapers,Bread,Butter",
    "Beer,Cookies"
  )
)

trans_list <- strsplit(df$Items,",")
transactions <- as(trans_list,"transactions")

inspect(transactions)

rules <- apriori(transactions,
                 parameter=list(supp=0.3,conf=0.5))

inspect(rules)

freq <- eclat(transactions,
              parameter=list(supp=0.3))

inspect(freq)

rules_fp <- ruleInduction(freq,confidence=0.5)
inspect(rules_fp)

install.packages("e1071")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")

library(e1071)
library(rpart)
library(rpart.plot)
library(caret)

df <- data.frame(
  age=c("<=30","<=30","31...40",">40",">40",">40",
        "31...40","<=30","<=30",">40","<=30","31...40",
        "31...40",">40"),
  
  income=c("high","high","high","medium","low","low",
           "low","medium","low","medium","medium","medium",
           "high","medium"),
  
  student=c("no","no","no","no","yes","yes","yes",
            "no","yes","yes","yes","no","yes","no"),
  
  credit_rating=c("fair","excellent","fair","fair","fair",
                  "excellent","excellent","fair","fair","fair",
                  "excellent","excellent","fair","excellent"),
  
  buys_computer=c("no","no","yes","yes","yes","no",
                  "yes","no","yes","yes","yes","yes","yes","no")
)

df[] <- lapply(df,factor)

set.seed(123)

train_index <- sample(1:nrow(df),0.7*nrow(df))

train_data <- df[train_index,]
test_data <- df[-train_index,]

nb_model <- naiveBayes(buys_computer~.,data=train_data)

nb_pred <- predict(nb_model,test_data)

nb_acc <- mean(nb_pred==test_data$buys_computer)

print(nb_acc)
confusionMatrix(nb_pred,test_data$buys_computer)

dt_model <- rpart(buys_computer~.,data=train_data,
                  method="class")

rpart.plot(dt_model)

dt_pred <- predict(dt_model,test_data,type="class")

dt_acc <- mean(dt_pred==test_data$buys_computer)

print(dt_acc)
confusionMatrix(dt_pred,test_data$buys_computer)

cat("Naive Bayes Accuracy:",nb_acc)
cat("Decision Tree Accuracy:",dt_acc)

install.packages("ggplot2")
install.packages("dplyr")
install.packages("caTools")

library(ggplot2)
library(dplyr)
library(caTools)

df <- read.csv("diabetes.csv")

str(df)
summary(df)

df$Glucose[df$Glucose==0] <- NA
df$BloodPressure[df$BloodPressure==0] <- NA
df$BMI[df$BMI==0] <- NA

df <- na.omit(df)

df <- df %>%
  mutate(AgeGroup=cut(Age,
                      breaks=c(20,30,40,50,60,80),
                      labels=c("21-30","31-40","41-50","51-60","61+"),
                      right=FALSE))

age_trends <- df %>%
  group_by(AgeGroup) %>%
  summarise(
    Count=n(),
    DiabetesRate=mean(Outcome)*100
  )

print(age_trends)

ggplot(age_trends,aes(x=AgeGroup,y=DiabetesRate))+
  geom_bar(stat="identity")+
  labs(title="Diabetes Trend by Age Group",
       y="Diabetes Rate")

simple_model <- lm(Glucose~Age,data=df)

summary(simple_model)

ggplot(df,aes(x=Age,y=Glucose))+
  geom_point()+
  geom_smooth(method="lm")

multiple_model <- lm(Outcome~Age+Glucose+
                       BloodPressure+BMI,data=df)

summary(multiple_model)

set.seed(123)

split <- sample.split(df$Outcome,SplitRatio=0.7)

train <- subset(df,split==TRUE)
test <- subset(df,split==FALSE)

model_test <- lm(Outcome~Age+Glucose+
                   BloodPressure+BMI,data=train)

pred <- predict(model_test,newdata=test)

results <- data.frame(
  Actual=test$Outcome,
  Predicted=pred
)

print(head(results))


@relation transactions

@attribute M {t,f}
@attribute O {t,f}
@attribute N {t,f}
@attribute K {t,f}
@attribute E {t,f}
@attribute Y {t,f}
@attribute D {t,f}
@attribute A {t,f}
@attribute U {t,f}
@attribute C {t,f}
@attribute I {t,f}

@data
t,t,t,t,t,t,f,f,f,f,f
f,t,t,t,t,t,t,f,f,f,f
t,f,f,t,t,f,f,t,f,f,f
t,f,f,t,f,t,f,f,t,t,f
f,t,f,t,t,f,f,f,f,t,t


algorithm <- c("JRip","J48")

accuracy <- c(90,95)

barplot(accuracy,
        names.arg=algorithm,
        col=c("blue","green"),
        main="Accuracy Comparison",
        xlab="Algorithms",
        ylab="Accuracy (%)",
        ylim=c(0,100))