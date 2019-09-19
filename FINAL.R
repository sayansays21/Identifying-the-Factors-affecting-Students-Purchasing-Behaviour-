install.packages("ResourceSelection")
install.packages("MASS")
install.packages("pROC")
install.packages("ggplot2")
install.packages("DescTools")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("broom")
library(MASS)
library(pROC)
library(ResourceSelection)
library(car)
library(DescTools)
library(dplyr)
library(tidyverse)
library(broom)

data=read.csv("C:\\Users\\HP\\Desktop\\SUKANITA\\DATA_R.csv",header=T,sep=",")
data=data[,-c(3,12)]

#plotting
library(ggplot2)
ggplot(data, aes(x = as.factor(PURCHASED), y = AGE, fill = as.factor(PURCHASED))) +   geom_boxplot(size = .75) +   facet_grid(FAMILY.INCOME ~ SEX, margins = FALSE) +   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

data_train=data[1:80,]
data_test=data[81:100,]
model_1<- glm(as.factor(PURCHASED)~SEX+AGE+FAMILY.INCOME+QUALITY.ETC+OCASSIONS+PRICE.AND.PROMOTIONAL+TRENDING.FASHION+PRESENCE+SHOPPING+ACCOMPANY, family = binomial,data = data_train)
summary(model_1)##model
hist(data_train$AGE) ###no outlier assumption
car::vif(model_1)#no multicollinearity Assumption

model_2=stepAIC(model_1)
summary(model_2)


###############linearity assumption
probabilities <- predict(model_2,data_train, type = "response")
logit=log(probabilities/(1-probabilities))
plot(data_train$AGE,logit)

###############influential values assumptuion by cooks distance
plot(model_2, which = 4, id.n = 3)
model.data <- augment(model_2) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = data_train$PURCHASED),alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)


hl=hoslem.test(data_train$PURCHASED,fitted(model_2),g=10)
for (i in 5:15) {
  print(hoslem.test(data_train$PURCHASED,fitted(model_2),g=i)$p.value)
}
cbind(hl$observed,hl$expected)
hl

# Predict the probability (p) of buyer being frequent

probabilities <- predict(model_2,data_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
my_table=table(data_test$PURCHASED,predicted.classes)
rownames(my_table) <- c("normal","frequent")
colnames(my_table) <- c("normal","frequent")
my_table

efficiency <- sum(diag(my_table))/sum(my_table)
efficiency

roc(data_test$PURCHASED~probabilities, data = data_test, plot = TRUE, main = "ROC CURVE", col= "blue")


odd_ratio= exp(cbind(coef(model_2),confint(model_2,level = 0.90)))
odd_ratio
Spec <- my_table[1,1]/(my_table[1,1] + my_table[1,2])
Sens=my_table[2,2]/(my_table[2,1] + my_table[2,2])
Prec=my_table[2,2]/(my_table[1,2] + my_table[2,2])






summary(model_2)

beta=as.vector(model_2$coefficients)

fun=function(x){
  z=beta[1]+(x$SEX=="MALE")*beta[2]+(x$AGE)*beta[3]+(x$QUALITY.ETC=="neutral")*beta[4]+(x$TRENDING.FASHION=="disagree")*beta[7]+(x$TRENDING.FASHION=="neutral")*beta[8]+(x$SHOPPING=="online")*beta[9]+(x$ACCOMPANY=="family")*beta[10]
  return(z)
}
z=fun(data_test)

sigmoid=1/(1+exp(-z))
plot(z,sigmoid,xlim=c(-10,10))



