library(corrplot)
library(car)

olympics <- read.csv("olympics.csv")



olymp = subset(olympics,select = c("Sex","Age","Height","Weight","Medal","Sport"))
olymp = olymp[olymp$Sport=='Basketball',]
olympsex = olymp['Sex']
table(olymp$Medal)
olymp = olymp[,-1]


library(dummies)
olymp = dummies::dummy.data.frame(olymp)

olymp$TotalMedals = olymp$MedalBronze +olymp$MedalGold +olymp$MedalSilver
olymp = olymp[,-4]
olymp = olymp[,-4]
olymp = olymp[,-4]
olymp = cbind(olymp,olympsex)

medals_won <- olymp[which(olymp$TotalMedals == 1), ]  # all 1's
medals_lost <- olymp[which(olymp$TotalMedals == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
medalswon_training_rows <- sample(1:nrow(medals_won), 0.7*nrow(medals_won))  # 1's for training
medalslost_training_rows <- sample(1:nrow(medals_lost), 0.7*nrow(medals_lost))  # 0's for training. Pick as many 0's as 1's
medalswon.train <- medals_won[medalswon_training_rows, ]  
medalslost.train <- medals_lost[medalslost_training_rows, ]
trainingData <- rbind(medalswon.train, medalslost.train)  # row bind the 1's and 0's 

# Create Test Data
medalswon.test <- medals_won[-medalswon_training_rows, ]
medalslost.test <- medals_lost[-medalslost_training_rows, ]
testData <- rbind(medalswon.test, medalslost.test)  # row bind the 1's and 0's 


#build logit models

logitMod2 <- glm(TotalMedals ~ as.factor(Sex) + Age +Height + Weight, data = trainingData, family =binomial(link = "logit"))
summary(logitMod2)

nrow(olymp[which(olymp$Sex == 'F'),])

nrow(olymp[which(olymp$Sex == 'M'),])


logistic <- glm(TotalMedals ~ Sex, data = trainingData, family = "binomial")
summary(logistic)

#Totalmedal = -0.80388 - 0.51334 * player is male
#Null deviance: 3485.5  on 3174  degrees of freedom
#Residual deviance: 3453.2  on 3173  degrees of freedom
#AIC: 3457.2
  
logistic <- glm(TotalMedals ~ Sex + Weight + Age + Height, data = trainingData, family = "binomial")
summary(logistic)
#this model is better due to lower AIC



#################################################################################

#Datamining

oly = olympics[which(!is.na(olympics$Medal)),]

oly = subset(oly,select = c("Sex","Age","Height","Weight","NOC","Medal","Sport"))
oly <- na.omit(oly)
olyretained <- oly
library(dummies)
oly <- dummies::dummy.data.frame(oly)


clusters <- kmeans(oly,centers = 3)
summary(clusters)


clusters$cluster


oly$Cluster <- clusters$cluster
olyretained$cluster <- clusters$cluster

oly = subset(olyretained,select = c("Sex","Age","Height","Weight","NOC","Medal","Sport","cluster"))

write.csv(oly, file = "OlympicClusters.csv")
