#PR2: Anàlisis del conjunt de dades Titanic

# Lectura fitxer
ruta <- "C:\\Users\\carla\\Desktop\\Ricard\\3_Tercer Quadrimestre\\Tipologia i cicle de vida de les dades\\Pràctica2\\Titanic\\csv\\"
passatgers <- read.csv(paste(ruta, "train.csv", sep = ""), header = TRUE, sep = ",", na.strings = "NA", encoding = "UTF-8")
passatgers_test <- read.csv(paste(ruta, "test.csv", sep = ""), header = TRUE, sep = ",", na.strings = "NA", encoding = "UTF-8")
passatgers

# Resum de les dades
# Número de passatgers
nrow(passatgers)
# Número de variables
ncol(passatgers)
# Nom de les variables
labels(passatgers)[2]
summary(passatgers)

#  Cabin
summary(passatgers$Cabin)

# Mostrem gràfica comparant Pclass i Fare
stripchart(passatgers$Pclass~passatgers$Fare,pch=20)

# Survived
summary(passatgers$Survived)
# Factoritzem Survived
passatgers$Survived <- as.factor(passatgers$Survived)
is.factor(passatgers$Survived)

# Fare
summary(passatgers$Fare)

# Sex
summary(passatgers$Sex)

# Age
summary(passatgers$Age)

# SibSp
summary(passatgers$SibSp)

# Parch
summary(passatgers$Parch)

# Mètode KNN per imputar valors perduts
library(VIM)
passatgers_2 <- kNN(passatgers)[,1:12]
passatgers_test <- kNN(passatgers_test)[,1:11]
summary(passatgers_2$Age)

passatgers[which(passatgers$Fare == 0),1:6]
passatgers[which(passatgers$Fare == 0),7:12]
nrow(passatgers[which(passatgers_2$Ticket == 'LINE'),])
nrow(passatgers[which(passatgers_2$Ticket == 112059),])
nrow(passatgers[which(passatgers_2$Cabin == 'B94'),])
nrow(passatgers[which(passatgers_2$Ticket == 239853),])
nrow(passatgers_2[which(passatgers_2$Ticket == 239854),])
nrow(passatgers_2[which(passatgers_2$Ticket == 112052),])
nrow(passatgers_2[which(passatgers_2$Ticket == 239856),])
nrow(passatgers_2[which(passatgers_2$Ticket == 239855),])
nrow(passatgers_2[which(passatgers_2$Ticket == 112050),])
nrow(passatgers_2[which(passatgers_2$Cabin == 'A36'),])
nrow(passatgers_2[which(passatgers_2$Ticket == 112058),])
nrow(passatgers_2[which(passatgers_2$Cabin == 'B102'),])
nrow(passatgers_2[which(passatgers_2$Ticket == 19972),])

mean(passatgers_2[which(passatgers_2$Pclass == 3),'Fare'])
passatgers_2[which(passatgers_2$Fare == 0 & passatgers_2$Pclass == 3),'Fare'] <- mean(passatgers_2[which(passatgers_2$Pclass == 3),'Fare'])
mean(passatgers_2[which(passatgers_2$Pclass == 2),'Fare'])
passatgers_2[which(passatgers_2$Fare == 0 & passatgers_2$Pclass == 2),'Fare'] <- mean(passatgers_2[which(passatgers_2$Pclass == 2),'Fare'])
mean(passatgers_2[which(passatgers_2$Pclass == 1),'Fare'])
passatgers_2[which(passatgers_2$Fare == 0 & passatgers_2$Pclass == 1),'Fare'] <- mean(passatgers_2[which(passatgers_2$Pclass == 1),'Fare'])

# Boxplots Age, SibSp i Parch
variables <- c("Age", "Fare")
for (name in variables){
  boxplot(passatgers_2[,name], main=name)
}
# Valors extrems
boxplot.stats(passatgers_2$Age)$out
boxplot.stats(passatgers_2$Fare)$out

head(passatgers[which(passatgers$Fare >= min(boxplot.stats(passatgers_2$Fare)$out)),1:6])
head(passatgers[which(passatgers$Fare >= min(boxplot.stats(passatgers_2$Fare)$out)),7:12])

passatgers[which(passatgers$Fare >= min(boxplot.stats(passatgers_2$Fare)$out) & passatgers$Pclass == 3),1:6]
passatgers[which(passatgers$Fare >= min(boxplot.stats(passatgers_2$Fare)$out) & passatgers$Pclass == 3),7:12]

passatgers[which(passatgers$Fare >= min(boxplot.stats(passatgers_2$Fare)$out) & passatgers$Pclass == 2),1:6]
passatgers[which(passatgers$Fare >= min(boxplot.stats(passatgers_2$Fare)$out) & passatgers$Pclass == 2),7:12]

head(passatgers)
head(passatgers_test)
passatgers_test$Survived <- NaN
passatgers_test <- subset (passatgers_test, select=c(1,12,2,3,4,5,6,7,8,9,10,11))
passatgers_all <- rbind(passatgers, passatgers_test)
nrow(passatgers_all)
library(dplyr)
grupos_sibsp <- group_by(passatgers_all, passatgers_all$SibSp)
resum_grupos_sibsp <- summarise(grupos_sibsp, num = n())
resum_grupos_sibsp

# Passatgers amb 8 germans i cònjugues
passatgers_all[which(passatgers_all$SibSp == 8),1:6]
passatgers_all[which(passatgers_all$SibSp == 8),7:12]


# Passatgers amb 5 germans i cònjugues
passatgers_all[which(passatgers_all$SibSp == 5),1:6]
passatgers_all[which(passatgers_all$SibSp == 5),7:12]


# Valors extrems Parch
grupos_parch <- group_by(passatgers_all, passatgers_all$Parch)
resum_grupos_parch <- summarise(grupos_parch, num = n())
resum_grupos_parch

# Passatgers amb 9 pares o fills
passatgers_all[which(passatgers_all$Parch == 9),1:6]
passatgers_all[which(passatgers_all$Parch == 9),7:12]

# Passatgers amb 6 pares o fills
passatgers_all[which(passatgers_all$Parch == 6),1:6]
passatgers_all[which(passatgers_all$Parch == 6),7:12]

#Assignar lletres als factors de Pclass i Sex
library(car)
passatgers_2$Sex <- recode(passatgers_2$Sex,"'male'='M';'female'='F'")
summary(passatgers_2$Sex)

# Reordenem la variable Sex en la nova variable SexR
passatgers_2$SexR <- relevel(passatgers_2$Sex, ref = "F")
head(passatgers_2$SexR)

library(nortest)
plot(density(passatgers_2$Age))
pearson.test(passatgers_2$Age)
plot(density(passatgers_2$Fare))
pearson.test(passatgers_2$Fare)

bartlett.test(passatgers_2$Age~passatgers_2$Survived, data=passatgers_2)
bartlett.test(passatgers_2$Fare~passatgers_2$Survived, data=passatgers_2)


cor(passatgers_2$Fare, passatgers_2$Pclass)

# Escriptura del fitxer netejat
passatgers_clean <- cbind(passatgers_2[,1:4],passatgers_2$SexR,passatgers_2[,6:12])
write.csv(passatgers_clean, file = paste(ruta, "Titanic_clean.csv", sep = ""), sep = ",")

# Generem el model
model_logit <- glm(passatgers_2$Survived ~ passatgers_2$Fare 
                   + passatgers_2$SexR + passatgers_2$Age + passatgers_2$SibSp
                   + passatgers_2$Parch, family = "binomial")
summary(model_logit)

#coefficients
coeff_beta <- coefficients(model_logit)
coeff_beta

# Calculem els valors predits
valors_predits <- predict(model_logit,passatgers_2,type = "response")
head(valors_predits)
# Interpretem els resultats amb el llindar indicat
clase_predita <- ifelse(valors_predits>0.7, 1,0)
head(clase_predita)
# Montem el data set a analitzar
data <- data.frame(obs = passatgers_2$Survived, pre = clase_predita)
kable(data.frame(Observació=head(data$obs),
                 Predicció=head(data$pre)), align = c("l","l"))

# Montem la matriu de confusió
matriu_confusio <- table(data$obs, data$pre, dnn = c("Observació", "Predicció"))
matriu_confusio

# Valors descriptius de la predicció
positius <- sum(data$obs == 1)
negatius <- sum(data$obs == 0)
positius_predit <- sum(data$pre == 1)
negatius_predit <- sum(data$pre == 0)
total <- nrow(data)
kable(data.frame(Mesura=c("Positius", "Negatius", "Positius predits", "Negatius Predits"),
                 Valor=c(positius, negatius, positius_predit, negatius_predit)), align = c("l","l","l","l"))

tp <- sum(data$obs == 1 & data$pre == 1)
tn <- sum(data$obs == 0 & data$pre == 0)
fp <- sum(data$obs == 0 & data$pre == 1)
fn <- sum(data$obs == 1 & data$pre == 0)
kable(data.frame(Mesura=c("Certs positius", "Certs negatius", "Falsos positius", "Falsos negatius"),
                 Valor=c(tp,tn,fp,fn)), align = c("l","l","l","l"))

exactitut <- (tp+tn)/total
ratio_error <- (fp+fn)/total
sensibilitat <- tp/positius
especificitat <- tn/negatius
precisio <- tp/positius_predit
valor_pre_neg <- tn/negatius_predit
kable(data.frame(Mesura=c("Exactitud", "Ratio d'error", "Sensibilitat", "Especificitat", "Precisió", "Valor de predicció de negatius"),
                 Valor=c(exactitut, ratio_error, sensibilitat, especificitat, precisio, valor_pre_neg)), align = c("l","l","l","l","l","l"))

stripchart(valors_predits~passatgers_2$Fare,pch=19)

passatgers_2$Ppre <- valors_predits
passatgers_2$Pre <- data$pre
passatgers_2[which(passatgers_2$Ppre >= 0.7),]
p_low_sur_pre <- passatgers_2[which(passatgers_2$Ppre >= 0.7 & passatgers_2$Fare <= quantile(passatgers_2$Fare)[2]),]
p_low_sur_pre[4:7]
head(p_low_sur_pre[4:7])

stripchart(valors_predits~passatgers_2$Age,pch=19)

p_lage_sur_pre <- passatgers_2[which(passatgers_2$Ppre <= 0.2 & passatgers_2$Age <= quantile(passatgers_2$Age)[2]),]
p_lage_sur_pre[3:7]
head(p_lage_sur_pre[3:7])

p_hage_sur_pre <- passatgers_2[which(passatgers_2$Ppre >= 0.7 & passatgers_2$Age >= quantile(passatgers_2$Age)[4]),]
p_hage_sur_pre[3:7]
head(p_hage_sur_pre[3:7])

stripchart(valors_predits~passatgers_2$SexR,pch=19)

library(pROC)
# Calculem la corba ROC
roc <- roc(passatgers$Survived,valors_predits) 
# Mostrem la corba calculada en un gràfic
plot(roc, print.thres="best", print.thres.best.method="closest.topleft")


clase_predita_2 <- ifelse(valors_predits>0.407, 1,0)
# Montem el data set a analitzar
data_2 <- data.frame(obs_2 = passatgers_2$Survived, pre_2 = clase_predita_2)
# Montem la matriu de confusió
matriu_confusio_2 <- table(data_2$obs_2, data_2$pre_2, dnn = c("Observació", "Predicció"))
matriu_confusio_2
