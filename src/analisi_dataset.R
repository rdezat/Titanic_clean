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

# Pclass
summary(passatgers$Pclass)
# Factoritzem Survived
passatgers$Pclass <- as.factor(passatgers$Pclass)
passatgers_test$Pclass <- as.factor(passatgers_test$Pclass)
is.factor(passatgers$Pclass)

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
passatgers_test <- kNN(passatgers_test)[,1:12]
summary(passatgers_2$Age)

# Boxplots Age, SibSp i Parch
variables <- c("Age", "SibSp", "Parch")
for (name in variables){
  boxplot(passatgers_2[,name], main=name)
}
# Valors extrems
boxplot.stats(passatgers_2$Age)$out
boxplot.stats(passatgers_2$SibSp)$out
boxplot.stats(passatgers_2$Parch)$out

# Passatgers amb 8 germans i cònjugues
passatgers[which(passatgers$SibSp == 8),1:6]
passatgers[which(passatgers$SibSp == 8),7:12]
# Passatgers amb 8 germans i cònjugues (test)
passatgers_test[which(passatgers_test$SibSp == 8),1:6]
passatgers_test[which(passatgers_test$SibSp == 8),7:11]

# Passatgers amb 5 germans i cònjugues
passatgers[which(passatgers$SibSp == 5),1:6]
passatgers[which(passatgers$SibSp == 5),7:12]
# Passatgers amb 5 germans i cònjugues (test)
passatgers_test[which(passatgers_test$SibSp == 5),1:6]
passatgers_test[which(passatgers_test$SibSp == 5),7:11]

# Valors extrems sense 0
boxplot.stats(passatgers_2$Parch[which(passatgers_2$Parch != 0)])$out

# Passatgers amb 6 pares o fills
passatgers[which(passatgers$Parch == 6),1:6]
passatgers[which(passatgers$Parch == 6),7:12]
# Passatgers amb 6 pares o fills (test)
passatgers_test[which(passatgers_test$Parch == 6),1:6]
passatgers_test[which(passatgers_test$Parch == 6),7:12]

#Assignar lletres als factors de Pclass i Sex
library(car)
passatgers_2$Pclass <- recode(passatgers_2$Pclass,"1='H';2='M';3='L'")
summary(passatgers_2$Pclass)
passatgers_2$Sex <- recode(passatgers_2$Sex,"'male'='M';'female'='F'")
summary(passatgers_2$Sex)

# Reordenem la variable Pclass en la nova variable PclassR
passatgers_2$PclassR <- relevel(passatgers_2$Pclass, ref = "H")
head(passatgers_2$PclassR)
# Reordenem la variable Sex en la nova variable SexR
passatgers_2$SexR <- relevel(passatgers_2$Sex, ref = "F")
head(passatgers_2$SexR)

# Generem el model
model_logit <- glm(passatgers_2$Survived ~ passatgers_2$PclassR 
                   + passatgers_2$SexR + passatgers_2$Age + passatgers_2$SibSp
                   + passatgers_2$Parch, family = "binomial")
summary(model_logit)

# Generem el model sense Parch
model_logit_2 <- glm(passatgers_2$Survived ~ passatgers_2$PclassR 
                     + passatgers_2$SexR + passatgers_2$Age + passatgers_2$SibSp
                     , family = "binomial")
summary(model_logit_2)


#coefficients
coeff_beta <- coefficients(model_logit_2)
coeff_beta

# Calculem els valors predits
valors_predits <- predict(model_logit_2,passatgers_2,type = "response")
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

stripchart(valors_predits~passatgers_2$PclassR,pch=19)

passatgers_2$Ppre <- valors_predits
passatgers_2$Pre <- data$pre
p_low_sur_pre <- passatgers_2[which(passatgers_2$Ppre >= 0.7 & passatgers_2$PclassR == 'L'),]
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
