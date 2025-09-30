rm(list = ls())

library(readxl)

#--- 1.
titanic_place <- read_excel('./test_files/titanic_place.xlsx')

#--- 2.
summary(titanic_place)

#--- 3.
titanic_place$Classe <- as.character(titanic_place$Classe)

#--- 4.
any(duplicated(titanic_place)) #... pas de doublons

#--- 5.

# a. Première solution
head(titanic_place[order(as.numeric(titanic_place$Prix), decreasing = TRUE),], n = 1)

# b. Deuxième solution
max_price <- max(as.numeric(titanic_place$Prix))
titanic_place[which(titanic_place$Prix == as.character(max_price)),]

#--- 6. 
# Ici on utilise la deuxième solution de la 5.
min_price <- min(as.numeric(titanic_place$Prix))
titanic_place[which(titanic_place$Prix == as.character(min_price)),]

#--- 7.
summary(titanic_place$Prix)
boxplot(as.numeric(titanic_place$Prix), main = 'Distribution des prix des billets pour le Titanic', ylab = 'Prix')

#--- 8.
summary(as.factor(titanic_place$Classe))

t <- table(titanic_place$Classe)

#--- 9.
prop.table(t)

#--- 10.

# a.
aggregate(titanic_place$Survie, by = list(titanic_place$Classe), sum)

# b.
tapply(titanic_place$Survie, titanic_place$Classe, sum)

#--- 11. Déjà fait.

#--- 12. 
c1 <- read.csv2('./test_files/titanic_client1.csv', sep = ',')
c2 <- read.csv2('./test_files/titanic_client2.csv', sep = ',')
c3 <- read.csv2('./test_files/titanic_client3.csv', sep = ',')

titanic_client <- rbind(c1, c2, c3)

#--- 13.
question13 <- read_excel("./test_files/titanic_place.xlsx") 
question13$Prix <- as.factor(question13$Prix)

#--- 14.
id14_15_16 <- question13[question13$id %in% c("id14","id15","id16"),]
id14_15_16$Prix <- as.numeric(id14_15_16$Prix) # Pas bien, car renvoie les modalités plutôt que les valeurs : fausse toutes les données.

#--- 15.
rm(id14_15_16)

#--- 16.
question13$Prix <- as.numeric(as.character(question13$Prix))

#--- 17.
dup <- duplicated(titanic_client)
any(dup) #... oui

titanic_client <- titanic_client[-dup,]
titanic_client$Age <- as.numeric(titanic_client$Age)

#--- 18.
titanic_passager <- merge(titanic_client, titanic_place, by = 'id')

#--- 19.
titanic_svm <- titanic_passager[-which(is.na(titanic_passager$Age)),]

#--- 20.
titanic_svm$Sexe <- factor(
  ifelse(titanic_svm$Civilite %in% c('ColM', 'DrM', 'Master', 'Mr', 'Major', 'Rev'), 'Homme', 
      ifelse(titanic_svm$Civilite %in% c('DrF', 'Miss', 'Mrs', 'Ms', 'Mme', 'Mlle', 'ColF', 'Dona'), 'Femme', 'Inconnu')
  )
)

#--- 21.
mean(titanic_svm$Age)
mean(titanic_svm[titanic_svm$Sexe == 'Femme',]$Age)
mean(titanic_svm[titanic_svm$Sexe == 'Homme',]$Age)

#--- 22.
boxplot(titanic_svm$Age ~ titanic_svm$Classe)

#--- 23.
length(which(titanic_svm$Survie == 1 & titanic_svm$Sexe == 'Homme' & titanic_svm$Classe == '1'))
length(which(titanic_svm$Survie == 1 & titanic_svm$Sexe == 'Homme' & titanic_svm$Classe == '2'))
length(which(titanic_svm$Survie == 1 & titanic_svm$Sexe == 'Homme' & titanic_svm$Classe == '3'))

#--- 24.
titanic_svm$Sexe_modif <- factor(ifelse(titanic_svm$Age > 12, ifelse(titanic_svm$Sexe == 'Homme', 'Homme', 'Femme'), 'Enfant'))

#--- 25.
t <- table(titanic_svm$Sexe_modif)
t
p1 <- prop.table(t)

#--- 26.
t <- table(titanic_svm[titanic_svm$Survie == '1',]$Sexe_modif)
t
p2 <- prop.table(t)

#--- 27.


#--- 28.
table(titanic_svm$PortEmbarq, titanic_svm$Classe)

#--- 29.
table(titanic_svm$PortEmbarq, titanic_svm$Classe, titanic_svm$Survie)

