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




