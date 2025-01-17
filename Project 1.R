# Exercice 1

#Question 1
dat =read.table("Pet_Licenses.txt", sep=",", header=TRUE, stringsAsFactors=TRUE, quote="\"", fill=TRUE)

#Question 2

dim(dat) # 7 variables et 43683 individus

#Question3:le nombre de modalités de la variable species .

length(levels(dat$Species))

# 4 modalites "cat","Dog", "Goat","Pig"
#le nombre d’individus possédant chaque modalité

table(dat$Species)

#Question 4:fonctions names, table et sort donner les 10 noms de chiens et les 10 noms de chats les plus populaires.
names(dat)

#les 10 noms de chiens les plus populaires
name_Dog= dat$Animal.s.Name[dat$Species=="Dog"]
table(name_Dog)
sort(table(name_Dog),decreasing = TRUE)[1:10]

#les 10 noms de chats les plus populaires
name_Cat=dat$Animal.s.Name[dat$Species=="Cat"]
length(name_Cat)
table(name_Cat)
sort(table(name_Cat),decreasing = TRUE)[1:10]  

# 5 race de chiens les plus pop
sort(table(dat$Primary.Breed[dat$Species=="Dog"]),decreasing = TRUE)[1:5]

#Question 5
#une liste contenant les 30 noms de chien les plus populaires 
dat_Dog =names(sort(table(name_Dog),decreasing = TRUE)[1:30])
dat_Dog

#une liste contenant les 30 races primaires de chien les plus populaires.
dat_Dog_Breed =names(sort(table(dat$Primary.Breed[dat$Species=="Dog"]),decreasing = TRUE)[1:30])
dat_Dog_Breed

#Créer un jeu de données dat_pop ne contenant que les licences de chiens de compagnie dont le nom etla race primaire sont parmi les 30 plus populaires.

names(dat)
dat_pop = dat[dat$Animal.s.Name %in% dat_Dog & dat$Primary.Breed %in% dat_Dog_Breed,]
names(dat_pop)

# Question 6 Retirer les modalités inutilisées des variables Animal.s.Name et Primary.Breed avec la fonction droplevels
dat_pop$Animal.s.Name= droplevels(dat_pop$Animal.s.Name)
length(levels(dat_pop$Animal.s.Name))
dat_pop$Primary.Breed= droplevels(dat_pop$Primary.Breed)
length(levels(dat_pop$Primary.Breed))

#Question 7: la table de contingence entre les variables Animal.s.Name et Primary.Breed sur les données de dat_pop
table_ctg=table(dat_pop$Animal.s.Name,dat_pop$Primary.Breed)
print(table_ctg)
#tracer sa carte de chaleur avec la fonction heatmap
heatmap(table_ctg)

#Question 8: Visuellement, pour quelle race de chien les noms "Milo", "Ollie" et "Leo" sont-ils les plus populaires
# le nom Milo est le plus populaire pour les Retriever, Labrador
# les noms "Ollie" et "Leo" sont les plus populaires pour les Retriever, Labrador et les Retriever, Golden

#Question 9
#Effectuer une AFC sur cette table de contingence.
library(FactoMineR)
res= CA(table_ctg, graph= FALSE )

#Tracer le nuage des modalités en dimension 2
plot(res)
#pourcentage d’inertie: 22.57%
# Que dire sur la qualité de cette AFC? : 
rowSums(res$row$cos2[,1:2])
rowSums(res$col$cos2[,1:2])
