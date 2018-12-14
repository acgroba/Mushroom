suppressMessages(library(caret))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(gridExtra))
suppressMessages(library(gmodels))
suppressMessages(library(ggparallel))
suppressMessages(library(rpart.plot))
suppressMessages(library(sqldf))
suppressMessages(library(tidyverse))
suppressMessages(library(vcd))
suppressMessages(library(GoodmanKruskal))
suppressMessages(library(dismo))
suppressMessages(library(MLmetrics))
suppressMessages(library(ROCR))
suppressMessages(library(e1071))
suppressMessages(library(randomForest))

#Importacion data set
setas<-read.csv("../inputs/mushrooms.csv", header = TRUE)

#Descripcion data set
head(setas)
dim(setas)
glimpse(setas)

number_class <- function(x){
  x <- length(levels(x))
}
x <- setas %>% map_dbl(function(.x) number_class(.x)) %>% as_tibble() %>% 
  rownames_to_column() %>% arrange(desc(value))
colnames(x) <- c("Variable name", "Number of levels")
print(x)

#Missing values
map_dbl(setas, function(.x) {sum(is.na(.x))})

#Formateo

colnames(setas) <- c("edibility", "cap_shape", "cap_surface", 
                     "cap_color", "bruises", "odor", 
                     "gill_attachement", "gill_spacing", "gill_size", 
                     "gill_color", "stalk_shape", "stalk_root", 
                     "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                     "stalk_color_below_ring", "veil_type", "veil_color", 
                     "ring_number", "ring_type", "spore_print_color", 
                     "population", "habitat")

levels(setas$edibility) <- c("edible", "poisonous")
levels(setas$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(setas$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                             "green", "purple", "white", "yellow")
levels(setas$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(setas$bruises) <- c("no", "yes")
levels(setas$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(setas$gill_attachement) <- c("attached","descending", "free","notched")
levels(setas$gill_spacing) <- c("close", "crowded","distant")
levels(setas$gill_size) <- c("broad", "narrow")
levels(setas$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                              "pink", "green", "purple", "white", "yellow")
levels(setas$stalk_shape) <- c("enlarging", "tapering")
levels(setas$stalk_root) <- c("bulbous", "club", "cup", "equal","rhizomorphs", "rooted", "missing")
levels(setas$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(setas$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(setas$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                          "green", "purple", "white", "yellow")
levels(setas$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                          "green", "purple", "white", "yellow")
levels(setas$veil_type) <- c("partial", "universal")
levels(setas$veil_color) <- c("brown", "orange", "white", "yellow")
# En el dataset pone B o y T y en la despcripcion None one and two. 
levels(setas$ring_number) <- c("b","none", "one", "two")
levels(setas$ring_type) <- c("cobwebby","evanescent", "flaring", "large", "none", "pendant", "sheathing", "zone")
levels(setas$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                     "green", "purple", "white", "yellow")
levels(setas$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(setas$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

glimpse(setas)

#Distribuciones


barplot(table(setas$edibility), xlab = "Edibility", ylab = "Number of examples",
        main = "Edibility distribution",ylim=c(0,5000))
barplot(table(setas$cap_shape), xlab = "Shape", ylab = "Number of examples",
        main = "Cap shape distribution",ylim=c(0,3000))
barplot(table(setas$cap_surface), xlab = "Surface", ylab = "Number of examples",
        main = "Cap surface distribution",ylim=c(0,3000))
barplot(table(setas$cap_color), xlab = "Color", ylab = "Number of examples",
        main = "Cap color distribution",ylim=c(0,1800))
barplot(table(setas$bruises), xlab = "Bruises", ylab = "Number of examples",
        main = "Bruises distribution",ylim=c(0,5000))
barplot(table(setas$odor), xlab = "Odor", ylab = "Number of examples",
        main = "Odor distribution",ylim=c(0,2500))
barplot(table(setas$gill_attachement), xlab = "Gill attachement", ylab = "Number of examples",
        main = "Gill attachement distribution",ylim=c(0,5000))
barplot(table(setas$gill_spacing), xlab = "Gill spacing", ylab = "Number of examples",
        main = "Gill spacing distribution",ylim=c(0,5000))
barplot(table(setas$gill_size), xlab = "Gill size", ylab = "Number of examples",
        main = "Gill size distribution",ylim=c(0,5000))
barplot(table(setas$gill_color), xlab = "Gill color", ylab = "Number of examples",
        main = "Gill color distribution",ylim=c(0,1500))
barplot(table(setas$stalk_shape), xlab = "Stalk shape", ylab = "Number of examples",
        main = "Stalk shape distribution",ylim=c(0,5000))
barplot(table(setas$stalk_root), xlab = "Stalk root", ylab = "Number of examples",
        main = "Stalk root distribution",ylim=c(0,2500))
barplot(table(setas$stalk_surface_above_ring), xlab = "Stalk surface above ring", ylab = "Number of examples",
        main = "Stalk surface above ring distribution",ylim=c(0,3500))
barplot(table(setas$stalk_surface_below_ring), xlab = "Stalk surface below ring", ylab = "Number of examples",
        main = "Stalk surface below ring distribution",ylim=c(0,3500))
barplot(table(setas$stalk_color_above_ring), xlab = "Stalk color above ring", ylab = "Number of examples",
        main = "Stalk color above ring distribution",ylim=c(0,3000))
barplot(table(setas$stalk_color_below_ring), xlab = "Stalk color below ring", ylab = "Number of examples",
        main = "Stalk color below ring distribution",ylim=c(0,3000))
barplot(table(setas$veil_type), xlab = "Veil type", ylab = "Number of examples",
        main = "Veil type distribution",ylim=c(0,7000))
barplot(table(setas$veil_color), xlab = "Veil color", ylab = "Number of examples",
        main = "Veil color distribution",ylim=c(0,6000))
barplot(table(setas$ring_number), xlab = "Ring number", ylab = "Number of examples",
        main = "Ring number distribution",ylim=c(0,6000))
barplot(table(setas$ring_type), xlab = "Ring type", ylab = "Number of examples",
        main = "Ring type distribution",ylim=c(0,3000))
barplot(table(setas$spore_print_color), xlab = "Spore print color", ylab = "Number of examples",
        main = "Spore print color distribution",ylim=c(0,2000))
barplot(table(setas$population), xlab = "Population", ylab = "Number of examples",
        main = "Population distribution",ylim=c(0,3000))
barplot(table(setas$habitat), xlab = "Habitat", ylab = "Number of examples",
        main = "Habitat",ylim=c(0,2500))

#Distribuciones conjuntas

ggplot(setas, aes(x = edibility, y = cap_shape, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = cap_surface, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = cap_shape, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = cap_color, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = bruises, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = odor, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = gill_attachement, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = gill_spacing, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = gill_size, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = gill_color, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = stalk_shape, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = stalk_root, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")
ggplot(setas, aes(x = edibility, y = stalk_surface_above_ring, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = stalk_surface_below_ring, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = veil_type, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = veil_color, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = ring_number, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = ring_type, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = spore_print_color, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = population, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

ggplot(setas, aes(x = edibility, y = habitat, col = edibility,)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red") ) + theme(legend.position = "none")

#cap_surface vs cap_shape
library(ggplot2)
ggplot(setas, aes(x = cap_surface, y = cap_shape, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

#gill_spacing vs gill_size 
ggplot(setas, aes(x = gill_spacing, y = gill_size, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

#habitat vs population
library(ggplot2)
ggplot(setas, aes(x = habitat, y = population, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

#habitat vs odor  
ggplot(setas, aes(x = habitat, y = odor, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))


# gill_color y cap_color
ggplot(setas, aes(x = gill_color, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
#Chi cuadrado test
#cap_shape vs cap_surface
chisq.test(setas$cap_shape, setas$cap_surface, correct = FALSE)
#gill_spacing vs gill_size 
chisq.test(setas$gill_spacing, setas$gill_size, correct = FALSE)
#habitat vs population
chisq.test(setas$habitat, setas$population, correct = FALSE)
#habitat vs odor 
chisq.test(setas$habitat, setas$odor, correct = FALSE)
# gill_color y cap_color
chisq.test(setas$gill_color, setas$cap_color, correct = FALSE)


#Goodman-Kruskal
varset1<- c("edibility","cap_shape","cap_surface","gill_spacing","gill_size","gill_color", "habitat", "population", "odor")
setas2<- subset(setas, select = varset1)
GKmatrix1<- GKtauDataframe(setas2)
plot(GKmatrix1, corrColors = "blue")

#Dataset limpio
setas_clean <- setas[which (!((setas$gill_color=='yellow' & setas$cap_color=='purple') | (setas$gill_color=='green' & setas$cap_color=='green') | (setas$gill_color=='red' & setas$cap_color=='cinnamon') |(setas$gill_color=='red' & setas$cap_color=='green')|(setas$ring_number=='none'))),]
write.csv(setas_clean, file = "../inputs/mushroomClean.csv", row.names=FALSE)

#Separacion en train y test
trainIndex<-createDataPartition(setas_clean$edibility, p=.8, list=FALSE)
train_set<-setas_clean[trainIndex,]
test_set<-setas_clean[-trainIndex,]

kfold_indexes = kfold(setas_clean,10);

#Naive bayes

t <- proc.time()
model_nbayes<-naiveBayes(edibility~., data=train_set)
proc.time()-t 
t <- proc.time()
prediction<-predict(model_nbayes, newdata=test_set)
proc.time()-t 
confusionMatrix(data=prediction, reference=test_set$edibility, positive="edible")

#Support Vector Machine

t <- proc.time()
tuned = tune.svm(edibility~., data = train_set, gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=10))
print(tuned$best.model)
proc.time() -t

#Random forest
model_rf <- randomForest(edibility ~ ., ntree = 500, data = train_set)
plot(model_rf)

t <- proc.time()
model_rf=randomForest(edibility ~ ., ntree = 80, data = train_set)
proc.time()-t
t <- proc.time()
test_rf <- predict(model_rf, newdata = test_set)
proc.time()-t
accuracy=0
recall=0
precision=0
sensitivity=0
specificity=0

table(test_rf, test_set$edibility)

setas_clean <- setas[which (!((setas$gill_color=='yellow' & setas$cap_color=='purple') | (setas$gill_color=='green' & setas$cap_color=='green') | (setas$gill_color=='red' & setas$cap_color=='cinnamon') |(setas$gill_color=='red' & setas$cap_color=='green')|(setas$ring_number=='none'))),]