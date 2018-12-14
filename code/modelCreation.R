
suppressMessages(library(randomForest))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))



importData <- function (file){
  
  setas<-read.csv(file, header = TRUE)
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
  # En el dataset poe B o y T y en la despcripcion None one and two. 
  levels(setas$ring_number) <- c("b","none", "one", "two")
  levels(setas$ring_type) <- c("cobwebby","evanescent", "flaring", "large", "none", "pendant", "sheathing", "zone")
  levels(setas$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                       "green", "purple", "white", "yellow")
  levels(setas$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
  levels(setas$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")
  return(setas)
}


trainModel <- function (dataset){
  
  model <- randomForest(edibility ~ ., ntree = 80, data = dataset)
  return (model)
}

setas<-importData("../inputs/mushroomClean.csv")
model_rf=trainModel(setas)
saveRDS(model_rf, "model.rds")
