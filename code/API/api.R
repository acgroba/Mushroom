

library(shiny)
library(shinythemes)

suppressMessages(library(randomForest))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))


ui <- fluidPage(theme = shinytheme("superhero"),
  
  
  #LOGO
  fluidRow(
    column(4, titlePanel(title=div(img(src="logo.png", width="215", height="180"), align="center")))
  ),
  #SELECT INPUTS
  fluidRow(
      column(2, selectInput(inputId="cap_shape" , choices= c("bell", "conical", "flat", "knobbed", "sunken", "convex"), label="Cap shape:"),
      selectInput(inputId="cap_color" , choices= c("buff", "cinnamon", "red", "gray", "brown", "pink","green", "purple", "white", "yellow"), label="Cap color:"),
      selectInput(inputId="cap_surface" , choices= c("fibrous", "grooves", "scaly", "smooth"), label="Cap surface:"),
      selectInput(inputId="bruises" , choices= c("no", "yes"), label="Bruises?:"),
      selectInput(inputId="odor" , choices= c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy"), label="Odor:"),
      selectInput(inputId="gill_attachement" , choices= c("attached","descending", "free","notched"), label="Gill attachement:"),
      selectInput(inputId="gill_spacing" , choices= c("close", "crowded","distant"), label="Gill spacing:"),
      selectInput(inputId="gill_size" , choices= c("broad", "narrow"), label="Gill size:"),
      selectInput(inputId="gill_color" , choices= c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow"), label="Gill color:"),
      selectInput(inputId="stalk_shape" , choices= c("enlarging", "tapering"), label="Stalk shape:"),
      selectInput(inputId="stalk_root" , choices= c("bulbous", "club", "cup", "equal","rhizomorphs", "rooted", "missing"), label="Stalk root:")
      ),
 
      column(2, selectInput(inputId="stalk_surface_above_ring" , choices= c("fibrous", "silky", "smooth", "scaly"), label="Stalk surface above ring:"),
      selectInput(inputId="stalk_surface_below_ring" , choices= c("fibrous", "silky", "smooth", "scaly"), label="Stalk surface below ring:"),
      selectInput(inputId="stalk_color_above_ring" , choices=  c("buff", "cinnamon", "red", "gray", "brown", "pink","green", "purple", "white", "yellow"), label="Stalk color above ring:"),
      selectInput(inputId="stalk_color_below_ring" , choices= c("buff", "cinnamon", "red", "gray", "brown", "pink","green", "purple", "white", "yellow"), label="Stalk color below ring:"),
      selectInput(inputId="veil_type" , choices= c("partial", "universal"), label="Veil type:"),
      selectInput(inputId="veil_color" , choices= c("brown", "orange", "white", "yellow"), label="Veil color:"),
      selectInput(inputId="ring_number" , choices= c("b","none", "one", "two"), label="Ring number:"),
      selectInput(inputId="ring_type" , choices=  c("cobwebby","evanescent", "flaring", "large", "none", "pendant", "sheathing", "zone"), label="Ring type:"),
      selectInput(inputId="spore_print_color" , choices= c("buff", "chocolate", "black", "brown", "orange", "green", "purple", "white", "yellow"), label="Spore print color:"),
      selectInput(inputId="population" , choices= c("abundant", "clustered", "numerous", "scattered", "several", "solitary"), label="Population:"),
      selectInput(inputId="habitat" , choices= c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste"), label="Habitat:")
      
      ),
      #RESULT
      column(4,
             imageOutput("response")
      )
  ),
  #BUTTON
  fluidRow(
    column(4, actionButton(inputId="classify",label="Classify" ),align="center")
  )
  

 
)

server <- function(input, output, session) {

  
  model_rf=readRDS("model.rds")
   
  observeEvent(input$classify, {
  
  #Due to a peculiarity in random forest algorithm for R, we have to asure that new data has the same levels as training data
  seta=data.frame( cap_shape="", cap_surface="", 
                cap_color="", bruises="", odor="", 
                gill_attachement="", gill_spacing="", gill_size="", 
                gill_color="", stalk_shape="", stalk_root="", 
                stalk_surface_above_ring="", stalk_surface_below_ring="", stalk_color_above_ring="", 
                stalk_color_below_ring="", veil_type="", veil_color="", 
                ring_number="", ring_type="", spore_print_color="", 
                population="", habitat="")

levels(seta$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(seta$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                            "green", "purple", "white", "yellow")
levels(seta$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(seta$bruises) <- c("no", "yes")
levels(seta$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(seta$gill_attachement) <- c("attached","descending", "free","notched")
levels(seta$gill_spacing) <- c("close", "crowded","distant")
levels(seta$gill_size) <- c("broad", "narrow")
levels(seta$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                             "pink", "green", "purple", "white", "yellow")
levels(seta$stalk_shape) <- c("enlarging", "tapering")
levels(seta$stalk_root) <- c("bulbous", "club", "cup", "equal","rhizomorphs", "rooted", "missing")
levels(seta$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(seta$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(seta$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                         "green", "purple", "white", "yellow")
levels(seta$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                         "green", "purple", "white", "yellow")
levels(seta$veil_type) <- c("partial", "universal")
levels(seta$veil_color) <- c("brown", "orange", "white", "yellow")
levels(seta$ring_number) <- c("b","none", "one", "two")
levels(seta$ring_type) <- c("cobwebby","evanescent", "flaring", "large", "none", "pendant", "sheathing", "zone")
levels(seta$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                    "green", "purple", "white", "yellow")
levels(seta$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(seta$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

#Reading from inputs
seta$cap_shape[1]=as.factor(input$cap_shape)
seta$cap_surface[1]=as.factor(input$cap_surface)
seta$cap_color[1]=as.factor(input$cap_color)
seta$bruises[1]=as.factor(input$bruises)
seta$odor[1]=as.factor(input$odor)
seta$gill_attachement[1]=as.factor(input$gill_attachement)
seta$gill_spacing[1]=as.factor(input$gill_spacing)
seta$gill_size[1]=as.factor(input$gill_size)
seta$gill_color[1]=as.factor(input$gill_color)
seta$stalk_shape[1]=as.factor(input$stalk_shape)
seta$stalk_root[1]=as.factor(input$stalk_root)
seta$stalk_surface_above_ring[1]=as.factor(input$stalk_surface_above_ring)
seta$stalk_surface_below_ring[1]=as.factor(input$stalk_surface_below_ring)
seta$stalk_color_above_ring[1]=as.factor(input$stalk_color_above_ring)
seta$stalk_color_below_ring[1]=as.factor(input$stalk_color_below_ring)
seta$veil_type[1]=as.factor(input$veil_type)
seta$veil_color[1]=as.factor(input$veil_color)
seta$ring_number[1]=as.factor(input$ring_number)
seta$ring_type[1]=as.factor(input$ring_type)
seta$spore_print_color[1]=as.factor(input$spore_print_color)
seta$population[1]=as.factor(input$population)
seta$habitat[1]=as.factor(input$habitat )

#Prediction
 predict.output<-predict(model_rf, newdata=seta)

 #Response
 output$response <- renderImage({
   filename=""
   if (predict.output=="poisonous"){
     filename <- normalizePath(file.path('./www',
                                         paste('poisonous', input$n, '.png', sep='')))
   }
   
   if(predict.output=="edible"){
     filename <- normalizePath(file.path('./www',
                                         paste('edible', input$n, '.png', sep='')))
   }
   # Return a list containing the filename and alt text
   list(src = filename,
        alt = paste("Result", input$n))
   
 }, deleteFile = FALSE)
 
 
})
  
}




shinyApp(ui, server)
