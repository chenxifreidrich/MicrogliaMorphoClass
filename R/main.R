#' Main user interface that runs the whole pipeline
#' 
#' This is to make the functions that present in the package be easier to use.
#' 
#' @export
#' 
#' 

MgMorphoClass<-function(){
  cat("Classifier for microglia activation status v0.1.0 \n")
  Sys.sleep(1)
  cat("Checking the dependency availability....\n")
  while ("EBImage" %in% installed.packages() !=TRUE){
    cat("Installing EBImage...\n")
    source("http://bioconductor.org/biocLite.R")
    biocLite("EBImage")
  }
  cat("All required packages are installed....\n")
  cat("Loading required packages...\n")
  library(EBImage)
  Sys.sleep(1)
  t_f=readline(prompt = "Please type in the directory that contains the image files you would like to work on.\n")
  cat("Importing images...\n")
  import_images(t_f)
  cat("Extracting 3rd channel from all images...\n")
  get_3_channel(original_files)
  cat("The images will be processed to have the same brightness and then filtered for objects recognition.\n Recognized objects will be measured for its morphology")
  processing_image(image_stack_f3)
  cat("Exporting objects identified. Individual section will have one individual object in environment contain all objects identified\n")
  gen_object_stack(label_image = label_image,image = original_files)
  cat("To build a training and verification dataset, we here will randomly select 1/3 of objects identified\n")
  if (readline(prompt = "Would you like to pick a seed for randomization which allows reproducibility of this random selected objects?(y/n)")=="y"){
    seed<<-readline(prompt = "Please type the seed number as you wish:")
  }
  stacks=ls(name = .GlobalEnv,pattern = "Object_stack")
  if (exists("seed",envir = .GlobalEnv)){
    random_selection(image_stack = stacks,seed = seed)
  }
  else{
    random_selection(image_stack = stacks)
  }
  cat("The training and verification set has been selected...\n")
  cat("Now, we will tell the computer what are those selected...\n")
  record_cell_type(selected_all)
  cat("All objects are assigned with one class for training\n")
  cat("Saving data...\n")
  save.image(file = paste(t_f,"Classifier_data.RData"))
}