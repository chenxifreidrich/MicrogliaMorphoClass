#' Import Images
#' 
#' import all the images from the working directory into a image stack. this code wraps the orginal readImage from EBImage package (4.22.1) with user-friendly interactions to choose the files and make massive processing of images feasible
#' 
#' @param directory The directory that contains the images that user will be working on
#' @return The function returns and stores a Image class object in the Global Environment that contains the images user choose to import
#' @export


import_images=function(directory){
  file_list=as.character(list.files(path = directory,full.names = TRUE))
  print("Files in the directory you choose are:")
  print(file_list)
  choice=readline(prompt = "The items wanted to be imported are: type in the numbers of the items and seperate each with single space")
  choice_vec=as.numeric(unlist(strsplit(choice,split = " ")))
  file_to_import=file_list[choice_vec]
  n=length(file_to_import)
  original_files<<-readImage(file_to_import[1],names = file_to_import[1])
  for (i in 2:n){
    temp=readImage(files = file_to_import[i],names = file_to_import[i])
    original_files<<-combine(original_files,temp)
  }
  number_of_image<<-n
}