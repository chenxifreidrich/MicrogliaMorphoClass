#' Record cell type
#' 
#' This function/ program is for user to perform the classification of objects that is randomly selected.
#' 
#' @param image_stack The image stack that the user is going to determine the class of microglia activation status 
#' @return A data matrix recorded the type of microglia activation status that the user decided
#' @export

record_cell_type=function(image_stack){
  n=dim(image_stack)[4]
  if (exists("type_matrix") !=TRUE){
    type_matrix<<-matrix(nrow = n,ncol = 1)
    rownames(type_matrix)<<-seq(1,n)
    colnames(type_matrix)<<-"object type code"
  }
  j=1
  while (j <= n){
    if (is.na(type_matrix[j,1])){
      break;
    }
    else{
      j=j+1
    }
  }
  for (i in j:n){
    display(image_stack[,,,i],method = "raster")
    head_line=paste("Object showing in the window belongs to: 1) miscellous object 2) homeostatic microglia 3) bushy microglia 4) round microglia 5) infiltrating macrophages? type the code for object class here","(",i,"/",n,")","(To Save and Quit,Type in 'Quit'" ,":",sep = "")
    type_object=readline(prompt = head_line)
    if (type_object=="Quit"){
      print("Saving data...")
      save.image(file = data_file_path)
      print("Finish")
      break;
    }
    else {
      while (type_object %in% c("1","2","3","4","5") !=TRUE){
        print("Input is not indicating any of the 5 categories of object type")
        type_object=readline(prompt = head_line)
      }
    }
    type_matrix[i,1]<<-as.numeric(type_object)
  }
  save.image(file = data_file_path)
}