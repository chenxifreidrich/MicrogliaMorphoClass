#' Generating Object Stack
#' 
#' generating object stack. This code wraps the EBImage function stackObjects in order to make the function compatible with the massive images processing
#' 
#' @param label_image The object identified labeled image stack 
#' @param image The original image that the labeled image was generating from
#' @return A Image class image stack that contains the obejct identified
#' @export

gen_object_stack<-function(label_image,image){
  n=tail(dim(label_image),1)
  if(n!=tail(dim(image),1)){
    print("The labeled image has to be paired with its original image")
    break;
  }
  fore_name=deparse(substitute(label_image))
  for (i in 1:n){
    name=paste(fore_name,"Object","stack",i,sep = "_")
    assign(name,stackObjects(label_image[,,i],image[,,,i]),envir = .GlobalEnv)
  }
}