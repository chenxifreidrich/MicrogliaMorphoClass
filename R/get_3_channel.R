#' Get 3 channel
#'
#' create the frame 3 for all the images in the image stack that was imported
#' 
#' @param image_vector The original color image stack 
#' @return The single 3rd channel only image
#' @export
get_3_channel=function(image_vector){
  n_f=tail(dim(image_vector),1)
  temp=image_vector[,,,1]
  image_stack_f3=getFrame(temp,3)
  for (i in 2:n_f){
    temp=image_vector[,,,i]
    temp=getFrame(temp,3)
    image_stack_f3=combine(image_stack_f3,temp)
  }
  image_stack_f3<<-image_stack_f3
}