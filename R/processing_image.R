#' Processing the image stack
#'
#' This function is to process the image stack, identifies the object from the images and measures the characteristic of the objects.
#' First thing that fucntion do is adjust all images to be at the same brightness. Using the first image as the reference and adjust the median of intensity of the image to be same as median of intensity of the first image
#' Afterward, the function convert the image into a negative image and filter out the pixel with intensity less than 0.72. The function then fill the hull in the object.
#' The image then subjected for object identification and paint the original image with circling around the object. The identified objects will be measures then.
#' 
#' @param image_vector The single channel image stack need to be processed
#' @return The results from each step of the processing
#' @export

processing_image=function(image_vector){
  n=tail(dim(image_vector),1)
  image_cont<<-image_vector[,,1]*(0.43137/median(density(image_vector[,,1])$x)*1.9)
  for (i in 2:n){
    temp=image_vector[,,i]*(0.43137/median(density(image_vector[,,i])$x)*1.9)
    image_cont<<-combine(image_cont,temp)
  }
  image_neg_contrast<<-max(image_cont)-image_cont
  filtered_image<<-image_neg_contrast>0.72
  fh_image<<-fillHull(filtered_image)
  label_image<<-bwlabel(fh_image)
  paint_all_image<<-paintObjects(label_image,image_vector)
  for (i in 1:n){
    assign(paste("shape_objects",i,sep="_"),computeFeatures.shape(label_image[,,i]),envir = .GlobalEnv)
    assign(paste("moment_objects",i,sep="_"),computeFeatures.moment(label_image[,,i]),envir = .GlobalEnv)
    assign(paste("shape_moment_objects",i,sep = "_"),cbind(eval(parse(text=paste("shape_objects",i,sep = "_"))),eval(parse(text=paste("moment_objects",i,sep = "_")))),envir = .GlobalEnv)
  }
}