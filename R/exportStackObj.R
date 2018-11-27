#' Export the Object Stack
#' 
#' massive export of image of objects
#' 
#' @param stack The object stack where the exporting images of individual object is from
#' @param tag The name of the experiment or anything that identifies the experiment or slide. It should a string that does not contain space in between
#' @return The file exports the images of individual object identified that are stored in the image stack as tiff file with bits.per.sample set to 16
#' @export

exportStackObj=function(stack,tag){
  if (is.character(tag) !=TRUE){
    print("Provide correct experiment name and file path as character")
    break;
  }
  slides=dim(stack)[4]
  for (i in 1:slides){
    file_name=paste(tag,"_",i,".tiff",sep = '')
    writeImage(stack[,,,i],file_name,type="tiff",bits.per.sample = 16)
  }
}