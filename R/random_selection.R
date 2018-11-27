#' Random selection of image object from image stack
#' 
#' To establish the training set for cell type classifier, a pre-determined classification of object has to be perfermed.
#' In order to get a random set of image for manual classification which will be used as training set for later machine learning steps.
#' 
#' @param image_stack the object stacks that the user is going to work on. The import should be a string containing name of object(s) in GlobalEnvironment
#' @param seed The seed to be set for the randomnization. This is an optional parameter intending to create the reproducibility of the randomnizaiton process
#' @return selected frames images in an image stack and the number of the frame in original stack from each of the stacks user input
#' @export
random_selection=function(image_stack,seed){
  if (is.null(seed)!=TRUE){
    set.seed(seed)
  }
  n_stacks=length(image_stack)
  frames_each=numeric(length = n_stacks)
  frames=0
  for (i in 1:n_stacks){
    frames_each[i]=tail(dim(eval(parse(text=image_stack[i]))),1)
    frames=frames+frames_each
  }
  all=seq(1,frames)
  selected=sample(all,all/3)
  selected_1_num=selected[selected<=frames_each[1]]
  selected_1<<-eval(parse(text=image_stack[1]))[,,,selected_1_num]
  all_previous=tail(dim(eval(parse(text = image_stack[1]))),1)
  selected_all=selected_1
  if (n_stacks>1){
    for (i in 2:n_stacks){
      name=paste("selected",i,sep = "_")
      name_num=paste("selected",i,"num",sep = "_")
      numbers=tail(dim(eval(parse(text = image_stack[i]))),1)
      max_num=all_previous+numbers
      assign(name_num,selected[all_previous<selected&selected<=max_num],envir=.GlobalEnv)
      assign(name,eval(parse(text = image_stack[i]))[,,,eval(parse(text = name_num))],envir = .GlobalEnv)
      selected_all<<-combine(selected_all,eval(parse(text = name)))
    }
  }
}