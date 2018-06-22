#' Build the report list.
#' @description Specify the path to the file containing the report list.
#' Build_report reads the list, turns it into your report object, and uploads it
#' to the s3 path you specify.
#' @param report_path The path to your report list.
#' @return Technically does not return anything. Build_report writes the report
#' objects using global assignment, so you'll have the report object in memory
#' after execution, even if a particular function fails.
#' @export
#' @examples build_report('path/to/my/file.R')
build_report <- function(report_path, .disable_tests = FALSE){
  #Messages
  library_error <- paste(
     "Please specify a location for all syberiaReports functions in your syberia.config file"
  )
  test_error <- paste(
     "Please specify a location for all syberiaReports tests in your syberia.config file"
  )
  
  #Create the report environment and object and append the basics
  syberiaReports::make_report_env(.disable_tests)
  .syberiaReport$recipe <- source(report_path)$value
  .syberiaReport$model <- do.call(s3read, as.list(recipe()$model))
  
  #Build out the core report properties
  cat('Initializing Report\n')
  syberiaReports::add_element(syberiaReports::recipe()$save, 'location$report')
  syberiaReports::add_element(syberiaReports::recipe()$model, 'location$model')
  syberiaReports::add_element(syberiaReports::model()$output$model, 'model')
  syberiaReports::add_element(syberiaReports::recipe()$raw_data, 'raw_data')
  syberiaReports::add_element(syberiaReports::lib(), '.env$library')
  syberiaReports::add_element(syberiaReports::tests(), '.env$tests')
  syberiaReports::add_element(syberiaReports::recipe(), '.env$recipe')

  #Score all the listed data sets.
  cat('Scoring the data sets\n')
  sapply(names(syberiaReports::recipe()$scored_data), score_data) %>% 
    {syberiaReports::add_element(., 'scored_data')}

  #Evaluate each of the reporting functions.
  cat('Running reporting functions\n')
  report_on <- function(element){
    cat(names(element))
    do.call(element[[1]], element[-1])
  }
  dummy <- syberiaReports::recipe()$report %>% lapply(report_on)

  #Save the report
  cat(paste('Writing out report to', syberiaReports::recipe()$save,'\n'))
  s3store(syberiaReports::report(), syberiaReports::recipe()$save)
}


#' Score a data set.
#' @description The main function for scoring data sets
#' @param data_name The name of the set you specify in the report_list
#' @return report$scored_data
score_data <- function(data_name){

  #Set up the default options
  options <- unlist(syberiaReports::recipe()$scored_data[[data_name]][-1], recursive=F)
  if(is.null(options$filters)){options$filters <- list('TRUE')}
  if(is.null(options$id_name)){
    options$id_name <- syberiaReports::model()$input[c('id_type','id_var')] %>% 
      unlist %>% unique %>% .[1]
  }
  if(is.null(options$scores)){options$scores <- 'score'}
  if(!is.null(options$predict_method)){
    syberiaReports::model()$input$predict_method <- options$predict_method
  }
  if(is.null(options$dep_var_name)){options$dep_var_name <- 'dep_var'}
  #' GLMNet handles 's' weird. You need to assign predict_method to model$input.
  #' You can't just pass it as an optional parameter to predict. So, if it exists
  #' we gotta handle it special.
  
  #Read in the data
  data <- s3read(syberiaReports::recipe()$scored_data[[data_name]][[1]]) %>%
    {do.call(function(x, ...) filter_(., ...), options$filters)}
  if(is.null(data[[options$id_name]])){
    stop(paste("The column",options$id_name,"doesn't exist. Please set id_name manually."))
  }
  if(options$dep_var_name != FALSE){ 
    if(is.null(data[[options$dep_var_name]])){
      stop(paste("The column",options$dep_var_name,"doesn't exist. Please set dep_var manually."))
  }}

  #Compose the data set. 
  post_munged <- syberiaReports::model()$munge(data) %>% 
    {.[,!(colnames(.) %in% options$dep_var_name)]}
  
  data[,options$scores] <- syberiaReports::model()$predict(data, options)
  if(options$dep_var_name != FALSE){
    out <- tryCatch(
       left_join(data[,c(options$id_name, options$dep_var_name, options$scores)], post_munged)
      ,error = function(e) {cbind(data[,c(options$id_name, options$dep_var_name, options$scores)], post_munged)}
    )
    colnames(out)[colnames(out) == options$dep_var_name] <- options$dep_var_name
  } else {
    out <- tryCatch(
       left_join(data[,c(options$id_name, options$scores)], post_munged)
      ,error = function(e) {cbind(data[,c(options$id_name, options$scores)], post_munged)}
    )
  }

  #Write
  path <- paste0(syberiaReports::recipe()$save, '/', data_name)
  s3store(out, path)

  #Return the path
  return(path)
}


#' Write out a plot to s3
#' @description. Really just a wrapper around s3_plot, but helps by managing the
#' plot paths for you.
#' @param plot_obj The output from a plot function like ggplot() or plot()
#' @param name Give the plot a name. A cool one. No, cooler.
#' @param opts Optional parameters to pass as a list to s3_plot
#' @return Returns the s3 path to the plot
#' @export
store_plot <- function(plot_obj,name,opts=list()){
  s3_plot(
    paste0(syberiaReports::report()$location$report,'/',name), 
    plot(plot_obj),
    opts
  )
}

#' Add something to your report.
#' @description Rather than rebuilding the report every time, append_report lets
#' you add functions to a report that already exists on s3. Good for testing new 
#' functions, troubleshooting, or ad hoc requests.
#' @param report_path The s3 path to the report.
#' @param func_list List of functions to add, formatted exactly like your
#' report list.
#' @return Technically does not return anything. But report elements are globally
#' assigned, so the new elements will be in memory.
#' @export
append_report <- function(report_path, func_list){

  #Read in report
  syberiaReports::make_report_env(.disable_tests=TRUE)
  .syberiaReport$report <- s3read(report_path)
  .syberiaReport$model <- do.call(s3read, as.list(syberiaReports::report()$location$model))

  #Evaluate each of the reporting functions.
  report_on <- function(element) do.call(element[[1]], element[-1])
  lapply(func_list, report_on)

  #Write out
  s3store(syberiaReports::report(), syberiaReports::report()$location$report)
}


#' Assigns object to a report via a text path. 
#' @description Takes the object, forms the string report$[[object_path]], and
#' then assigns the object to that location in the report. E.g. 
#' add_element(some_plot, 'plots$plot_a') will take the plot object and will 
#' perform `report$plots$plot_a <- some_plot`
#' @param item any. The object you'd like to store. Commonly dataframes, urls to
#' images, etc, but could be anything. 
#' @param location character. The location in the report you'd like the object 
#' to be stored in, as a text string. 
#' @export
add_element <- function(item, location){
  .syberiaReport$temp <- item
  eval(parse(text=paste0('report$',location,' <- temp')) ,envir=.syberiaReport)
}




