

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
build_report <- function(report_path, .disable_tests=TRUE){
  #Messages
  library_error <- paste(
    "Please specify a location for all syberiaReports functions via either the"
    ,"library attribute of your report list, or"
    ,"options('syberiaReports.library')"
  )
  test_error <- paste(
    "Please specify a location for all syberiaReports functions via either the"
    ,"test attribute of your report list, or"
    ,"options('syberiaReports.test')"
  )
  
  #Create the report environment and object and append the basics
  make_report_env()
  .ReportEnv$report_list <- source(report_path)$value
  .ReportEnv$model <- s3read(report_list()$model)

  #Import report function libraries, import the testing functions, and then run.
  c(report_list()$library, options('syberiaReports.library')) %>% 
    unlist %>% .[[1]] %>%
    {if(is.null(.)){stop(library_error)}else{.}} %>%
    list.files(full.names=TRUE) %>% 
    sapply(function(x) source(x, local = report_library()))
  
  c(report_list()$tests, options('syberiaReports.tests')) %>% 
    unlist %>% .[[1]] %>%
    {if(is.null(.)){stop(test_error)}else{.}} %>%
    list.files(full.names=TRUE) %>% 
    sapply(function(x) source(x, local = report_tests()))
  
  if(.disable_tests == TRUE){
    cat("Checks disabled. Skipping. But make sure this is what you want to do.\n")
  }else{
    cat("Running through report tests. This will only take a moment.")
    report_tests() %>% as.list %>% lapply(function(x) x())
    
    if(length(setdiff(ls(report_library()),ls(report_tests()))) > 0){
      warning("The following reporting functions don't have tests:", immediate = TRUE)
      setdiff(ls(report_library()),ls(report_tests())) %>% cat
    }
  }
  
  
  #Build out the core report properties
  cat('Initializing Report\n')
  add_element(report_list()$save, 'location$report')
  add_element(report_list()$model, 'location$model')
  add_element(model()$output$model, 'model')
  add_element(report_list()$raw_data, 'raw_data')
  add_element(report_library(), '.env$library')
  add_element(report_tests(), '.env$tests')
  add_element(report_list(), '.env$list')

  #Score all the listed data sets.
  cat('Scoring the data sets\n')
  sapply(names(report_list()$scored_data), score_data) %>% add_element('scored_data')

  #Evaluate each of the reporting functions.
  cat('Running reporting functions\n')
  report_on <- function(element){
    cat(names(element))
    do.call(element[[1]], element[-1])
  }
  dummy <- report_list()$report %>% lapply(report_on)

  #Save the report
  cat(paste('Writing out report to', report_list()$save,'\n'))
  s3store(report(), report_list()$save)
}


#' Score a data set.
#' @description The main function for scoring data sets
#' @param data_name The name of the set you specify in the report_list
#' @return report$scored_data
score_data <- function(data_name){

  #Set up the default options
  options <- unlist(report_list()$scored_data[[data_name]][-1], recursive=F)
  if(is.null(options$filters)){options$filters <- list('TRUE')}
  if(is.null(options$id_name)){
    options$id_name <- model()$input[c('id_type','id_var')] %>% 
      unlist %>% unique %>% .[1]
  }
  if(!is.null(options$predict_method)){
    model()$input$predict_method <- options$predict_method
  }
  if(is.null(options$dep_var_name)){options$dep_var_name <- 'dep_var'}
  #' GLMNet handles 's' weird. You need to assign predict_method to model$input.
  #' You can't just pass it as an optional parameter to predict. So, if it exists
  #' we gotta handle it special.
  
  #Read in the data
  data <- s3read(report_list()$scored_data[[data_name]][[1]]) %>%
    {do.call(function(x, ...) filter_(., ...), options$filters)}
  if(is.null(data[[options$id_name]])){
    stop(paste("The column",options$id_name,"doesn't exist. Please set id_name manually."))
  }

  #Compose the data set. 
  post_munged <- model()$munge(data) %>% {.[,!(colnames(.) %in% options$dep_var_name)]}
  data$score <- model()$predict(data, options)
  out <- left_join(data[,c(options$id_name, options$dep_var_name,'score')], post_munged)
  colnames(out)[colnames(out) == options$dep_var_name] <- 'dep_var'

  #Write
  path <- paste0(report_list()$save, '/', data_name)
  s3store(out, path)

  #Return the path
  return(path)
}


#' Get the s3 link to a set by using the report_list name.
#' @description Gets a data set from the report_list section you specify. 
#' @param name The name of the data set.
#' @param list Optional param to specify whether you want to use the scored data
#' list or the raw data list. Defaults to scored data.
#' @return The s3link to the data set.
#' @export
get_set <- function(name, list="scored_data"){
  sapply(name, function(x) report()[[list]][[x]][[1]])
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
  s3_plot(paste0(report()$location$report,'/',name), plot(plot_obj),opts)
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
  make_report_env()
  .ReportEnv$report <- s3read(report_path)
  .ReportEnv$model <- s3read(report()$location$model)

  #Evaluate each of the reporting functions.
  report_on <- function(element) do.call(element[[1]], element[-1])
  lapply(func_list, report_on)

  #Write out
  s3store(report(), report()$location$report)
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
  .ReportEnv$temp <- item
  eval(parse(text=paste0('report$',location,' <- temp')) ,envir=.ReportEnv)
}

#' Retrieves object via a text path. 
#' @description Works like add_element(), but retrieves the object at the 
#' specified text path. 
#' @param location character. The location in the report you'd like to retrieve
#' an object from. 
#' @export
get_element <- function(location){
  eval(parse(text=paste0('report$',location)) ,envir=.ReportEnv)
}

#' Create new, blank .ReportEnv
#' @description Cleans the slate and creates a new reporting environment. 
#' @export
make_report_env <- function(){
  assign('.ReportEnv', new.env(), .GlobalEnv)
  attach(.ReportEnv)
  assign('report', list(), .ReportEnv)
  assign('report_library', new.env(), .ReportEnv)
  assign('report_tests', new.env(), .ReportEnv)
}


#Useful internal helper functions, don't document or export. 
report <- function(){.ReportEnv$report}
report_list <- function(){.ReportEnv$report_list}
model <- function(){.ReportEnv$model}
report_library <- function(){.ReportEnv$report_library}
report_tests <- function(){.ReportEnv$report_tests}

