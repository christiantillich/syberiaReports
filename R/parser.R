

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
build_report <- function(report_path){

  #Create the report environment and object and append the basics
  make_report_env()
  .ReportEnv$report_list <- source(report_path)$value
  .ReportEnv$model <- s3read(report_list()$model)

  #Build out the core report properties
  cat('Initializing Report\n')
  add_element(report_list()$save, 'location$report')
  add_element(report_list()$model, 'location$model')
  add_element(model()$output$model, 'model')
  add_element(report_list()$raw_data, 'raw_data')

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

  #Read the data
  options <- unlist(report_list()$scored_data[[data_name]][-1], recursive=F)
  if(is.null(options$filters)){options$filters <- list('TRUE')}
  data <- s3read(report_list()$scored_data[[data_name]][[1]]) %>%
    {do.call(function(x, ...) filter_(., ...), options$filters)}
  
  #' GLMNet handles 's' weird. You need to assign predict_method to model$input.
  #' You can't just pass it as an optional parameter to predict. So, if it exists
  #' we gotta handle it special.
  if(!is.null(options$predict_method)){
    model()$input$predict_method <- options$predict_method
  }

  #Compose the data set. 
  id_name <- model()$input[c('id_type','id_var')] %>% unlist %>% unique %>% .[1]
  post_munged <- model()$munge(data) %>% {.[,!(colnames(.) %in% 'dep_var')]}
  data$score <- model()$predict(data, options)
  #' This is a bit obtuse, but some model munging produces a trivial, NA-filled
  #' dep_var, so this just kicks it out

  out <- left_join(data[,c(id_name, 'dep_var','score')], post_munged)
  
  # #Create the score from the list options
  # data$score <- model()$predict(data, options)
  # 
  # #Join the data to the post-munged variable set. 
  # id_name <- model()$input[c('id_type','id_var')] %>% unlist %>% unique %>% .[1]
  # out <- data %>%
  #   .[,c(id_name,'score','dep_var')] %>%
  #   merge(model()$munge(data) %>% {.[,!(colnames(.) %in% 'dep_var')]},by=id_name)
  #'  The problem's here, consider just rewriting it all to make the munged data
  #'  explicit instead of doing this as one big ugly pipe. 

  
  #Write
  path <- paste0(report_list()$save, '/', data_name)
  s3store(out, path)

  #Return the path
  return(path)
}


#' Get the s3 link to a set by using the report_list name.
#' @description
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
#' you add functions to an existing report. Good for testing new functions,
#' troubleshooting, or ad hoc requests.
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


add_element <- function(item, location){
  .ReportEnv$temp <- item
  eval(parse(text=paste0('report$',location,' <- temp')) ,envir=.ReportEnv)
}

get_element <- function(location){
  eval(parse(text=paste0('report$',location)) ,envir=.ReportEnv)
}


make_report_env <- function(){
  assign('.ReportEnv', new.env(), .GlobalEnv)
  attach(.ReportEnv)
  assign('report', list(), .ReportEnv)
}
report <- function(){.ReportEnv$report}
report_list <- function(){.ReportEnv$report_list}
model <- function(){.ReportEnv$model}