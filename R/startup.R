#' Create new, blank .syberiaReport environment
#' @description Cleans the slate and creates a new reporting environment. 
#' @param .disable_tests bool. Whether to run tests from syberiaReports.test
#' directory. 
#' @export
make_report_env <- function(.disable_tests = FALSE){
  if (exists(".syberiaReport")){
    detach('.syberiaReport$library')
    detach('.syberiaReport')
    base::rm(list = ls(.syberiaReport), envir = .syberiaReport)
  }
  assign('.syberiaReport', new.env(), .GlobalEnv)
  if(!any(search() %in% ".syberiaReport")){attach(.syberiaReport)}
  assign('report', list(), .syberiaReport)
  
  assign('library', new.env(), .syberiaReport)
  list.files(settings$common$syberiaReports$library, full.name=TRUE) %>% 
    grep("\\.R$", ., value=TRUE) %>% 
    sapply(function(x) source(x, local=.syberiaReport$library))
  if(!any(search() %in% ".syberiaReport$library")){attach(.syberiaReport$library)}
  
  assign('tests', new.env(), .syberiaReport)
  list.files(settings$common$syberiaReports$test, full.name=TRUE) %>% 
    grep("\\.R$", ., value=TRUE) %>%
    sapply(function(x) source(x, local=.syberiaReport$tests))
  
  if(!.disable_tests){
    cat("Running Tests. Hold on to 'ya butts... \n")
    check_coverage()
    perform_tests()
    make_report_env(.disable_tests = TRUE)
  }
}

#' Get the s3 path to a set by using the report_list name.
#' @description Gets a data set from the report_list section you specify. 
#' @param name The name of the data set.
#' @param list Optional param to specify whether you want to use the scored data
#' list or the raw data list. Defaults to scored data.
#' @return The s3link to the data set.
#' @export
get_set <- function(name, list="scored_data"){
  sapply(name, function(x) report()[[list]][[x]][[1]])
}

#' Retrieves object via a text path. 
#' @description Works like add_element(), but retrieves the object at the 
#' specified text path. 
#' @param location character. The location in the report you'd like to retrieve
#' an object from. 
#' @export
get_element <- function(location){
  eval(parse(text=paste0('report$',location)) ,envir=.syberiaReport)
}

#' Combines multiple s3 paths into a single data set
#' @param sets character. A vector of the sets to include. 
#' @param list character. The name of the set list to draw from. 
#' @return A data set containing the rbound data, , with column 'set' to 
#' distinguish the data's origin. 
#' @export
combine_sets <- function(sets, list = "scored_data"){
  report()[[list]][sets] %>% 
    lapply(s3read) %>%
    {mapply(function(x,y){y$set <- x; y}, names(.), ., SIMPLIFY=FALSE)} %>%
    do.call(rbind,.)
}

#' Helper to call the report object from .syberiaReport
#' @return
#' @export
report <- function(){.syberiaReport$report}

#' Helper to call the report recipe from .syberiaReport
#' @return
#' @export
recipe <- function(){.syberiaReport$recipe}

#' Helper to call the model from .syberiaReport
#' @return
#' @export
model <- function(){.syberiaReport$model}

#' Helper to call the library environment from .syberiaReport, with all the
#' reporting functions
#' @return
#' @export
lib <- function(){.syberiaReport$library}

#' Helper to call the test environment from .syberiaReport, with all the
#' reporting functions
#' @return
#' @export
tests <- function(){.syberiaReport$tests}
