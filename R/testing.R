#' Check that all reporting functions have a non-trivial test. 
#'
#' @return NULL. Prints a message to the screen when functions are missing. 
#' @export
check_coverage <- function(){
  
  #' I have to do it by file names, not function names, because the test 
  #' functions won't be able to hold the same name as the function they're
  #' testing. 
  functions <- settings$common$syberiaReports$library %>% 
    list.files(pattern = "\\.R")
  
  tests <- settings$common$syberiaReports$tests %>% 
    list.files(pattern = "\\.R", full.name=TRUE) %>% 
    file.info %>% 
    .[.$size > 0,] %>%
    rownames %>%
    gsub('.+/','',.)
  
  setdiff(functions, tests) %>%
    paste(collapse = ", ") %>%
    {
      if(length(.) > 0) cat("Tests missing for the following functions: ", ., "\n")
    }
    
}


#' Executes all tests in the test library. 
#' @return NULL. Will print a series of warnings if functions fail tests. 
#' @export
perform_tests <- function(){
  error_func <- function(e){ 
    as.character(e) %>% 
      strsplit('\n') %>% 
      unlist %>% 
      {c(.[[1]], grep('\\*',., value=TRUE))} %>%
      paste(collapse='\n\t') %>% 
      warning(.,'\n')
  }
  errors <- syberiaReports::tests() %>% 
    sapply(function(x) {tryCatch(x(), error = error_func)})
}