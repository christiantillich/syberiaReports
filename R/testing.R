check_coverage <- function(){
  .ReportEnv %>% 
    {setdiff(names(.$report_library), names(.$report_tests))} %>%
    paste(collapse = ", ") %>%
    cat("Tests missing for the following functions: ", ., "\n")
}


perform_tests <- function(){
  error_func <- function(e){ 
    as.character(e) %>% 
      strsplit('\n') %>% 
      unlist %>% 
      {c(.[[1]], grep('\\*',., value=TRUE))} %>%
      paste(collapse='\n\t') %>% 
      warning(.,'\n')
  }
  errors <- .ReportEnv$report_tests %>% 
    sapply(function(x) tryCatch(x(), error = error_func))
}