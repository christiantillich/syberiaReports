

#' Get the coefficients from a glmnet model.
#' @description Turns the ugly matrix presentation of model coefficients into
#' a nice, neat data frame.
#' @param s The lambda value for the coefficients.
#' @return report$coef$`lambda_val`
#' @export
get_coef_glmnet <- function(s){
  single_coef <- function(x){
    report$coef[as.character(x)] <<-
      tryCatch(
         coef(model$output$model, as.numeric(x))
        ,error = function(y) coef(model$output$model, x)
      ) %>%
      as.matrix %>%
      .[. != 0,] %>%
      data.frame(var = gsub('`','',names(.)), coef = .) %>%
      (function(x){rownames(x)<-NULL; x})(.)  %>%
      list
  }

  sapply(s, single_coef, simplify=F)
}




#' Add metadata
#' @description Add a file containing metadata to coefficients. Report$coef
#' must already be populated.
#' @param s3path S3 path to the metadata file.
#' @param var_col The name of the column that specifies the variable. For joining.
#' @param desc_col The name of the column that specifies the description.
#' @param additions Any additional column names from the metadata file you wish to add.
#' @return report$coef
#' @export
add_metadata_glmnet <- function(s3path, var_col, desc_col, additions=NULL){

  #Stop if coefficients haven't been attached.
  stopifnot(!is.null(report$coef))

  #Pull down metadata
  df.meta <- s3read(s3path)

  #Join the metadata to all coefficient sets
  add_desc <- function(coef_name){
    report$coef[[coef_name]] <<- df.meta[,c(var_col,desc_col,additions)] %>%
      merge(report$coef[[coef_name]], ., all.x=T, by.x="var",by.y = var_col)
  }
  names(report$coef) %>% sapply(add_desc)
}
