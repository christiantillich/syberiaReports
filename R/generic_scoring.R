

#' Calculate auc for a scored data set.
#' @description Specify one of the scored data sets. get_auc will calculate the
#' AUC for that set and append it to a vector of auc values.
#' @param data_name The name of the data set, defined in the report list.
#' @param fold An optional param to calculate AUC split by the named column. Useful
#' for calculating AUC on cross-validation sets.
#' @return report$auc$`set_name`
#' @export
get_auc <- function(data_name, fold=NA){
  data <- s3read(get_set(data_name))
  calc <- function(x){
    pred <- ROCR::prediction(x[['score']], x[['dep_var']])
    ROCR::performance(pred, "auc")@y.values[[1]]
  }

  report$auc$val[names(get_set(data_name))] <<- if(!is.na(fold)){
    sapply(split(data, data[[fold]]), calc)
  }else{
    calc(data)
  }
}

#' Calculate AUCs for all scored sets.
#' @description Just sapply get_auc across every scored_set.
#' @param data_paths A vector of the scored set names
#' @param folds An optional vector of the names of fold columns. Defaults to NA,
#' and will recycle. #'
#' @return report$auc
#' @export
get_aucs <- function(data_names, folds=NA){
  mapply(get_auc, data_names, folds)
}


#' Adds decile info to report.
#' @description Create a dataframe with set name, x column decile, and expected
#' and actual scores. Also appends a plot visualizing the differences between
#' expected and actual values.
#' @param scored_sets The names of the scored data sets.
#' @param xcol Which column to rank by decile. Defaults to score, but could be
#' used for other rank-orderings.
#' @return report$performance$`colname_decile_table` and
#' report$performance$`colname_decile_plot`
#' @export
make_decile_tables <- function(scored_sets, xcol = 'score'){

  #Function to create single table
  decile_table <- function(scored_set, xcol){

    data <- s3read(get_set(scored_set))

    if(length(unique(data[[xcol]])) <= 10){
      lvls = unique(data[[xcol]])
    }else{
      lvls = data[xcol] %>% {c(unique(.[. < 0 | . == 999]), quantile(.[. > 0],0:10/10))}
    }

    data %>%
      mutate_(decile = xcol, .dots=xcol) %>%
      mutate(decile = cut(decile, unique(sort(lvls)), labels=F, include.lowest=T)) %>%
      group_by(decile) %>%
      summarise(
         count=n()
        ,set=scored_set
        ,expected = mean(score)
        ,actual = mean(dep_var)
      ) %>%
      select(set, decile, count, expected, actual)
  }

  report$performance[[paste0(xcol,'_decile_table')]] <<- scored_sets %>%
    lapply(function(y) decile_table(y, xcol)) %>%
    do.call(rbind, .)

  report$performance[paste0(xcol,'_decile_plot')] <<-
    report$performance[[paste0(xcol,'_decile_table')]] %>%
    {
      qplot(decile, expected, data=., geom="line", color="black") +
        geom_point(aes(y=actual, color="darkred")) +
        geom_col(aes(y = count/sum(count), fill="navyblue"),alpha=I(0.20)) +
        facet_wrap(~set) +
        scale_fill_manual(
          name = "test"
          ,values=c('navyblue'='navyblue')
          ,labels = c('Volume')
        ) +
        scale_color_manual(
          name = "test"
          ,values=c('black'='black','darkred'='darkred')
          ,labels = c('Expected','Actual')
        ) +
        ggtitle('Validation Plot')
    } %>% store_plot(paste0(xcol,'_decile'))

}



