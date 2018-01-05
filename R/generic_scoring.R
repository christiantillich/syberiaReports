

#' Calculate auc for a scored data set.
#' @description Specify one of the scored data sets. get_auc will calculate the
#' AUC for that set and append it to a vector of auc values.
#' @param data_name The name of the data set, defined in the report list.
#' @param fold An optional param to calculate AUC split by the named column. Useful
#' for calculating AUC on cross-validation sets.
#' @return Returns the auc for a given data set. 
get_auc <- function(data_name, fold=NA){
  data <- s3read(get_set(data_name))
  calc <- function(x){
    pred <- ROCR::prediction(x[['score']], x[['dep_var']])
    ROCR::performance(pred, "auc")@y.values[[1]]
  }

  #TODO: Not sure if this actually works anymore with data that has folds. 
  out <- if(!is.na(fold)){
    sapply(split(data, data[[fold]]), calc)
  }else{
    calc(data)
  }
  out
}

#' Calculate AUCs for all scored sets.
#' @description Just sapply get_auc across every scored_set.
#' @param data_paths A vector of the scored set names
#' @param folds An optional vector of the names of fold columns. Defaults to NA,
#' and will recycle. #'
#' @return AUC measures for each data set. 
#' @export
get_aucs <- function(data_names, folds=NA, location="auc$val"){
  mapply(get_auc, data_names, folds) %>% add_element(location)
}


#' Adds decile info to report.
#' @description Create a dataframe with set name, x column decile, and expected
#' and actual scores. Also appends a plot visualizing the differences between
#' expected and actual values.
#' @param scored_sets The names of the scored data sets.
#' @param xcol Which column to use as an expected score, ranked by decile. 
#' Defaults to 'score', but could be used for other rank-orderings.
#' @param ycol Which column to use as the actual measure. Defaults to 'dep_var',
#' but could be used on custom measures. 
#' @return A plot to show expected vs. actual by score, and the tables used to
#' create that plot. 
#' @export
make_decile_tables <- function(
    scored_sets
  , xcol = 'score'
  , ycol='dep_var'
  , location = 'performance'
){

  #Function to create single table
  decile_table <- function(scored_set, xcol, ycol){

    data <- s3read(get_set(scored_set))

    if(length(unique(data[[xcol]])) <= 10){
      lvls = unique(data[[xcol]])
    }else{
      lvls = data[xcol] %>% {c(unique(.[. < 0 | . == 999]), quantile(.[. > 0],0:10/10))}
    }

    data %>%
      mutate_(decile = xcol, .dots=xcol) %>%
      mutate_(target = ycol, .dots=ycol) %>%
      mutate(decile = cut(decile, unique(sort(lvls)), labels=F, include.lowest=T)) %>%
      group_by(expected_decile = decile) %>%
      summarise(
         count=n()
        ,set=scored_set
        ,expected = mean(score)
        ,actual = mean(target)
      ) %>%
      as.data.frame
  }

  scored_sets %>%
    lapply(function(y) decile_table(y, xcol, ycol)) %>%
    do.call(rbind, .) %>%
    add_element(paste0(location,'$decile_table$',xcol,'_v_',ycol))
  
  
  get_element(paste0(location,'$decile_table$',xcol,'_v_',ycol)) %>%
    {
      qplot(expected_decile, expected, data=., geom="line", color="black") +
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
    } %>% store_plot(paste0(xcol,'_v_',ycol)) %>%
    add_element(paste0(location,'$decile_plot$',xcol,'_v_',ycol))
}



