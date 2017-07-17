# Objective

To create a simple, Syberia-like package that turns a list of functions into a report object for a particular Syberia model, similar to how Syberia turns a list of stages into a model object. 


# Usage

The Syberia report list has five distinct stages

#### Model

Specify the s3 path to your Syberia model. 

#### Raw Data

A list of s3 paths, with each path corresponding to a single data set that makes up the raw data for your analysis. This will not be altered by the report, but may be called using get_set(list="raw_data")

#### Scored Data

A list of lists, where each list has the s3path to the original data set as the first element, and a list of optional scoring parameters as the second. The parameters are passed to Syberia's model$predict() method. 

#### Report 

This is the main section. A list of executable functions that adds various calculations/elements to your report. Could be functions to calculate AUCs, make histograms, diagnostic plots, etc. Whatever your report needs. 



# Example. 
```
list(
   model = 's3path/to/your/model' #Let's say a glmnet model
  
  ,raw_data = list(
     training = 's3path/original/training/data'
   )
  
  ,scored_data = list(
      training = list('s3path/original/training/data', list(on_train=TRUE))
     ,training_min = list('s3path/original/training/data', list(on_train=TRUE,s='lambda.min'))
     ,training_custom = list('s3path/original/training/data', list(on_train=TRUE,s=1.579416e-03))
   )
  
  ,report = list(
      "AUC Calc " = list(get_aucs, c('training','training_min','training_custom'))
     ,"Coefs" = list(get_coef_glmnet, s=c('lambda.1se','lambda.min',1.579416e-03))
     ,"Metadata" = list(
         add_metadata
        ,'path/to/my/metadata'
        ,'variable_name'
        ,'description'
        ,c('variable_type')
      )
     ,"Deciles" = list(make_decile_tables, c('training','training_min','training_custom'))
   ) 
  
  ,save = 'path/to/my/saved/report'
)
```
