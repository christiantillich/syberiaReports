% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\name{score_data}
\alias{score_data}
\title{Score a data set.}
\usage{
score_data(data_name)
}
\arguments{
\item{data_name}{The name of the set you specify in the report_list}
}
\value{
report$scored_data
GLMNet handles 's' weird. You need to assign predict_method to model$input.
You can't just pass it as an optional parameter to predict. So, if it exists
we gotta handle it special.
}
\description{
The main function for scoring data sets
}

