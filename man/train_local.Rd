% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local.R
\name{train_local}
\alias{train_local}
\title{Train a Model Locally}
\usage{
train_local(application = getwd(), config = "default", ...)
}
\arguments{
\item{application}{The path to a TensorFlow application.}

\item{config}{The name of the configuration to be used.}

\item{...}{Optional arguments. TODO: describe in more detail.}
}
\description{
Train a model locally, using the \code{gcloud} command line
utility. This can be used as a testing bed for TensorFlow
applications which you want to later run on Google Cloud,
submitted using \code{\link[=train_cloud]{train_cloud()}}.
}
