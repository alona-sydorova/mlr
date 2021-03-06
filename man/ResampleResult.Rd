% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResampleResult.R
\name{ResampleResult}
\alias{ResampleResult}
\title{ResampleResult object.}
\description{
A resample result is created by \code{\link{resample}} and
contains the following object members:
\describe{
\item{task.id [\code{character(1)}]:}{
  Name of the Task.
}
\item{learner.id [\code{character(1)}]:}{
  Name of the Learner.
}
\item{measures.test [\code{data.frame}]:}{
  Gives you access to performance measurements
  on the individual test sets. Rows correspond to sets in resampling iterations,
  columns to performance measures.
}
\item{measures.train [\code{data.frame}]:}{
  Gives you access to performance measurements
  on the individual training sets. Rows correspond to sets in resampling iterations,
  columns to performance measures. Usually not available, only if specifically requested,
  see general description above.
}
\item{aggr [\code{numeric}]:}{
  Named vector of aggregated performance values. Names are coded like
  this <measure>.<aggregation>.
}
\item{err.msgs [\code{data.frame}]:}{
  Number of rows equals resampling iterations
  and columns are: \dQuote{iter}, \dQuote{train}, \dQuote{predict}.
  Stores error messages generated during train or predict, if these were caught
  via \code{\link{configureMlr}}.
}
\item{err.dumps [\code{list of list of dump.frames}]:}{
  List with length equal to number of resampling iterations. Contains lists
  of \code{dump.frames} objects that can be fed to \code{debugger()} to inspect
  error dumps generated on learner errors. One iteration can generate more than
  one error dump depending on which of training, prediction on training set,
  or prediction on test set, operations fail. Therefore the lists have named
  slots \code{$train}, \code{$predict.train}, or \code{$predict.test} if relevant.
  The error dumps are only saved when option \code{on.error.dump} is \code{TRUE}.
}
\item{pred [\code{\link{ResamplePrediction}}]:}{
  Container for all predictions during resampling.
}
\item{models [list of \code{\link{WrappedModel}}]:}{
  List of fitted models or \code{NULL}.
}
\item{extract [\code{list}]:}{
  List of extracted parts from fitted models or \code{NULL}.
}
\item{runtime [\code{numeric(1)}]:}{
  Time in seconds it took to execute the resampling.
}
}
The print method of this object gives a short overview, including
task and learner ids, aggregated measures and runtime for the resampling.
}
\seealso{
Other resample: \code{\link{ResamplePrediction}},
  \code{\link{addRRMeasure}},
  \code{\link{getRRPredictionList}},
  \code{\link{getRRPredictions}},
  \code{\link{getRRTaskDescription}},
  \code{\link{getRRTaskDesc}},
  \code{\link{makeResampleDesc}},
  \code{\link{makeResampleInstance}},
  \code{\link{resample}}

Other debug: \code{\link{FailureModel}},
  \code{\link{getPredictionDump}}, \code{\link{getRRDump}}
}
