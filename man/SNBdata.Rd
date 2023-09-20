\name{SNBdata}
\alias{SNBdata-package}
\alias{SNBdata}
\alias{fetch_data}
\alias{fetch_last_update}
\alias{fetch_info}
\concept{SNB}
\title{
  Download Data from the Swiss National Bank (SNB)
}
\description{

  Download datasets from the Swiss National Bank (SNB)
  and convert to data-frames.

}
\usage{
fetch_data(id,
           type = "table", dest.dir = NULL,
           return.class = NULL, verbose = TRUE,
           language = "en",
           name.sep = " :: ",
           method,
           na.drop = TRUE,
           timeseries = FALSE, ...)

fetch_last_update(id,
                  type = "table", dest.dir = NULL,
                  verbose = TRUE, language = "en", ...)

fetch_info(id,
           type = "table", dest.dir = NULL,
           verbose = TRUE,
           language = "en",
           name.sep = " :: ",
           method, ...)
}
\arguments{
  \item{dest.dir}{%
    file path: where to store the files?
  }
  \item{id}{%
    string: identifier of the table/dataset
  }
  \item{type}{%
    string: \sQuote{\code{table}} or \sQuote{\code{dataset}}
  }
  \item{method}{%
   see \code{\link{download.file}}
  }
  \item{verbose}{%
    logical: print messages, e.g. about download progress?
  }
  \item{return.class}{%
    \code{NULL} or character: not yet implemented (but in
    future versions, \code{\link[zoo]{zoo}} will be supported)
  }
  \item{language}{%
    string: \code{en}, \code{fr}, or \code{de}
  }
  \item{\dots}{%
    passed on to \code{\link{read.table}}
  }
  \item{name.sep}{%
    string used when pasting description hierarchies
  }
  \item{timeseries}{%
    logical. If \code{TRUE}, data are transformed into
    time-series.
  }
  \item{na.drop}{%
    logical. If \code{TRUE}, rows with no finite values at all
    are dropped.  Only used if \code{timeseries} is
    \code{TRUE}.
  }
}
\details{

  The Swiss National Bank (SNB) provides data as either
  so-called tables or datasets.  \code{fetch_table} can handle
  both, but the \code{type} argument must be specified.

  \code{fetch_table} downloads data, which typically are in
  CSV format, from the SNBs's website and stores them, with a
  date prefix, in directory \code{dest.dir}.  If the latter is
  \code{NULL}, a temporary directory is used (through
  \code{\link{tempdir}}); but much better is to use a
  more-persistant storage location.  If a file with today's
  date exists in \code{dest.dir}, that file is read, and
  nothing is downloaded.

  When argument \code{timeseries} is \code{TRUE},
  \code{fetch_table} will rearrange the data into time-series.
  This requires columns named \dQuote{\code{Date}} and
  \dQuote{\code{Value}} in the particular dataset/table.

}
\value{
  typically a \code{\link{data.frame}},
  potentially with attributes additional attributes:
  \describe{
    \item{\code{dimensions}}{%
      information about identifiers used by the SNB
    }
    \item{\code{columns}}{%
      if \code{timeseries} is \code{TRUE}, the names of
      the columns used for grouping the rows
    }
  }
}
\references{
  \url{https://data.snb.ch/en/}

  for \code{help} on the \acronym{API},
  see\url{https://data.snb.ch/en/help#data_api}

}
\author{
  Enrico Schumann
}
%% \seealso{
%%   package
%% }
\examples{
\donttest{
## (Internet connection required)

### set directory for storing the files.  This is
### only an example: Much better is to use a permanent
### storage-location, such as '~/Downloads/SNBdata'
data.dir <- tempdir()

rates <- fetch_data("rendoblim",
                    type = "table",
                    dest.dir = data.dir,
                    language = "en")
##      Date D0 Value
## 1 1988-01 1J 2.887
## 2 1988-01 2J 3.218
## 3 1988-01 3J 3.393
## 4 1988-01 4J 3.554
## 5 1988-01 5J 3.695
## ....

### transform into time-series
rates <- fetch_data("rendoblim",
                    type = "table",
                    dest.dir = data.dir,
                    language = "en",
                    timeseries = TRUE)
##      Date    1J    2J    3J ....
## 1 1988-01 2.887 3.218 3.393 ....
## 2 1988-02 2.638 2.990 3.271 ....
## 3 1988-03 2.641 3.263 3.573 ....
## ....

attr(rates, "dimensions")
##                                                           1J
##  "Overview :: CHF Swiss Confederation bond issues :: 1 year"
##                                                           2J
## "Overview :: CHF Swiss Confederation bond issues :: 2 years"
## ....

stock.markets <- fetch_data("capchstocki",
                            type = "table",
                            dest.dir = data.dir,
                            timeseries = TRUE)
stock.markets[, "GDR"]  ## total return index
}}
