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
\description{%

  Download data (\sQuote{tables} and \sQuote{datasets}) from the
  Swiss National Bank (\acronym{SNB}) and convert to data-frames.

}
\usage{%
fetch_data(id,
           type = "table", dest.dir = NULL,
           return.class = NULL, verbose = TRUE,
           language = "en",
           name.sep = " :: ",
           method,
           na.drop = TRUE,
           time.series = FALSE, ...)

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
    file path: where to store the downloaded files? See Details.
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
    string: \code{en}, \code{fr} or \code{de}
  }
  \item{\dots}{%
    passed on to \code{\link{read.table}}
  }
  \item{name.sep}{%
    string used when pasting description hierarchies
  }
  \item{time.series}{%
    logical. If \code{TRUE}, data are transformed into
    time-series.
  }
  \item{na.drop}{%
    logical. If \code{TRUE}, rows with no finite values at all
    are dropped.  Only used if \code{time.series} is
    \code{TRUE}.
  }
}
\details{%

  The Swiss National Bank (\acronym{SNB}) provides data as either
  so-called tables or datasets.  \code{fetch_table} can handle
  both, but the \code{type} argument must be specified.

  \code{fetch_table} downloads data, which typically are in
  \acronym{csv} format, from the \acronym{SNB}'s website and
  stores them, with a date prefix, in directory \code{dest.dir}.
  If the latter is \code{NULL}, a temporary directory is used
  (through \code{\link{tempdir}}); but much better is to use a
  more-persistent storage location.  If a file with today's date
  exists in \code{dest.dir}, that file is read, and nothing is
  downloaded.

  For downloading, function \code{\link{download.file}} is used.
  See \code{\link{download.file}} for options; in particular, see
  the hints about \code{timeout} when downloading large files.

  When argument \code{time.series} is \code{TRUE},
  \code{fetch_table} will rearrange the data into time-series.
  This requires columns named \dQuote{\code{Date}} and
  \dQuote{\code{Value}} in the particular dataset/table.  If the
  \dQuote{\code{Date}} column has a \code{YYYY-MM-DD} pattern,
  it will will be transformed with \code{\link{as.Date}}.

}
\value{%

  typically a \code{\link{data.frame}},
  potentially with attributes additional attributes:
  \describe{
    \item{\code{info}}{%
      information about identifiers used by the \acronym{SNB}
    }
    \item{\code{columns}}{%
      if \code{time.series} is \code{TRUE}, the names of the
      columns used for grouping the rows
    }
  }
  Should the download fail, the function returns
  \code{\link{NULL}} invisibly.
}
\references{
  \url{https://data.snb.ch/en}

  for \code{help} on the \acronym{API},
  see\url{https://data.snb.ch/en/help#data_api}

}
\author{
  Enrico Schumann
}
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

### ==> have data transformed into time-series
rates <- fetch_data("rendoblim",
                    type = "table",
                    dest.dir = data.dir,
                    language = "en",
                    time.series = TRUE)

if (!is.null(rates))  ## check: if download failed, results
                      ##        are NULL
    attr(rates, "info")


stock.markets <- fetch_data("capchstocki",
                            type = "table",
                            dest.dir = data.dir,
                            time.series = TRUE)
## e.g.: stock.markets[, "GDR"]  ## total return index
}}
