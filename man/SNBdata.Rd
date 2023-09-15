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
  and convert to data frames.

}
\usage{
fetch_data(id, type = "table",
           dest.dir = NULL,
           return.class = NULL, verbose = TRUE,
           method,
           language = "en",
           name.sep = " :: ",
           na.drop = TRUE, ...)
fetch_last_update(id,
                  dest.dir = NULL,
                  verbose = TRUE,
                  language = "en", ...)
fetch_info(id, dest.dir = NULL,
           return.class = NULL, verbose = TRUE,
           language = "en", method, ...)
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
  \item{return.class}{\code{NULL} or character:
      not yet implemented (but in future versions,
    \code{\link[zoo]{zoo}} will be supported)
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
  \item{na.drop}{%
    logical. If \code{TRUE}, rows with no finite values at all
    are dropped.
  }
}
\details{

  The \acronym{SNB} provides data as tables and datasets.

}
\value{
  typically a \code{\link{data.frame}}.

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
fetch_table("rendoblim",
            dest.dir = "~/Downloads/SNBdata/",
            language = "de")
}}
