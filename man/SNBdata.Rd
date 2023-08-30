\name{SNBdata}
\alias{SNBdata-package}
\alias{SNBdata}
\alias{fetch_table}
\alias{fetch_table_date}
\alias{fetch_table_info}
\concept{SNB}
\title{
  Download Data from the Swiss National Bank (SNB)
}
\description{

  Download datasets from the Swiss National Bank (SNB)
  and convert to data frames.

}
\usage{
fetch_table(table = "rendoblid", dest.dir = NULL,
            return.class = NULL, verbose = TRUE,
            method, ...)
fetch_table_date(table = "rendoblid", dest.dir = NULL,
                 verbose = TRUE,
                 language = "en", ...)
fetch_table_info(table = "rendoblid", dest.dir = NULL,
                 return.class = NULL, verbose = TRUE,
                 language = "en", ...)
}
\arguments{
  \item{dest.dir}{%
    file path: where to store the files?
  }
  \item{table}{%
    the name of the table
  }
  \item{method}{%
      see \code{\link{download.file}}
  }
  \item{verbose}{
    logical: print messages about download progress?
  }
  \item{return.class}{\code{NULL} or character:
      not yet implemented (but in future version,, only
    \code{\link[zoo]{zoo}} will be supported)
  }
  \item{language}{
    string: \code{en}, \code{fr}, or \code{de}
  }
  \item{\dots}{
    passed on to \code{\link{read.table}}
  }
}
\details{

  The \acronym{SNB} provides data as tables and datasets
}
\value{
  typically a \code{\link{data.frame}}.


}
\references{
  \url{https://data.snb.ch/en/help#data_api}
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
