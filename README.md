# SNBdata

Functions for downloading data from the Swiss National Bank
(SNB; <https://data.snb.ch>), the Swiss central bank.  The
package is lightweight and comes with few dependencies;
suggested packages are used only if data is to be
transformed into particular data structures, for instance
into 'zoo' objects. Downloaded data can optionally be
cached, to avoid repeated downloads of the same files.

## Installation

To install the development version of the package from a
running R session, type:

    install.packages('SNBdata',
                     repos = c('http://enricoschumann.net/R',
                               getOption('repos')))


or clone/build the git repository's latest version.


## Examples

(Examples require an internet connection.)

Start by setting the directory for storing the files.
This is only an example: Much better is to use a permanent
storage-location, such as '~/Downloads/SNBdata'

    data.dir <- tempdir()

Now fetch data:

    rates <- fetch_data("rendoblim",
                        type = "table",
                        dest.dir = data.dir,
                        language = "en")

Have data transformed into time-series:

    rates <- fetch_data("rendoblim",
                        type = "table",
                        dest.dir = data.dir,
                        language = "en",
                        time.series = TRUE)

Information about identifiers/codes:

    if (!is.null(rates))  ## check: if download failed, results
                          ##        are NULL
        attr(rates, "info")

Another example:

    stock.markets <- fetch_data("capchstocki",
                                type = "table",
                                dest.dir = data.dir,
                                time.series = TRUE)
    ## e.g.: stock.markets[, "GDR"]  ## total return index
