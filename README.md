# SNBdata

Functions for downloading data from the Swiss National Bank
(SNB; <https://data.snb.ch/>), the Swiss central bank.  The
package is lightweight and comes with few dependencies;
suggested packages are used only if data is to be
transformed into particular data structures, for instance
into 'zoo' objects. Downloaded data can optionally be
cached, to avoid repeated downloads of the same files.

## Installation

To install the package from a running R session, type:

    install.packages('SNBdata',
                     repos = c('http://enricoschumann.net/R',
                               getOption('repos')))


or clone/build the repository's latest version.
