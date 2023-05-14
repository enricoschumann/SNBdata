
## https://data.snb.ch/en/help#data_api
## https://data.snb.ch/de/topics/snb/cube/snbbipo

## https://data.snb.ch/en/topics/ziredev/cube/rendoblid
## https://data.snb.ch/api/cube/rendoblid/dimensions/en

fetch_table_date <- function(table = "rendoblid",
                             dest.dir = NULL,
                             verbose = TRUE,
                             language = "en", ...) {


    if (!is.null(dest.dir))
        message("currently not supported")

    site <- paste0("https://data.snb.ch/api/cube/", table, "/lastUpdate")
    con <- url(site)
    ans <- try(readLines(con, warn = FALSE), silent = TRUE)
    try(close(con), silent = TRUE)
    em <- geterrmessage()
    jsonlite::fromJSON(ans)
}

fetch_table_info <- function(table = "rendoblid",
                             dest.dir = NULL,
                             return.class = NULL,
                             verbose = TRUE,
                             language = "en", ...) {

    if (!is.null(dest.dir))
        message("currently not supported")

    site <- paste0("https://data.snb.ch/api/cube/", table, "/dimensions/", language)

    con <- url(site)
    dats <- try(readLines(con, warn = FALSE), silent = TRUE)
    close(con)
    em <- geterrmessage()
    jsonlite::fromJSON(dats, FALSE)
}

fetch_table <- function(table = "rendoblid",
                        dest.dir = NULL,
                        return.class = NULL,
                        verbose = TRUE,
                        method, ...) {

    site <- paste0("https://data.snb.ch/api/cube/",
                   table, "/data/csv/en")

    if (!is.null(dest.dir)) {
        filename <- paste0(format(Sys.Date(), "%Y%m%d"),
                           "__", table,
                           ".csv")
        filename <- file.path(dest.dir, filename)
        if (!file.exists(filename)) {
            if (verbose)
                message("Downloading data from SNB ... ", appendLF = FALSE)
            download.file(url = site, destfile = filename,
                          method = method,
                          quiet = TRUE)
        } else
            if (verbose)
                message("Using cache ... ", appendLF = FALSE)

        dats <- try(readLines(filename), silent = TRUE)
        em <- geterrmessage()

    } else {
        if (verbose)
            message("Downloading data from SNB ... ", appendLF = FALSE)

        con <- url(site)
        dats <- try(readLines(con), silent = TRUE)
        close(con)
        em <- geterrmessage()
    }

    if (inherits(dats, "try-error")) {
        if (verbose) {
            message("failed")
            message(em)
        }
        return(invisible(NULL))
    } else {
        if (verbose)
            message("done")
    }

    empty <- grep("^ *$", dats)
    dats <- read.table(text = dats,
                       sep = ";",
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       as.is = TRUE, skip = empty)
    ans <- tapply(dats$Value, list(dats$Date, dats[, 2]), identity)
    i <- min(which(!is.na(dats$Value)))
    ans <- ans[, dats[1:(i-1), 2]]

    if (!is.null(return.class)) {
        stop("not yet supported")
        if (return.class == "zoo")
            if (requireNamespace("zoo"))
                stop("not yet implemented")
            else
                stop("package ", sQuote("zoo"), " not available")

        else if (return.class == "data.frame")
            result <- ans

        else if (return.class == "list")
            result <- NA 

    } else
        result <- ans

        ## attr(result, "info") <-  doc
    result

}
