fetch_data <- function(id,
                       type = "table",
                       dest.dir = NULL,
                       return.class = NULL,
                       verbose = TRUE,
                       method,
                       language = "en",
                       name.sep = " :: ",
                       na.drop = TRUE,
                       do.timeseries = FALSE, ...) {

    if (type == "table") {
        site <- paste0("https://data.snb.ch/api/cube/",
                       id, "/data/csv/", language)
    } else if (type == "dataset") {
        site <- paste0("https://data.snb.ch/api/warehouse/cube/",
                       gsub("@", ".", id),
                       "/data/csv/", language)
    } else {
        stop("either table of dataset must be specified")
    }

    info <- fetch_info(id = id, type = type,
                       language = language,
                       dest.dir = dest.dir,
                       name.sep = name.sep,
                       method = method)

    if (!is.null(dest.dir)) {
        filename <- paste0(format(Sys.Date(), "%Y%m%d"),
                           "__", id,
                           "__", language,
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

        dats <- try(readLines(filename, warn = FALSE), silent = TRUE)
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
    header <- dats[1:empty]
    dats <- read.table(text = dats,
                       sep = ";",
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       as.is = TRUE, skip = empty, ...)

    if (do.timeseries) {
        id <- dats[[2]][seq(1, min(which(duplicated(dats[[2]]))) - 1)]
        ans <- vector("list", length(id))
        names(ans) <- as.character(id)
        for (i in as.character(id)) {
            ans[[i]] <- dats[dats[, 2] == i, -2]
        }
        result <- do.call(cbind, lapply(ans, `[[`, 2))
        row.names(result) <- ans[[1]][[1]]

        if (na.drop) {
            drop <- apply(result, 1, function(x) all(!is.finite(x)))
            result <- result[!drop,, drop = FALSE]
        }

    } else {
        result <- dats
    }

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

    }

    attr(result, "info") <- unlist(info)
    result
}



fetch_last_update <- function(id,
                              type = "table",
                              dataset,
                              dest.dir = NULL,
                              verbose = TRUE,
                              language = "en", ...) {
    if (!is.null(dest.dir))
        message("currently not supported")

    site <- if (type == "table") {
                paste0("https://data.snb.ch/api/cube/",
                       id, "/lastUpdate")
            } else if (type == "dataset") {
                paste0("https://data.snb.ch/api/warehouse/cube/",
                       gsub("@", ".", id), "/lastUpdate")
            }

    con <- url(site)
    ans <- try(readLines(con, warn = FALSE), silent = TRUE)
    try(close(con), silent = TRUE)
    em <- geterrmessage()
    if (requireNamespace("jsonlite"))
        jsonlite::fromJSON(ans)
    else
        ans
}



fetch_info <- function(id,
                       type = "table",
                       dest.dir = NULL,
                       verbose = TRUE,
                       language = "en",
                       name.sep = " :: ",
                       method, ...) {

    .do_item <- function(item, path = "", name.sep = " :: ") {
        if (length(item) == 2 &&
            identical(names(item), c("id", "name"))) {
            id.info[[item$id]] <<- paste0(path, name.sep, item$name)
        } else {
            if (!is.null(item$name))
                path <- paste0(path,
                               if (path != "") name.sep,
                               item$name)
            for (i in item) {
                if (is.list(i))
                    .do_item(i, path, name.sep)
            }
        }
    }

    if (type == "table") {
        site <- paste0("https://data.snb.ch/api/cube/",
                       id, "/dimensions/", language)
    } else if (type == "dataset") {
        site <- paste0("https://data.snb.ch/api/warehouse/cube/",
                       gsub("@", ".", id), "/dimensions/", language)
    }

    if (!is.null(dest.dir)) {
        filename <- paste0(format(Sys.Date(), "%Y%m%d"),
                           "__", id,
                           "__", language,
                           "__info.csv")
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

        dats <- try(readLines(filename, warn = FALSE), silent = TRUE)
        em <- geterrmessage()

    } else {
        if (verbose)
            message("Downloading data from SNB ... ", appendLF = FALSE)

        con <- url(site)
        dats <- try(readLines(con, warn = FALSE), silent = TRUE)
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

    if (requireNamespace("jsonlite")) {
        info <- jsonlite::fromJSON(dats, FALSE)
        items <- info$dimensions ## overview

        id.info <- list()
        .do_item(items, path = "", name.sep = name.sep)
        dats <- id.info
    }

    dats
}



## fetch_table_date <- function(table = "rendoblid",
##                              dest.dir = NULL,
##                              verbose = TRUE,
##                              language = "en", ...) {

##     if (!is.null(dest.dir))
##         message("currently not supported")

##     site <- paste0("https://data.snb.ch/api/cube/", table, "/lastUpdate")
##     con <- url(site)
##     ans <- try(readLines(con, warn = FALSE), silent = TRUE)
##     try(close(con), silent = TRUE)
##     em <- geterrmessage()
##     if (requireNamespace("jsonlite"))
##         jsonlite::fromJSON(ans)
##     else
##         ans
## }



## fetch_table_info <- function(table = "rendoblid",
##                              dest.dir = NULL,
##                              return.class = NULL,
##                              verbose = TRUE,
##                              language = "en",
##                              method, ...) {

##     site <- paste0("https://data.snb.ch/api/cube/",
##                    table, "/dimensions/",
##                    language)
##     if (!is.null(dest.dir)) {
##         filename <- paste0(format(Sys.Date(), "%Y%m%d"),
##                            "__", table,
##                            "__", language,
##                            "__info.csv")
##         filename <- file.path(dest.dir, filename)
##         if (!file.exists(filename)) {
##             if (verbose)
##                 message("Downloading data from SNB ... ", appendLF = FALSE)
##             download.file(url = site, destfile = filename,
##                           method = method,
##                           quiet = TRUE)
##         } else
##             if (verbose)
##                 message("Using cache ... ", appendLF = FALSE)

##         dats <- try(readLines(filename, warn = FALSE), silent = TRUE)
##         em <- geterrmessage()

##     } else {
##         if (verbose)
##             message("Downloading data from SNB ... ", appendLF = FALSE)

##         con <- url(site)
##         dats <- try(readLines(con), silent = TRUE)
##         close(con)
##         em <- geterrmessage()
##     }

##     if (inherits(dats, "try-error")) {
##         if (verbose) {
##             message("failed")
##             message(em)
##         }
##         return(invisible(NULL))
##     } else {
##         if (verbose)
##             message("done")
##     }

##     if (requireNamespace("jsonlite"))
##         jsonlite::fromJSON(dats, FALSE)
##     else
##         dats
## }

## fetch_table <- function(table = "rendoblid",
##                         dest.dir = NULL,
##                         return.class = NULL,
##                         verbose = TRUE,
##                         method,
##                         language = "en",
##                         name.sep = " :: ",
##                         na.drop = TRUE, ...) {

##     .do_item <- function(item, path = "", name.sep = " :: ") {
##         if (length(item) == 2 &&
##             identical(names(item), c("id", "name"))) {
##             id.info[[item$id]] <<- paste0(path, name.sep, item$name)
##         } else {
##             if (!is.null(item$name))
##                 path <- paste0(path,
##                                if (path != "") name.sep,
##                                item$name)
##             for (i in item) {
##                 if (is.list(i))
##                     .do_item(i, path, name.sep)
##             }
##         }
##     }

##     info <- fetch_table_info(table,
##                              language = language,
##                              dest.dir = dest.dir,
##                              method = method)
##     items <- info$dimensions ## overview
##     id.info <- list()
##     if (requireNamespace("jsonlite"))
##         .do_item(items, path = "", name.sep = name.sep)
##     else
##         id.info <- info

##     site <- paste0("https://data.snb.ch/api/cube/",
##                    table, "/data/csv/", language)

##     if (!is.null(dest.dir)) {
##         filename <- paste0(format(Sys.Date(), "%Y%m%d"),
##                            "__", table,
##                            ".csv")
##         filename <- file.path(dest.dir, filename)
##         if (!file.exists(filename)) {
##             if (verbose)
##                 message("Downloading data from SNB ... ", appendLF = FALSE)
##             download.file(url = site, destfile = filename,
##                           method = method,
##                           quiet = TRUE)
##         } else
##             if (verbose)
##                 message("Using cache ... ", appendLF = FALSE)

##         dats <- try(readLines(filename), silent = TRUE)
##         em <- geterrmessage()

##     } else {
##         if (verbose)
##             message("Downloading data from SNB ... ", appendLF = FALSE)

##         con <- url(site)
##         dats <- try(readLines(con), silent = TRUE)
##         close(con)
##         em <- geterrmessage()
##     }

##     if (inherits(dats, "try-error")) {
##         if (verbose) {
##             message("failed")
##             message(em)
##         }
##         return(invisible(NULL))
##     } else {
##         if (verbose)
##             message("done")
##     }

##     empty <- grep("^ *$", dats)
##     header <- dats[1:empty]
##     dats <- read.table(text = dats,
##                        sep = ";",
##                        header = TRUE,
##                        stringsAsFactors = FALSE,
##                        as.is = TRUE, skip = empty, ...)

##     id <- dats[[2]][seq(1, min(which(duplicated(dats[[2]]))) - 1)]
##     ans <- vector("list", length(id))
##     names(ans) <- as.character(id)
##     for (i in as.character(id)) {
##         ans[[i]] <- dats[dats[, 2] == i, -2]
##     }
##     result <- do.call(cbind, lapply(ans, `[[`, 2))
##     row.names(result) <- ans[[1]][[1]]

##     if (na.drop) {
##         drop <- apply(result, 1, function(x) all(!is.finite(x)))
##         result <- result[!drop,, drop = FALSE]
##     }
##     if (!is.null(return.class)) {
##         stop("not yet supported")
##         if (return.class == "zoo")
##             if (requireNamespace("zoo"))
##                 stop("not yet implemented")
##             else
##                 stop("package ", sQuote("zoo"), " not available")

##         else if (return.class == "data.frame")
##             result <- ans

##         else if (return.class == "list")
##             result <- NA

##     }

##     attr(result, "info") <- unlist(id.info)
##     result
## }

## fetch_dataset <- function(dataset = "BSTA@SNB.JAHR_U.BIL.AKT.TOT",
##                           dest.dir = NULL,
##                           return.class = NULL,
##                           verbose = TRUE,
##                           method,
##                           language = "en",
##                           name.sep = " :: ",
##                           na.drop = TRUE, ...) {

##     .do_item <- function(item, path = "", name.sep = " :: ") {
##         if (length(item) == 2 &&
##             identical(names(item), c("id", "name"))) {
##             id.info[[item$id]] <<- paste0(path, name.sep, item$name)
##         } else {
##             if (!is.null(item$name))
##                 path <- paste0(path,
##                                if (path != "") name.sep,
##                                item$name)
##             for (i in item) {
##                 if (is.list(i))
##                     .do_item(i, path, name.sep)
##             }
##         }
##     }

##     info <- fetch_info(dataset = dataset,
##                        language = language,
##                        dest.dir = dest.dir,
##                        method = method)
##     items <- info$dimensions ## overview
##     id.info <- list()
##     if (requireNamespace("jsonlite"))
##         .do_item(items, path = "", name.sep = name.sep)
##     else
##         id.info <- info

##     site <- paste0("https://data.snb.ch/api/warehouse/cube/",
##                    gsub("@", ".", dataset),
##                    "/data/csv/", language)

##     if (!is.null(dest.dir)) {
##         filename <- paste0(format(Sys.Date(), "%Y%m%d"),
##                            "__", dataset,
##                            ".csv")
##         filename <- file.path(dest.dir, filename)
##         if (!file.exists(filename)) {
##             if (verbose)
##                 message("Downloading data from SNB ... ", appendLF = FALSE)
##             download.file(url = site, destfile = filename,
##                           method = method,
##                           quiet = TRUE)
##         } else
##             if (verbose)
##                 message("Using cache ... ", appendLF = FALSE)

##         dats <- try(readLines(filename), silent = TRUE)
##         em <- geterrmessage()

##     } else {
##         if (verbose)
##             message("Downloading data from SNB ... ", appendLF = FALSE)

##         con <- url(site)
##         dats <- try(readLines(con), silent = TRUE)
##         close(con)
##         em <- geterrmessage()
##     }

##     if (inherits(dats, "try-error")) {
##         if (verbose) {
##             message("failed")
##             message(em)
##         }
##         return(invisible(NULL))
##     } else {
##         if (verbose)
##             message("done")
##     }

##     empty <- grep("^ *$", dats)
##     header <- dats[1:(empty - 1)]
##     dats <- read.table(text = dats,
##                        sep = ";",
##                        header = TRUE,
##                        stringsAsFactors = FALSE,
##                        as.is = TRUE, skip = empty, ...)

##     ## id <- dats[[2]][seq(1, min(which(duplicated(dats[[2]]))) - 1)]
##     ## ans <- vector("list", length(id))
##     ## names(ans) <- as.character(id)
##     ## for (i in as.character(id)) {
##     ##     ans[[i]] <- dats[dats[, 2] == i, -2]
##     ## }
##     ## result <- do.call(cbind, lapply(ans, `[[`, 2))
##     ## row.names(result) <- ans[[1]][[1]]
##     result <- dats

##     if (na.drop) {
##         warning("ignored")

##         ## drop <- apply(result, 1, function(x) all(!is.finite(x)))
##         ## result <- result[!drop,, drop = FALSE]
##     }
##     if (!is.null(return.class)) {
##         stop("not yet supported")
##         if (return.class == "zoo")
##             if (requireNamespace("zoo"))
##                 stop("not yet implemented")
##             else
##                 stop("package ", sQuote("zoo"), " not available")

##         else if (return.class == "data.frame")
##             result <- ans

##         else if (return.class == "list")
##             result <- NA

##     }

##     attr(result, "info") <- unlist(id.info)
##     result
## }
