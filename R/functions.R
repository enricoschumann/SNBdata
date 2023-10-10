fetch_data <- function(id,
                       type = "table",
                       dest.dir = NULL,
                       return.class = NULL,
                       verbose = TRUE,
                       language = "en",
                       name.sep = " :: ",
                       method,
                       na.drop = TRUE,
                       time.series = FALSE, ...) {

    if (type == "table") {
        site <- paste0("https://data.snb.ch/api/cube/",
                       id, "/data/csv/", language)
    } else if (type == "dataset") {
        site <- paste0("https://data.snb.ch/api/warehouse/cube/",
                       gsub("@", ".", id),
                       "/data/csv/", language)
    } else {
        stop("type must be either table or dataset")
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
                       quote = 
                       stringsAsFactors = FALSE,
                       as.is = TRUE, skip = empty, ...)

    if (time.series) {
        date.col <- grep("Date", colnames(dats))
        value.col <- grep("Value", colnames(dats))
        if (!length(date.col) || !length(value.col)) {
            message("both ", sQuote("Date"),
                    " and ", sQuote("Value"),
                    " column required for timeseries")
        } else {
            other.col <- setdiff(colnames(dats),
                                 colnames(dats)[c(date.col,
                                                  value.col)])

            dates <- sort(unique(dats[, date.col]))
            if (all(grepl("[12][0-9]{3}-[0-9]{2}-[0-9]{2}",
                          dates, perl = TRUE)))
                dates <- as.Date(dates)
            if (length(other.col) > 1L)
                groups <- apply(dats[, other.col], 1, paste0,
                                collapse = name.sep)
            else
                groups <- dats[, other.col]
            u.groups <- unique(groups)
            result <- array(numeric(1),
                            dim = c(length(dates),
                                    length(u.groups)))
            result <- as.data.frame(result)
            result <- cbind(Date = sort(dates), result)
            colnames(result) <- c("Date", u.groups)

            for (g in u.groups) {
                tmp <- dats[g == groups, ]
                ## no (documented) guarantee data are sorted
                i <- match(tmp$Date, dates)
                result[i, g] <- tmp[, "Value"]
            }


            ## id <- dats[[2]][seq(1, min(which(duplicated(dats[[2]]))) - 1)]
            ## ans <- vector("list", length(id))
            ## names(ans) <- as.character(id)
            ## for (i in as.character(id)) {
            ##     ans[[i]] <- dats[dats[, 2] == i, -2]
            ## }
            ## result <- do.call(cbind, lapply(ans, `[[`, 2))
            ## row.names(result) <- ans[[1]][[1]]

            if (na.drop) {
                drop <- apply(result[, -1], 1,
                              function(x) all(!is.finite(x)))
                result <- result[!drop,, drop = FALSE]
            }
            attr(result, "columns") <- other.col
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
            NULL
        else if (return.class == "list")
            result <- NA

    }

    attr(result, "dimensions") <- unlist(info)
    result
}



fetch_last_update <- function(id,
                              type = "table",
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
