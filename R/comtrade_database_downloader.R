#' Download Comtrade Database Period
#' 
#' Part of the \pkg{ComtradeDatabaseDownloader} package. \cr
#' A support function for the \code{\link{get_comtrade}} function.
#' @export
get_comtrade_file <- function(freq, year, month = NULL, token) {
  if (freq == "monthly") {
    freq_parm <- "M" } else { freq_parm <- "A"
    }
  ps_parm <- paste0(year, formatC(month, width = 2, format = "d", flag = "0"))
  
  base_url <- "http://comtrade.un.org/api/get/bulk/C"
  url <- paste(base_url, freq_parm, ps_parm, "ALL", paste0("HS?token", token), sep = "/")
  
  f <- file.path(tempdir(), ps_parm)
  if (file.exists(f)) {
    message(paste0("    ", ps_parm, " already extracted, reading data..."))
    d <- read.csv(list.files(f, full.names = TRUE))
    message(paste0("    cleaning up..."))
    unlink(f, recursive = TRUE)
  } else {
    z <- file.path(tempdir(), paste0(ps_parm, ".zip"))
    if (file.exists(z)) {
      message(paste0("    ", ps_parm, ".zip already downloaded, extracting data..."))
      unzip(z, exdir = f)
      message(paste0("    reading data..."))
      d <- read.csv(list.files(f, full.names = TRUE))
      message(paste0("    cleaning up..."))
      unlink(f, recursive = TRUE)
    } else {
      message(paste0("    downloading data..."))
      download.file(url, z, mode = "wb", quiet = TRUE)
      message(paste0("    extracting data..."))
      unzip(z, exdir = f)
      message(paste0("    reading data..."))
      d <- read.csv(list.files(f, full.names = TRUE))
      message(paste0("    cleaning up..."))
      unlink(f, recursive = TRUE)
    }
  }
  
  return(d)
}

#' Wrangle Comtrade Database Period
#' 
#' Part of the \pkg{ComtradeDatabaseDownloader} package. \cr
#' A support function for the \code{\link{get_comtrade}} function.
#' @export
wrangle_comtrade_data <- function(df, freq) {
  message(paste0("    wrangling..."))
  df <- df[, c(3, 8, 9, 10, 12, 13, 15, 21)]
  df <- df[df$Commodity.Code == "TOTAL",]
  df <- df[df$Trade.Flow %in% c("Imports", "Exports"),]
  df <- df[!(df$Reporter.Code == 0 | df$Partner.Code == 0),]
  df[, "Reporter.Code"] <- unlist(sapply(df$Reporter.Code, function(x) ComtradeDatabaseDownloader::comtrade_names[[as.character(x)]]))
  df[, "Partner.Code"] <- unlist(sapply(df$Partner.Code, function(x) ComtradeDatabaseDownloader::comtrade_names[[as.character(x)]]))
  df <- df[complete.cases(df),]
  if (freq == "monthly") {
    df[, "Year"] <- sapply(df$Period, function(x) as.numeric(substr(x, 1, 4)))
    df[, "Month"] <- sapply(df$Period, function(x) as.numeric(substr(x, 5, 6)))  
  }
  
  return(df)
}

#' Download And Wrangle Comtrade Database Data
#' 
#' Part of the \pkg{ComtradeDatabaseDownloader} package. \cr
#' Download data from the UN Comtrade database and wrangle it into a format which is easily applied to Gravity Model type estimations. Specify start year/month and end year/month (if empty, last available period is assumed). 
#' Specify a directory in \code{savedir} to automatically save the dataframe to a \code{.csv} file.
#' 
#' WARNING: The process might take up to 5 minutes per period to complete, depending on internet speed, processor, memory, etc.
#' 
#' Make sure to provide a valid token generated from \href{https://comtrade.un.org/api/swagger/ui/index#/Auth}{the Comtrade Database} website and that you are connecting from a network with a premium site licence subscription (e.g. your university network).
#' 
#' @param freq A string. Specify the frequency of the reported trade data, either \code{"annual"} or \code{"monthly"}.
#' @param startyear An integer. Specify the first year of the database. \cr >= 1962 for annual, >= 2010 for monthly frequency.
#' @param startmonth An integer. Specify the first month of the database. 
#' @param endyear An integer. Default: Current year. Specify the last year of the database. Must specify the same year or later as the \code{startyear}. 
#' @param endmonth An integer. Default: Previous month. Specify the last month of the database. Must specify previous month or before if current year is specified in \code{lastyear}.
#' @param token A string. Provide a valid token from the UN Comtrade database website. 
#' @param savedir A string. If directory is provided, a file named \code{gravity.csv} will be saved here.
#' @return Returns a dataframe. The dataframe contains the complete set of bilateral trade flows reported to the Comtrade database.
#' @examples 
#' df <- get_comtrade(freq = "monthly", startyear = 2018, startmonth = 1, endyear = 2021, endmonth = 12, token = "YOURTOKENHERE")
#' df <- get_comtrade(freq = "annual", staryear = 2000, token = "YOURTOKENHERE")
#' @export
get_comtrade <- function(freq, startyear, startmonth, endyear = as.numeric(format(Sys.Date(), "%Y")), endmonth = as.numeric(format(Sys.Date(), "%m")) - 1, token, savedir) {
  if (!(is.numeric(startyear))) stop("Non-numeric value provided for startyear")
  if (!(is.numeric(startmonth))) stop("Non-numeric value provided for startmonth")
  if (!(is.numeric(endyear))) stop("Non-numeric value provided for endyear")
  if (!(is.numeric(endmonth))) stop("Non-numeric value provided for endmonth")
  if (missing(freq)) stop("Data frequency was not specified.")
  if (missing(startyear)) stop("Start year was not specified.")
  if (freq == "monthly" & missing(startmonth)) stop("Monthly frequency specified, but startmonth is missing.")
  if (freq == "monthly" & endyear == as.numeric(format(Sys.Date(), "%Y")) & endmonth >= as.numeric(format(Sys.Date(), "%m"))) stop(paste0("Invalid endmonth specified, must be ", as.numeric(format(Sys.Date(), "%m")) - 1, " at the latest when ", format(Sys.Date(), "%Y"), " is specified as endyear."))  
  if (missing(endyear)) stop("End year was not specified")
  if (missing(token)) stop("Comtrade token was not specified")
  if (!(freq %in% c("annual", "monthly"))) stop("Invalid frequency specified \n(Specify either 'annual' or 'monthly')")
  if (freq == "annual" & !(startyear %in% seq(1962, format(Sys.Date(), "%Y")))) stop(paste0("Invalid start year specified, must be 1962 or later."))
  if (freq == "monthly" & !(startyear %in% seq(2010, format(Sys.Date(), "%Y")))) stop(paste0("Invalid start year specified, must be 2010 or later."))
  if (freq == "monthly" & as.numeric(paste0(startyear, formatC(startmonth, width = 2, format = "d", flag = "0"))) >= as.numeric(paste0(endyear, formatC(endmonth, width = 2, format = "d", flag = "0")))) stop(paste0("End period at or before start period specified\n"))
  if (freq == "annual" & startyear >= endyear) stop(paste0("endyear at or before startyear specified\n"))
  if (freq == "monthly" & !(paste0(startyear, formatC(startmonth, width = 2, format = "d", flag = "0")) %in% as.vector(outer(seq(2010, format(Sys.Date(), "%Y")), formatC(seq(1, 12), width = 2, format = "d", flag = "0"), paste0)))) stop(paste0("Invalid startyear/month combination specified\n", "startyear: ", startyear, ", startmonth: ", startmonth))
  if (freq == "monthly" & !(paste0(endyear, formatC(endmonth, width = 2, format = "d", flag = "0")) %in% as.vector(outer(seq(2010, format(Sys.Date(), "%Y")), formatC(seq(1, 12), width = 2, format = "d", flag = "0"), paste0)))) stop(paste0("Invalid endyear/month combination specified\n", "endyear: ", endyear, ", endmonth: ", endmonth))
  
  if (freq == "monthly") {
    if (startyear == endyear) {
      years <- rep(startyear, endmonth - startmonth + 1)
      months <- seq(startmonth, endmonth)
    } else {
      years <- c(rep(startyear, 12 - startmonth + 1), sapply(seq(startyear + 1, endyear - 1), function(x) rep(x, 12)), rep(endyear, endmonth - 1 + 1))
      months <- c(seq(startmonth, 12), rep(seq(1, 12), endyear - startyear - 1), seq(1, endmonth))
    }
    
    starttime <- Sys.time()
    
    message(paste0("[", 1, "/", length(years), "] ", "Processing period ", years[1], formatC(months[1], width = 2, format = "d", flag = "0")))
    df <- get_comtrade_file(freq, years[1], months[1], token)
    df <- wrangle_comtrade_data(df, freq)
    
    if (length(years) > 1) {
      for (i in seq(2, length(years))){
        message(paste0("[", i, "/", length(years), "] ", "Processing period ", years[i], formatC(months[i], width = 2, format = "d", flag = "0")))
        temp <- get_comtrade_file(freq, years[i], months[i], token)
        temp <- wrangle_comtrade_data(temp, freq) 
        df <- rbind(df, temp)
      }
    }
    
    df <- df[, c(1, 9, 10, 2:6, 8)]
    colnames(df) <- c("period", "year", "month", "flow", "repcode", "reporter", "parcode", "partner", "value")
    df <- df[order(df$repcode, df$parcode, df$year, df$month),]
  } else {
    years <- seq(startyear, endyear)
      
    starttime <- Sys.time()
    
    message(paste0("[", 1, "/", length(years), "] ", "Processing period ", years[1]))
    df <- get_comtrade_file(freq, years[1], token = token)
    df <- wrangle_comtrade_data(df, freq)
    
    if (length(years) > 1) {
      for (i in seq(2, length(years))){
        message(paste0("[", i, "/", length(years), "] ", "Processing period ", years[i]))
        temp <- get_comtrade_file(freq, years[i], token = token)
        temp <- wrangle_comtrade_data(temp, freq)
        df <- rbind(df, temp)
      }
    }
    
    df <- df[, c(1, 2:6, 8)]
    colnames(df) <- c("year", "flow", "repcode", "reporter", "parcode", "partner", "value")
    df <- df[order(df$repcode, df$parcode, df$year),]
  }
  
  if (!missing(savedir)) {
    message("Saving 'gravity.csv'...")
    write.csv(df, file.path(savedir, "gravity.csv"), row.names = FALSE)
  }
  
  endtime <- Sys.time()
  message(paste0("Complete!\nTime Elapsed: "), round(difftime(endtime, starttime)[[1]], 2), " minutes")
  return(df)
}