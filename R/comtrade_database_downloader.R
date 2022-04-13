#' Download Comtrade Database Period
#' 
#' Part of the \pkg{ComtradeDatabaseDownloader} package. \cr
#' A support function for the \code{\link{get_comtrade}} function.
#' 
#' @export
get_comtrade_file <- function(freq, year, month = NULL, token) 
{
  # Compose URL used to call Comtrade API
  base_url <- "http://comtrade.un.org/api/get/bulk/C"
  if (freq == "monthly") {
    freq_parm <- "M" 
  } else { 
    freq_parm <- "A"
  }
  ps_parm <- paste0(year, formatC(month, width = 2, format = "d", flag = "0"))
  url <- paste(
    base_url, 
    freq_parm, 
    ps_parm, 
    "ALL", 
    if (freq == "monthly") {
      paste0("HS?token", token)
    } else {
      paste0("S1?token", token)
    }
    , sep = "/"
  )
  
  # This code chunk will download, extract and import the raw data file from
  # Comtrade. If the file was previously downloaded and stored in the temp
  # folder, it will skip this step.
  f <- file.path(tempdir(), ps_parm)
  if (file.exists(f)) {
    message(paste0(
      "    ", ps_parm, " already extracted, reading data...")
    )
  } else {
    z <- file.path(tempdir(), paste0(ps_parm, ".zip"))
    if (file.exists(z)) {
      message(paste0(
        "    ", ps_parm, ".zip already downloaded, extracting data...")
      )
      utils::unzip(z, exdir = f)
      message(paste0("    reading data..."))
    } else {
      message(paste0("    downloading data..."))
      tryCatch({
        suppressWarnings(
          utils::download.file(url, z, mode = "wb", quiet = TRUE)
        )
      }, error = function(e) {
        message(paste0("    downloading failed, retrying..."))
        tryCatch({
          suppressWarnings(
            utils::download.file(url, z, mode = "wb", quiet = TRUE)
          )
        }, error = function(e) {
          message(paste0("    downloading failed, retrying..."))
          tryCatch({
            suppressWarnings(
              utils::download.file(url, z, mode = "wb", quiet = TRUE)
            )
          }, error = function(e) {
            stop("Download failed repeatedly, check token or try again later.")
          })
        })
      })
      message(paste0("    extracting data..."))
      utils::unzip(z, exdir = f)
      message(paste0("    reading data..."))
    }
  }
  df <- data.table::fread(
    list.files(f, full.names = TRUE), 
    encoding = "UTF-8",
    data.table = FALSE, 
    check.names = TRUE, 
    showProgress = FALSE, 
    select = list("integer" = 3, "character" = 8, "integer" = 9, 
                  "character" = 10, "integer" = 12, "character" = 13, 
                  "character" = 15, "integer64" = 21)
  )
  
  # Delete temporary files
  message(paste0("    cleaning up..."))
  unlink(c(f, z), recursive = TRUE)
  
  return(df)
}

#' Wrangle Comtrade Database Period
#' 
#' Part of the \pkg{ComtradeDatabaseDownloader} package. \cr
#' A support function for the \code{\link{get_comtrade}} function.
#' @export
wrangle_comtrade_data <- function(df, freq) 
{
  message(paste0("    wrangling..."))
  
  # Filter the aggregated trade values
  df <- df[df$Commodity.Code == "TOTAL",]

  # Filter Import and Export flows
  df <- df[df$Trade.Flow %in% c("Imports", "Exports", "Import", "Export"),]
  
  # Filter out trade with reporter/partner "the World", "EU-28" or
  # "Southern African Customs Union"
  excluded_codes <- c(0, 97, 711)
  df <- df[!(
    df$Reporter.Code %in% excluded_codes | df$Partner.Code %in% excluded_codes
  ),]
  
  # Convert reporter and partner country code from UN numeric to ISO 3166 
  # Alpha-3 codes
  df[, "Reporter.Code"] <- unlist(vapply(
    df$Reporter.Code, 
    function(x) ComtradeDatabaseDownloader::comtrade_names[[as.character(x)]],
    character(1)
  ))
  df[, "Partner.Code"] <- unlist(vapply(
    df$Partner.Code, 
    function(x) ComtradeDatabaseDownloader::comtrade_names[[as.character(x)]],
    character(1)
  ))
  
  # Remove reporter/partner entities without a valid Alpha-3 code
  df <- df[stats::complete.cases(df),]
  
  # Split "Period" variable into year and month values
  if (freq == "monthly") {
    df[, "Year"] <- vapply(
      df$Period, 
      function(x) as.numeric(substr(x, 1, 4)),
      numeric(1))
    df[, "Month"] <- vapply(
      df$Period, 
      function(x) as.numeric(substr(x, 5, 6)),
      numeric(1))  
  }
  
  # Rearrange and rename variables
  if (freq == "monthly") {
    df <- df[, c(1, 9, 10, 2:6, 8)]
    colnames(df) <- c(
      "period", "year", "month", "flow", 
      "repcode", "reporter", "parcode", "partner", 
      "value"
    )
    # Reorder observations
    df <- df[order(df$repcode, df$parcode, df$flow, df$year, df$month),]
  } else {
    df <- df[, c(1:6, 8)]
    colnames(df) <- c(
      "year", "flow", 
      "repcode", "reporter", "parcode", "partner", 
      "value"
    )
    df <- df[order(df$repcode, df$parcode, df$flow, df$year),]
  }
  
  return(df)
}

#' Download And Wrangle Comtrade Database Data
#' 
#' Part of the \pkg{ComtradeDatabaseDownloader} package. \cr
#' Download data from the UN Comtrade database and wrangle it into a format 
#' which is easily applied to Gravity Model type estimations. Specify start 
#' year/month and end year/month (if empty, last available period is assumed). 
#' Specify a directory in \code{savedir} to automatically save the dataframe to 
#' a \code{.csv} file.
#' 
#' WARNING: The process might take up to 5 minutes per period to complete, 
#' depending on internet speed, processor, memory, etc.
#' 
#' Make sure to provide a valid token generated from 
#' \href{https://comtrade.un.org/api/swagger/ui/index#/Auth}{the Comtrade 
#' Database} website and that you are connecting from a network with a premium 
#' site license subscription (e.g. your university network).
#' 
#' @param freq A single character string. Specify the frequency of the reported 
#' trade data, either \code{"annual"} or \code{"monthly"}.
#' @param startyear A single integer. Specify the first year of the database. 
#' \cr >= 1962 for annual, >= 2010 for monthly frequency.
#' @param startmonth A single integer. Specify the first month of the database. 
#' @param endyear A single integer. Default: Previous year. Specify the last 
#' year of the database. Must specify the same year or later as the 
#' \code{startyear}. 
#' @param endmonth A single integer. Default: Previous month. Specify the last 
#' month of the database. Must specify previous month or before if current year 
#' is specified in \code{lastyear}.
#' @param token A single character string. Provide a valid token from the UN 
#' Comtrade database website. 
#' @param savedir A single character string containing a valid directory. If 
#' \code{savedir} is specified, a file named \code{gravity.csv} containing the 
#' output will be saved here.
#' @param int64 A single logical. Default: \code{TRUE}. If \code{TRUE} is 
#' specified, the \code{data.frame} will retain \code{Integer64} class of the 
#' \code{value} column as. If \code{FALSE} is specified, the column will be 
#' converted to \code{numeric} (since some \code{R} operations do not support 
#' the \code{Integer64} class).
#' @return Returns a \code{data.frame}. The \code{data.frame} contains the 
#' complete set of bilateral trade flows reported to the Comtrade database 
#' within the specified time period.
#' @examples 
#' \dontrun{
#' df <- get_comtrade(
#'     freq = "monthly", 
#'     startyear = 2018, 
#'     startmonth = 1, 
#'     endyear = 2021, 
#'     endmonth = 12, 
#'     token = "YOURTOKENHERE"
#' )
#' 
#' df <- get_comtrade(
#'     freq = "annual", 
#'     staryear = 2000, 
#'     token = "YOURTOKENHERE"
#' )}
#' @export
get_comtrade <- function(freq, 
                         startyear, 
                         startmonth = NULL, 
                         endyear = NULL,
                         endmonth = NULL,
                         token, 
                         savedir = NULL,
                         int64 = TRUE) 
{
  # Assigning default values
  if (freq == "annual") {
    if (missing(endyear)) {
      endyear <- as.numeric(format(Sys.Date(), "%Y")) - 1
    }  
  } else {
    if (missing(endyear)) {
      if (as.numeric(format(Sys.Date(), "%m")) == 1) {
        endyear <- as.numeric(format(Sys.Date(), "%Y")) - 1
        endmonth <- 12
      } else {
        endyear <- as.numeric(format(Sys.Date(), "%Y"))
        endmonth <- as.numeric(format(Sys.Date(), "%m")) - 1
      }
    } else {
      if (as.numeric(format(Sys.Date(), "%m")) == 1) {
        endyear <- as.numeric(format(Sys.Date(), "%Y")) - 1
        endmonth <- 12
      } else if (missing(endmonth)) {
        endmonth <- as.numeric(format(Sys.Date(), "%m")) - 1
      }
    }
  }
  
  # Error handling
  padded_startmonth <- formatC(startmonth, width = 2, format = "d", flag = "0")
  padded_endmonth <- formatC(endmonth, width = 2, format = "d", flag = "0")
  
  if (!(freq %in% c("annual", "monthly"))) {
    stop(paste0(
      "Invalid frequency specified, ", 
      "specify either 'annual' or 'monthly'."
    ))
  }
  if (missing(startyear)) {
    stop("startyear was not specified.")
  }
  if (freq == "monthly" & missing(startmonth)) {
    stop("Monthly frequency specified, but startmonth is missing.")
  }
  if (!missing(startyear) & !(is.numeric(startyear))) {
    stop("Non-numeric value provided for startyear.")
  }
  if (!missing(startmonth) & !(is.numeric(startmonth))) {
    stop("Non-numeric value provided for startmonth.")
  }
  if (!missing(endyear) & !(is.numeric(endyear))) {
    stop("Non-numeric value provided for endyear.")
  }
  if (!missing(endmonth) & !(is.numeric(endmonth))) {
    stop("Non-numeric value provided for endmonth.")
  }
  if (missing(freq)) {
    stop("Data frequency was not specified.")
  }
  if (freq == "monthly") {
    if (
      endyear == as.numeric(format(Sys.Date(), "%Y")) &
      endmonth >= as.numeric(format(Sys.Date(), "%m"))
    ) {
      stop(paste0(
        "Invalid endmonth specified, must be ",
        as.numeric(format(Sys.Date(), "%m")) - 1,
        " at the latest when ",
        format(Sys.Date(), "%Y"),
        " is specified as endyear."
      ))
    }
  }
  if (
    freq == "annual" & 
    !(startyear %in% seq(1962, as.numeric(format(Sys.Date(), "%Y")) - 1))
  ) {
    stop(paste0(
      "Invalid startyear specified, must be between 1962 and ",
      as.numeric(format(Sys.Date(), "%Y")) - 1, "."
    ))
  }
  if (
    freq == "monthly" & 
    !(startyear %in% seq(2010, format(Sys.Date(), "%Y")))
  ) {
    stop(paste0(
      "Invalid startyear specified, must be between 2010 and ",
      format(Sys.Date(), "%Y"), "."
    ))
  }
  if (
    freq == "monthly" & 
    as.numeric(paste0(
      startyear, 
      padded_startmonth)) > 
    as.numeric(paste0(
      endyear, padded_endmonth))
  ) {
    stop("End period before start period specified.")
  }
  if (freq == "annual" & startyear > endyear) {
    stop("endyear before startyear specified.")
  }
  if (
    freq == "monthly" & 
    !(paste0(startyear, padded_startmonth) %in% 
      as.vector(outer(
        seq(2010, format(Sys.Date(), "%Y")), 
        formatC(seq(1, 12), width = 2, format = "d", flag = "0"), 
        paste0)))
  ) {
    stop(paste0(
      "Invalid startyear/month combination specified\n", 
      "startyear: ", startyear, 
      ", startmonth: ", startmonth
    ))
  }
  if (
    freq == "monthly" & 
    !(paste0(endyear, padded_endmonth) %in% 
      as.vector(outer(
        seq(2010, format(Sys.Date(), "%Y")), 
        formatC(seq(1, 12), width = 2, format = "d", flag = "0"), 
        paste0)))
  ) {
    stop(paste0(
      "Invalid endyear/month combination specified\n", 
      "endyear: ", endyear, 
      ", endmonth: ", endmonth
    ))
  }
  if (missing(token)) {
    stop("Comtrade token was not specified.")
  }
  if (!is.character(token)) {
    stop("Comtrade token must be a string.")
  }
  
  # If monthly frequency is specified
  if (freq == "monthly") {
    # Generate looping vectors
    if (startyear == endyear) {
      years <- rep(startyear, endmonth - startmonth + 1)
      months <- seq(startmonth, endmonth)
    } else {
      years <- c(
        rep(startyear, 12 - startmonth + 1), 
        vapply(
          seq(startyear + 1, endyear - 1), 
          function(x) rep(x, 12), numeric(12)), 
        rep(endyear, endmonth - 1 + 1)
      )
      months <- c(
        seq(startmonth, 12), 
        rep(seq(1, 12), endyear - startyear - 1), 
        seq(1, endmonth)
      )
    }
    
    # Start timer
    starttime <- Sys.time()
    
    # Assign directory for saving temp file
    t <- file.path(tempdir(), "monthlytemp.csv")
    
    # If temp file from previous download exists, load temp file
    if (file.exists(t)) {
      df <- data.table::fread(
        t, 
        encoding = "UTF-8",
        data.table = FALSE, 
        showProgress = FALSE, 
        select = list("integer" = 1, "integer" = 2, "integer" = 3, 
                      "character" = 4, "character" = 5, "character" = 6, 
                      "character" = 7, "character" = 8, "integer64" = 9)
      )
      
      # Save first and last periods of the temp file
      tstartyear <- df[1, "year"]
      tstartmonth <- df[1, "month"]
      tendyear <- df[nrow(df), "year"] 
      tendmonth <- df[nrow(df), "month"]
      
      # If temp start period does not match with current request, start over
      if (!(paste0(tstartyear, tstartmonth) == paste0(years, months)[1])) {
        message(paste0(
          "Period specified: From ",
          month.abb[startmonth], " ", startyear, " to ",
          month.abb[endmonth], " ", endyear, ".\n",
          "Estimated time to process: ",
          length(years) * 2.5, " minutes\n"
        ))
        message(paste0(
          "[", 1, "/", length(years), "] ", 
          "Processing period: ", 
          month.abb[months[1]], " ", years[1]
        ))
        
        # Run downloader/importer and wrangler functions
        df <- get_comtrade_file(freq, years[1], months[1], token)
        df <- wrangle_comtrade_data(df, freq)
        
      } else {
        # If temp start period matches with current request, continue on from
        # previous attempt.
        cutoff_index <- match(
          paste0(tendyear, tendmonth), 
          paste0(years, months)
        )
        years <- years[cutoff_index:length(years)]
        months <- months[cutoff_index:length(months)]
        
        message(paste0(
          "Period specified: From ",
          month.abb[startmonth], " ", startyear, " to ",
          month.abb[endmonth], " ", endyear, ".\n",
          "Previous temp file found, continuing from ",
          month.abb[months[2]], " ", years[2], ".",
          "\nEstimated time to process: ",
          length(years) * 2.5, " minutes\n",
          "[", 1, "/", length(years), "] ",
          "Importing temp file periods..."
        ))
      }
      
    } else {
      # If temp file does not exist, initiate process.
      message(paste0(
        "Period specified: From ",
        month.abb[startmonth], " ", startyear, " to ",
        month.abb[endmonth], " ", endyear, ".\n",
        "Estimated time to process: ",
        length(years) * 2.5, " minutes\n"
      ))
      message(paste0(
        "[", 1, "/", length(years), "] ", 
        "Processing period: ", 
        month.abb[months[1]], " ", years[1]
      ))
      
      df <- get_comtrade_file(freq, years[1], months[1], token)
      df <- wrangle_comtrade_data(df, freq)
    }
    
    # Loop if more than one period is specified to be downloaded
    if (length(years) > 1) {
      for (i in seq(2, length(years))){
        message(paste0(
          "[", i, "/", length(years), "] ", 
          "Processing period: ", 
          month.abb[months[i]], " ", years[i]
        ))
        
        temp <- get_comtrade_file(freq, years[i], months[i], token)
        temp <- wrangle_comtrade_data(temp, freq) 
        
        # Row-bind data.frames
        df <- rbind(df, temp)
        
        # Save progress to temp file
        data.table::fwrite(df, t, showProgress = FALSE)
      }
    }
    
  # If annual frequency is specified: 
  } else {
    # Generate looping vectors
    years <- seq(startyear, endyear)
    
    # Start timer  
    starttime <- Sys.time()
    
    # Assign directory for saving temp file
    t <- file.path(tempdir(), "annualtemp.csv")
    
    # If temp file from previous aborted download exists, load temp file
    if (file.exists(t)) {
      df <- data.table::fread(
        t, 
        encoding = "UTF-8",
        data.table = FALSE, 
        showProgress = FALSE, 
        select = list("integer" = 1, "character" = 2, "character" = 3, 
                      "character" = 4, "character" = 5, "character" = 6, 
                      "integer64" = 7)
      )
      
      # Save first and last periods of the temp file
      tstartyear <- df[1, "year"]
      tendyear <- df[nrow(df), "year"] 
      
      # If temp start year does not match with current request, start over
      if (!(tstartyear == years[1])) {
        message(paste0(
          "Period specified: From ",
          startyear, " to ", endyear, ".\n",
          "Estimated time to process: ",
          length(years) * 2.5, " minutes\n"
        ))
        message(paste0(
          "[", 1, "/", length(years), "] ", 
          "Processing period: ", 
          years[1]
        ))
        
        # Run downloader/importer and wrangler functions
        df <- get_comtrade_file(freq, years[1], token = token)
        df <- wrangle_comtrade_data(df, freq)
        
      } else {
        # If temp start year matches with current request, continue on from
        # previous attempt.
        cutoff_index <- match(tendyear, years)
        years <- years[cutoff_index:length(years)]
        
        message(paste0(
          "Period specified: From ",
          startyear, " to ", endyear, ".\n",
          "Previous temp file found, continuing from ", 
          years[2], ".",
          "\nEstimated time to process: ",
          length(years) * 2.5, " minutes\n",
          "[", 1, "/", length(years), "] ",
          "Importing temp file periods..."
        ))
      }
      
    } else {
      # If temp file does not exist, initiate process.
      message(paste0(
        "Period specified: From ",
        startyear, " to ", endyear, ".\n",
        "Estimated time to process: ",
        length(years) * 2.5, " minutes\n"
      ))
      message(paste0(
        "[", 1, "/", length(years), "] ", 
        "Processing period: ", 
        years[1]
      ))
      
      df <- get_comtrade_file(freq, years[1], token = token)
      df <- wrangle_comtrade_data(df, freq)
    }
    
    # Loop if more than one period is specified to be downloaded
    if (length(years) > 1) {
      for (i in seq(2, length(years))){
        message(paste0(
          "[", i, "/", length(years), "] ", 
          "Processing period: ", 
          years[i]
        ))
        
        temp <- get_comtrade_file(freq, years[i], token = token)
        temp <- wrangle_comtrade_data(temp, freq)
        
        # Row-bind data.frames
        df <- rbind(df, temp)
        
        # Save progress to temp file
        data.table::fwrite(df, t, showProgress = FALSE)
      }
    }
  }
  
  # Reset data.frame row index
  row.names(df) <- NULL
  
  # Save to directory if savedir is specified
  if (!missing(savedir)) {
    message("Saving 'gravity.csv'...")
    data.table::fwrite(
      df, file.path(savedir, "gravity.csv"), showProgress = FALSE)
  }
  
  # Convert "value" column from class Integer64 if int64 == FALSE is specified
  if (int64 == FALSE) {
    message("Converting 'value' from 'Integer64'...")
    df[,"value"] <- as.numeric(as.character(df$value))
  }
  
  # Delete temp file
  unlink(t)
  
  # Report elapsed time
  endtime <- Sys.time()
  message(paste0(
    "Complete!\nTime Elapsed: ", 
    round(difftime(endtime, starttime, units = "mins")[[1]], 2), 
    " minutes"
  ))
  
  return(df)
}

#' UN to ISO 3166 Alpha-3 country code correspondence table
#' 
#' @format A list
#' @source 
#' <https://unstats.un.org/wiki/display/comtrade/Country+codes+in+ISO+3166>
"comtrade_names"
