get_comtrade_file <- function(freq, year, month, token) {
  if (missing(freq)) stop("Data frequency was not specified")
  if (missing(year)) stop("Year was not specified")
  if (missing(token)) stop("Comtrade token was not specified")
  if (freq == "monthly" & missing(month)) stop("'monthly' frequency specified but calendar month is missing")
  if (!(freq %in% c("annual", "monthly"))) stop("Invalid frequency specified \n(Specify either 'annual' or 'monthly')")
  
  base_url <- "http://comtrade.un.org/api/get/bulk/C/"
  print(paste(freq, year, month, token))
}
