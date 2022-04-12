# ComtradeDatabaseDownloader

Download and wrangle global trade data from the 
[UN Comtrade Database](https://comtrade.un.org/) and output it to a dataframe 
object suited for Gravity Model estimation.

### Prerequisites:

- Access to a network with premium site license subscription to the Comtrade
Database.
- Creation of an [API token](https://comtrade.un.org/api/swagger/ui/index\#/Auth)

## Quick installation guide:

``` r
# install.packages("devtools")
devtools::install_github("andreas-andersen/ComtradeDatabaseDownloader")
```

Specify data frequency and periods to download and assign data.frame to an 
object (following example will download Comtrade data containing the 3 monthly 
trade records from January 2020 to March 2020).

``` r
df <- get_comtrade(
    freq = "monthly",
    startyear = 2020,
    startmonth = 1,
    endyear = 2020,
    endmonth = 3,
    token = "YOURTOKENHERE"
)
```

![Output data.frame](img/illustration_dataframe.png?raw=true "Output data.frame")

## Additional features:

- Fast reading/writing of `.csv` files using `data.table::fread()`. (Requires 
the `bit64` package to store large integer values)
- Automatic saving of progress in a temp file in case of connection errors.

## Notes:

Sometimes, the Comtrade Database may reject your request for a file. 

![Error](img/illustration_error.png?raw=true "Error")

If this happens, first make sure your Comtrade token is correct. Secondly, 
Comtrade seems to randomly reject requests, especially if many files have been 
downloaded in a row. The current version of the package will attempt to 
reconnect twice in case of a failed request. If this fails, try re-entering 
the same `get_comtrade()` command later. _As long as this is done 
during the same `R`-session_, the package will continue on from a temp file 
which has saved the progress. 

You should estimate approx. 2.5 minutes to download and process one period
(depending on your internet connection and processing power). In other words, 
downloading a full year of monthly data should take about 30 minutes.
