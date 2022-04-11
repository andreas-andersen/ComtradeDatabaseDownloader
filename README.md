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

Specify data frequency and periods to download and assign dataframe to an object 
(following example will download Comtrade data containing the 3 monthly trade 
records from January 2020 to March 2020).

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

Unfortunately, this seems to be happening randomly, and the current version of
the package is not programmed to handle this automatically. Instead, it will
save the progress in a temp file and you should be able to simply re-enter your 
previous `get_comtrade()` command and continue on from where the process
stopped.

You should estimate approx. 2.5 minutes to download and process one period
(depending on your internet connection and processing power). In other words, 
downloading a full year should take about 30 minutes.
