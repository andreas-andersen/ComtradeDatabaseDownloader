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