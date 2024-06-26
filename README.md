## Macroeconomics with R

The objective of this small project is building a Shiny app on Australian macroeconomic overview, using R. This Shiny app obtains historical economic data from Federal Reserve Economic Data (FRED) (https://fred.stlouisfed.org/) and create the corresponding charts.

**Note**: When running the Shiny app, the data will be downloaded from FRED. As the date range of data is set between July 1969 and the current date when this app is run, the data might be updated or new data might be available after each run.  

The following citations are for all datasets downloaded when running Shiny app on 30 May 2024:

Bank for International Settlements, Real Residential Property Prices for Australia [QAUR628BIS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/QAUR628BIS, May 28, 2024.

World Bank, Inflation, consumer prices for Australia [FPCPITOTLZGAUS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/FPCPITOTLZGAUS, May 28, 2024.

Organization for Economic Co-operation and Development, Consumer Price Indices (CPIs, HICPs), COICOP 1999: Consumer Price Index: Total for Australia [CPALTT01AUQ659N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/CPALTT01AUQ659N, May 28, 2024.

Organization for Economic Co-operation and Development, Financial Market: Share Prices for Australia [SPASTT01AUM661N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/SPASTT01AUM661N, May 21, 2024.

World Bank, Stock Market Capitalization to GDP for Australia [DDDM01AUA156NWDB], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DDDM01AUA156NWDB, May 29, 2024.

International Monetary Fund, Real Gross Domestic Product for Australia [NGDPRSAXDCAUQ], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/NGDPRSAXDCAUQ, May 28, 2024.

Organization for Economic Co-operation and Development, Interest Rates: Long-Term Government Bond Yields: 10-Year: Main (Including Benchmark) for Australia [IRLTLT01AUM156N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/IRLTLT01AUM156N, May 24, 2024.

Organization for Economic Co-operation and Development, Interest Rates: 3-Month or 90-Day Rates and Yields: Interbank Rates: Total for Australia [IR3TIB01AUM156N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/IR3TIB01AUM156N, May 28, 2024.

International Monetary Fund, General government gross debt for Australia [GGGDTAAUA188N], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/GGGDTAAUA188N, May 28, 2024.

Organization for Economic Co-operation and Development, Infra-Annual Labor Statistics: Unemployment Rate Total: 15 Years or over for Australia [LRUNTTTTAUM156S], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/LRUNTTTTAUM156S, May 28, 2024.

This Shiny App targets to cover the followings:

- :white_check_mark: Download historical datasets and render plots accordingly.  
- :white_large_square: Add forecast functionalities for economic indicators (to-do)
   
---

Steps to run the Shiny App:
1. Download RStudio Desktop
2. In RStudio, Go to File > New File > Shiny Web App...
![RStudio](https://github.com/DoThNg/MacroEco_with_R/blob/main/RStudio.png)

3. Name the Shiny App and select the directory where the app is located in (A file **app.R** will then be created).
4. The following codes will be used to run the Shiny app: [Link to app.R](https://github.com/DoThNg/MacroEco_with_R/blob/main/app.R)
   
**Note**: Install the library 'quantmod' in RStudio before running the app.

5. To run Shiny App, go to 'Run App' in RStudio.

The User Interface of Shiny app is as follows:

![Macroeconomics](https://github.com/DoThNg/MacroEco_with_R/blob/main/macroeconomics.png)
   
