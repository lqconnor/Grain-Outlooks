# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

pckgs <- c("tidyverse", "rnassqs", "stargazer", "Hmisc")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data Import ---------------------------------
feb_18 <- read_csv("../../Data/FarmIncome_WealthStatisticsData_November2018.csv")
wasde <- read_csv("../../Data/psd_grains_pulses.csv")
wasde_all <- read_csv("../../Data/psd_alldata.csv")
plt_acr <- read_csv("../../Data/crop_acres_state.csv")
prices <- read_csv("../../Data/prices.csv")

wasde_soy <- filter(wasde_all, Commodity_Description == "Oilseed, Soybean", 
                    Country_Name == "United States", Market_Year >= 2000)

soybean_all <- select(wasde_soy, Commodity_Description, Market_Year, Attribute_Description, Value) %>%
  spread(Attribute_Description, Value) %>%
  mutate(Use = `Total Supply` - `Ending Stocks`,
         stk_rto = `Ending Stocks`/Use) %>%
  select(Commodity_Description, Market_Year, Production, `Ending Stocks`, Use, stk_rto)
write_csv(soybean_all, "./Output/soybean_market.csv")

soybeans <- mutate(soybean_all, `Net Use` = (Use - Production)/1000) %>%
  select(Commodity_Description, Market_Year, Production, `Ending Stocks`, Use, `Net Use`)
write_csv(soybeans, "./Output/soybean_market_3.csv")

ending_stocks <- filter(wasde_soy, Attribute_Description == "Ending Stocks") %>%
  select(Commodity_Description, Market_Year, Value) %>%
  rename(end_stock = Value)

write_csv(ending_stocks, "./Output/ending_stocks.csv")

use_rto <- select(wasde_soy, Market_Year) %>%
  distinct() %>%
  mutate(use_rto = wasde_soy$Value[wasde_soy$Attribute_Description == "Ending Stocks"]/(wasde_soy$Value[wasde_soy$Attribute_Description == "Total Supply"] - wasde_soy$Value[wasde_soy$Attribute_Description == "Ending Stocks"])) %>%
  left_join(soybean_price, by = c( "Market_Year" = "Year"))
 
write_csv(use_rto, "./CSV/use_ratio.csv")