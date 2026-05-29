## code to prepare `country_meta_data` dataset goes here

library(tidyverse)
library(countrycode)
library(glue)
library(jsonlite)
library(httr)
library(rvest)




democracy_data <- coldwaR::load_data() |>
  mutate(
    continent = case_when(

      country_code %in% c(
        "DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD","COM","COG","COD",
        "CIV","DJI","EGY","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB","KEN",
        "LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NAM","NER","NGA",
        "RWA","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN","TZA","TGO","TUN","UGA",
        "ZMB","ZWE","ZAR"
      ) ~ "Africa",

      # North America (incl. Caribbean)
      country_code %in% c(
        "CAN","USA","MEX","BHS","BRB","CUB","DMA","DOM","GRD","HTI","JAM",
        "KNA","LCA","VCT","TTO","ATG"
      ) ~ "North America",

      # Central America
      country_code %in% c(
        "BLZ","CRI","SLV","GTM","HND","NIC","PAN"
      ) ~ "Central America",

      # South America
      country_code %in% c(
        "ARG","BOL","BRA","CHL","COL","ECU","GUY","PRY","PER","SUR","URY","VEN"
      ) ~ "South America",

      country_code %in% c(
        "AFG","ARM","AZE","BHR","BGD","BTN","BRN","KHM","CHN","CYP","GEO","IND","IDN",
        "IRN","IRQ","ISR","JPN","JOR","KAZ","KWT","KGZ","LAO","LBN","MYS","MDV","MNG",
        "MMR","NPL","PRK","OMN","PAK","PSE","PHL","QAT","SAU","SGP","KOR","LKA","SYR",
        "TWN","TJK","THA","TLS","TUR","TKM","ARE","UZB","VNM","YEM"
      ) ~ "Asia",

      country_code %in% c(
        "ALB","AND","AUT","BLR","BEL","BIH","BGR","HRV","CYP","CZE","DNK","EST","FIN",
        "FRA","DEU","GRC","HUN","ISL","IRL","ITA","LVA","LIE","LTU","LUX","MLT","MDA",
        "MCO","MNE","NLD","MKD","NOR","POL","PRT","ROU","RUS","SMR","SRB","SVK","SVN",
        "ESP","SWE","CHE","UKR","GBR","VAT","GER","ROM"
      ) ~ "Europe",

      country_code %in% c(
        "AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG","WSM","SLB","TON","TUV",
        "VUT","NUR"
      ) ~ "Oceania",

      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(continent))



populations <- read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population")

pop_data <- populations |>
  html_element("table.wikitable") |>
  html_table(fill = TRUE) |>
  select(
    Country = 1,
    Population = 2
  ) |>
  mutate(
    Country    = trimws(Country),
    Population = as.numeric(gsub("[^0-9]", "", Population))  # strip anything that isn't a digit
  ) |>
  filter(!is.na(Population))



pop_data <- pop_data |>
  mutate(
    iso3c = countrycode(Country,
                        origin = "country.name",
                        destination = "iso3c"
    ))

democracy_data_2020 <- democracy_data |>
  filter(year == 2020) |>
  mutate(
    iso3c = countrycode(country_name,
                        origin = "country.name",
                        destination = "iso3c"
    ))

# Now join based on iso3c country code
country_data <- pop_data |>
  inner_join(democracy_data_2020,
             by = "iso3c")



# Obtain 2020 GDP in USD for inputted country
get_gdp_2020 <- function(country, key = my_key)
{

  # Store url to access API with country and key queries
  url <- glue("https://api.api-ninjas.com/v1/gdp?year=2020;country={country};X-Api-Key={key}")

  # Temporarily store JSON data safely
  my_data <- safely(fromJSON)(url)

  # Check if JSON was successfully obtained
  if (is.null(my_data$result) |
      length(my_data$result) == 0) {
    return(NA)
  } else {
    return(my_data$result$gdp_nominal)
  }
}



gdp_data <- read.csv("country_gdps.csv")
gdp_data <- gdp_data |>
  select(-X)

# Combine GDP with aggregate 2020 data
country_meta_data <- country_data |>
  inner_join(gdp_data,
             by = "iso3c",
             relationship = "many-to-many")



country_meta_data <- country_meta_data |>
  filter(!is.na(regime_category_index)) |>
  mutate(gov_type = case_when(
    is_communist == TRUE ~ "Communist",
    is_democracy == TRUE & is_monarchy == TRUE ~ "Democratic Monarchy",
    is_democracy == TRUE & is_monarchy == FALSE ~ "Pure Democracy",
    is_democracy == FALSE & is_monarchy == TRUE ~ "Pure Monarchy",
    TRUE ~ "Other"),
    gov_type = factor(gov_type, levels = c("Pure Democracy",
                                           "Pure Monarchy",
                                           "Democratic Monarchy",
                                           "Communist",
                                           "Other"
    )),
    regime_category = case_when(
      regime_category_index == 0 ~ "Parliamentary democracy",
      regime_category_index == 1 ~ "Mixed democratic",
      regime_category_index == 2 ~ "Presidential democracy",
      regime_category_index == 3 ~ "Civilian dictatorship",
      regime_category_index == 4 ~ "Military dictatorship",
      regime_category_index == 5 ~ "Royal dictatorship")
  )


usethis::use_data(country_meta_data, overwrite = TRUE)
