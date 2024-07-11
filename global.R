# import libraries
library(tidyr)
library(rgdal)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)
library(DT)
library(ggiraph)
library(plotly)
library(shinycssloaders)


# read data
total <- read.csv("total_income.csv") # total income data
urban_lga <- read.csv("urban_lga_list.csv") # a list of urban lgas

# rename first column
colnames(total)[1] <- "LGA"

# former "Moreland" is now called "Merri-bek"
total$LGA[total$LGA == "Moreland"] <- "Merri-bek"

# create a dataframe for all LGAs and their corresponding type (urban/regional)
colnames(urban_lga)[1] <- "urban_lga"
urban_lga <- urban_lga$urban_lga
urban_lga <- sapply(urban_lga, 
                    function(x) strsplit(x, "of ")[[1]][2])

isUrbanLGA <- function(x){
  if(x %in% urban_lga) "urban" else "regional"
}

lga <- unique(total$LGA)
lga_type <- data.frame(lga)
lga_type$type <- sapply(lga_type$lga, isUrbanLGA)


# gather data, now data has three columns
total <- total %>%
  gather(key = "year", value = "value", 2:26)

# arrange year column and add stat column
total$stat <- sapply(total$year, function(x) str_sub(x, end = -8))
total$year <- sapply(total$year, function(x) gsub("_", "-", str_sub(x, start = -7)))


# replace abbreviation
replaceAbbr <- function(x){
  switch(
    x,
    "E" = "number of earners",
    "MA" = "median age of earners",
    "SUM" = "sum of total income",
    "MED" = "median of total income",
    "MEAN" = "mean of total income"
  )
}

total$stat <- sapply(total$stat, replaceAbbr)

# convert "value" to numeric data type
total$value <- sapply(total$value, 
                     function(x) as.numeric(gsub(",","",x)))

# handling vic lga shapefile
viclga <- readOGR(dsn="spatial_data", "LGA_POLYGON")

# group unincorporated lga
# all unincorporated LGAs are indicated by the (UNINC) in LGA_NAME column
groupUninc <- function(lga){
  if (grepl("(UNINC)",lga)) "UNINCORPORATED VIC" else lga
}
viclga$LGA_NAME <- sapply(viclga$LGA_NAME, groupUninc)


# convert total total data to upper case for merging with shapefile
total <- total %>%
  mutate(LGA = toupper(LGA))

# merge two tables
# lga <- merge(viclga, total, by.x = "LGA_NAME", by.y = "LGA", duplicateGeoms=TRUE)

# reverse replaceAbbr for save use
saveAbbr <- function(x){
  switch(
    x,
    "number of earners" = "E",
    "median age of earners" = "MA",
    "sum of total income" = "SUM",
    "median of total income" = "MED",
    "mean of total income" = "MEAN"
  )
}

# lists of choices
years <- unique(total$year)
stat_types <- unique(total$stat)
lgas <- unique(total$LGA)
ranks <- c(5, 10, 20, 30, 50, "All")


