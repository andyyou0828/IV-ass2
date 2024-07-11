library(tidyr)
library(rgdal)
library(ggplot2)
library(leaflet)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(magick)

setwd("D:/IV/assignment2")
# read data
total <- read.csv("total_income.csv")
urban_lga <- read.csv("urban_lga_list.csv")

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

# add type column, an LGA belongs to either urban or regional
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

total$stat <- sapply(total$stat, replaceAbbr)

# convert "value" to numeric data type
total$value <- sapply(total$value, 
                      function(x) as.numeric(gsub(",","",x)))

# handling vic lga shapefile
viclga <- readOGR(dsn="spatial_data", "LGA_POLYGON")

# group unincorporated lga
groupUninc <- function(lga){
  if (grepl("(UNINC)",lga)) "UNINCORPORATED VIC" else lga
}
viclga$LGA_NAME <- sapply(viclga$LGA_NAME, groupUninc)


# convert total total data to upper case for merging with shapefile
total <- total %>%
  mutate(LGA = toupper(LGA))

# convert lga_type$lga to upper case for merging
lga_type$lga <- sapply(lga_type$lga, toupper)

# join two table 
total_lga <- merge(lga_type, total, by.x = "lga", by.y = "LGA")

process <- function(df, x, y, z){ # x:lga type, y:stat type, z:year
  # select data
  temp <- df %>%
    filter(type == x & stat == y & year == z) %>%
    select(lga, value)
  
  # merge data with spatial dataframe
  res <- merge(viclga, temp, by.x = "LGA_NAME", by.y = "lga",
               all.x = FALSE, all.y = TRUE, duplicateGeoms=TRUE)
  
  # draw and save map
  tmp <- spplot(res, "value",
                par.settings = list(axis.line = list(col = "transparent")),
                col.regions = brewer.pal("PuBuGn", n = 5),
                cuts = 4,
                main = paste(y, "during", z, "for", x, "LGAs"))
  png(filename = paste0("trends/", x,"_",saveAbbr(y),"_",z,".png"), width = 600, height = 400)
  print(tmp)
  dev.off()
  
}

# urban_lga_e <- total_lga %>%
#   process("urban", "number of earners", "2015-16")

all_types <-  c("urban", "regional")
all_stats <- unique(total$stat)
all_years <- unique(total$year)
all_choices <- list(all_types, all_stats, all_years)
all_choices <- expand.grid(all_choices) # all permutation of choices

draw_all_map <- function(x){
  process(total_lga, x[1],x[2],x[3])
}

# apply to the dataframe
apply(all_choices, 1, draw_all_map)

# expand_grid can geneate all permutations for multiple vectors
# types_stats now is a dataframe with each row representing an unique (LGA type, stat type) pair
types_stats <- list(all_types, all_stats)
types_stats <- expand.grid(types_stats)

# use magick to group maps into layers
# see https://rstudio-pubs-static.s3.amazonaws.com/315157_73b802e0532c4ea3839f98afc0378ca1.html

# read the data with different year in sequence and store in a list/vector
make_gif <- function(df){ 
  layers <- sapply(all_years, 
                   function(x) image_read(paste0("trends/", df[1],"_",saveAbbr(df[2]),"_",x,".png")))
  
  # generate the gif animation by sticking the images together with frame per second = 1 (speed)
  anim <- image_animate(image_join(layers), fps = 1, dispose = "previous")
  
  # save the gif animation
  image_write(anim, paste0("animations/",df[1],"_",saveAbbr(df[2]),".gif")) 
}

# for each unique (LGA type, stat type) pair, apply make_gif function
apply(types_stats, 1, make_gif)
