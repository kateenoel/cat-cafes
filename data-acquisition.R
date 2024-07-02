# ******************************************************************************
# 
# Program name: data-acquisition.R
#
# Program author: Kate Noel
#
# Purpose: Scrape data from list of cat cafes in North America and create data
#           frame for future shiny
#
# Requirements: R libraries: rvest
#                            tidyverse  
#
# Inputs: https://www.meowaround.com/continents/north-america
#
# Outputs:
#
# Cautions: N/A
#
# Notes: Address data missing for several cafes, using ggmap as workaround
#        Amenity data missing for only Clowder of Cats Cat Lounge, excluding all
#           data for this cafe
#
# ******************************************************************************


# load libraries ----
library(rvest)
library(tidyverse)


# set up ----
url_base <- "https://www.meowaround.com/continents/north-america"

pagination <- read_html(url_base) %>% html_nodes(".pagination") %>% html_text() 
n_page <- as.numeric(sub("(.*)([^ ] )", "", sub("\\ Next.*", "", pagination)))

url_general  <- "https://www.meowaround.com/continents/north-america?page="
urls  <- lapply(paste0(url_general, 1:n_page), str_c)

webpage <- urls %>% map(read_html)


# extract cafe names and descriptions ---- 
cafe_names <- webpage %>% map(html_elements, css=".card-row-title") %>% map(html_text) %>% map(str_squish) %>% unlist()
#cafe_address <- webpage %>% map(html_elements, css=".detail-contact-address") %>% map(html_text) %>% map(str_squish) %>% unlist()
cafe_desc <- webpage %>% map(html_elements, css=".hidden-xs") %>% map(html_text) %>% unlist() 

df_cafe <- data.frame(cafe_names, cafe_desc) %>% filter(cafe_names != "Clowder of Cats Cat Lounge")


# extract cafe amenity details ---- 
list_amenities <- webpage %>% map(html_elements, css=".detail-amenities") %>% map(html_children)

amenities <- c()
for (i  in 1:n_page) {
  for (j in 1:length(list_amenities[[i]])) {
    amenities <- c(amenities, list_amenities[[i]][j])
  } 
}

amenity_names <- c("adoptions", "coffee", "tea", "wifi", "yoga", "movie_nights", "reservations",
            "wine", "beer", "food", "cat_themed_merch")

n_cafe <- length(df_cafe$cafe_names)
n_amenity <- length(amenity_names)

m <- matrix(nrow=n_cafe, ncol=n_amenity)

for (i in 1:n_cafe) {
  for (j in 1:n_amenity) {
    m[i,j] <- toupper(substr(amenities[[11*(i-1)+j]], 12, 12))
  }
}

colnames(m) <- amenity_names
df_amenity <- data.frame(m)


# get cafe addresses ----


# create final data frame ----
df <- cbind(df_cafe, df_amenity)












