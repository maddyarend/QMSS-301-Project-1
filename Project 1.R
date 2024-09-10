###############
# Maddy Arend
# Project 1
###############

# load packages
library(sf)
library(dplyr)
library(tmap)
library(writexl)

# read in data 
neighborhoods <- read_sf("Neighborhoods.shp")
zipcodes <- read_sf("zip_codes.shp")
grocery_stores <- read_sf("Grocery_Stores_in_City_of_Detroit_Public_View.shp")
crime <- read_sf("RMS_Crime_Incidents.shp")

# filter crime data for 2021 incidents only
crime <- crime%>%
  filter(year==2021)

# check CRS of data
st_crs(neighborhoods)
st_crs(zipcodes)
st_crs(grocery_stores)
st_crs(crime)

# check if data is projected
st_is_longlat(neighborhoods)
st_is_longlat(zipcodes)
st_is_longlat(grocery_stores)
st_is_longlat(crime)

# all of the data is unprojected

# project the data
neighborhoods <- st_transform(neighborhoods, crs = 5623)
zipcodes <- st_transform(zipcodes, crs = 5623)
grocery_stores <- st_transform(grocery_stores, crs=5623)
crime <- st_transform(crime, crs=5623)

# check CRS of data
st_crs(neighborhoods)
st_crs(zipcodes)
st_crs(grocery_stores)
st_crs(crime)

# verify data is projected
st_is_longlat(neighborhoods)
st_is_longlat(zipcodes)
st_is_longlat(grocery_stores)
st_is_longlat(crime)

# all of the data is projected

# task 1

# 1

# find number of crimes in each neighborhood
neigh_crime <- st_within(crime, neighborhoods, sparse = FALSE)

# sum up crimes per neighborhood
crime_count <- apply(X=neigh_crime, MARGIN=2,FUN=sum)

# bind crime counts to neighborhood data
neigh_crime1 <- cbind(neighborhoods, crime_count)

# create safe and unsafe neighborhood objects by filtering
safe_neighborhoods <- neigh_crime1%>%
  filter(crime_count < 100)

unsafe_neighborhoods <- neigh_crime1%>%
  filter(crime_count > 1200)

# set tmap mode to plot
tmap_mode("plot")

# create legend labels
legend_labels_neigh <- c("< 100 Crimes", "> 1200 Crimes")

# create legend colors
legend_colors_neigh <- c("green", "red")


# map of neighborhoods colored by safety 
tm_shape(neighborhoods) + tm_polygons(alpha = 0.4) +
  tm_shape(safe_neighborhoods) + tm_polygons (col = "green", alpha = 0.8) +
  tm_shape(unsafe_neighborhoods) + tm_polygons(col = "red", alpha = 0.8) +
  tm_add_legend(type = c("fill"), 
                labels = legend_labels_neigh, 
                col = legend_colors_neigh,
                title = "Crime Legend") +
  tm_layout(main.title = "Map of Crime Incidents in 2021 by Detroit Neighborhood",
            main.title.position = "center",
            main.title.size = 1,
            frame = FALSE)

# 2

# find number of crimes in each zipcode
zipcode_crime <-st_within(crime, zipcodes, sparse = FALSE)

# sum up crimes per zip code
crime_count_zip <- apply(X=zipcode_crime, MARGIN=2,FUN=sum)

# bind crime counts to zipcode data
zipcode_crime1 <- cbind(zipcodes, crime_count_zip)

# create safe and unsafe zipcode objects by filtering
safe_zipcodes <- zipcode_crime1%>%
  filter(crime_count_zip < 1000)

unsafe_zipcodes <- zipcode_crime1%>%
  filter(crime_count_zip > 5000)

# create legend labels 
legend_labels_zip <- c("< 1000 Crimes", "> 2000 Crimes")

# create legend colors
legend_colors_zip <- c("green", "red")

# create map of zipcodes colored by crime incidents
tm_shape(zipcodes) + tm_polygons(alpha = 0.4) +
  tm_shape(safe_zipcodes) + tm_polygons(col="green", alpha = 0.8) +
  tm_shape(unsafe_zipcodes) + tm_polygons(col="red", alpha= 0.8) +
  tm_shape(zipcodes) + tm_text('zipcode', size = 0.6) +
  tm_add_legend(type = c("fill"), 
                labels = legend_labels_zip, 
                col = legend_colors_zip,
                title = "Crime Legend") +
  tm_layout(main.title = "Map of Crime Incidents By Detroit Zipcode",
            main.title.position = "center",
            main.title.size = 1,
            frame = FALSE)

# task 2

# 1

# check units of measurement of CRS
st_crs(grocery_stores)$units
st_crs(neighborhoods)$units
st_crs(zipcodes)$units
st_crs(crime)$units

# all data is measured in feet

# create buffer zones around grocery stores
gro_0.5m_buff <- st_buffer(grocery_stores, dist = 2640)
gro_1m_buff <- st_buffer(grocery_stores, dist = 5280)

# create legend labels
legend_labels_gro <- c("0.5 mile radius", "1 mile radius")

# create legend colors
legend_colors_gro <- c("red", "blue")
  
# create map of neighborhoods and grocery stores with buffers
tm_shape(neighborhoods) + tm_polygons(alpha = 0.4) +
  tm_shape(grocery_stores) + tm_dots(col = "black", size = 0.1) +
  tm_shape(gro_0.5m_buff) + tm_borders(col = "red") + tm_fill(col = "red", alpha = 0.2) +
  tm_shape(gro_1m_buff) + tm_borders(col = "blue") + tm_fill(col="blue", alpha= 0.2)+
  tm_add_legend(type = c("fill"), 
                labels = legend_labels_gro, 
                col = legend_colors_gro,
                title = "Distance Legend") +
  tm_layout(main.title = "Map of Grocery Stores in Detroit Neighborhoods",
             main.title.position = "center",
             main.title.size = 1,
             frame = FALSE)
  
# 2

# create dataset containing only robbery crimes
robberies <- crime%>%
  filter(offense_de =="ROBBERY")

# find number of robberies within half a mile of the grocery stores
robberies_within_0.5m <-st_within(robberies, gro_0.5m_buff, sparse = FALSE)

# find number of robberies within a mile of the grocery stores
robberies_within_1m <- st_within(robberies, gro_1m_buff, sparse = FALSE)

# find counts of robberies within half and one mile of the grocery stores
robbery_count_0.5m <- apply(X=robberies_within_0.5m, MARGIN=2,FUN=sum)
robbery_count_1m <-apply(X=robberies_within_1m, MARGIN=2,FUN=sum)

# bind robbery counts to grocery store data
grocery_stores_robberies <- cbind(grocery_stores, robbery_count_0.5m, robbery_count_1m)

# create data set with only four columns
grocery_stores_robberies1 <- grocery_stores_robberies%>%
  select(Store_Name, Address, robbery_count_0.5m, robbery_count_1m)

# get descriptive statistics for each of the robbery variables
summary(grocery_stores_robberies1$robbery_count_0.5m)
summary(grocery_stores_robberies1$robbery_count_1m)


# remove geometry column
grocery_stores_robberies1 <- st_drop_geometry(grocery_stores_robberies1)

# export data to excel
write_xlsx(grocery_stores_robberies1, 'detroit grocery stores and robberies.xlsx')

  


