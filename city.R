library(raster)
library(sf)
library(ggmap)
library(tidyverse)
library(googleway)


# source('prepare.R')
# source('urban_rural.R')

dir.create("outputs/tmp/city_shape", showWarnings = F)
dir.create("outputs/tmp/location", showWarnings = F)
dir.create("outputs/tmp/footprint", showWarnings = F)

footprint_cities_urb <- readRDS("outputs/footprint_deg_urb.rds") %>% 
  filter(DEG_URB == "Cities") 



# Find city location ------------------------------------------------------
 
cities_num <- values(cities_raster) %>% unique() %>% sort()
cities_num <- cities_num[cities_num>0]


getCityFootprint <- function(city_num) {

  message("Evaluating city ", city_num)
  
  city_raster <- cities_raster
  city_raster[city_raster != city_num] <- NA
  city_raster <- trim(city_raster)
  
  # Get city shape 
  
  city_shape <- 
    rasterToPolygons(city_raster, dissolve = T) %>% 
    st_as_sf() %>% 
    set_names("city_num", "geometry") 
  
  saveRDS(city_shape, paste0("outputs/tmp/city_shape/city_", city_num, "_shape.rds"))
  
  plot(city_shape, main = "City shape")
  
  
  # City characteristics 
  # Population & emissions
  
  if (nrow(st_intersection(nuts, city_shape)) == 0 ) {
    warning("This city ", city_num, " is outside our map of Europe")
  } else {
    
    city_df <- 
      st_intersection(nuts, city_shape) %>% 
      raster::extract(x = population, y = ., fun = sum, na.rm = T, sp = T) %>% 
      st_as_sf() %>% 
      rename(population = GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0) %>% 
      left_join(.,
                footprint_cities_urb %>% select(-population), 
                by = c("NUTS_Iva" = "region")) %>% 
      mutate(footprint = population * footprint_cap) %>% 
      st_set_geometry(NULL) %>%
      group_by(CNTR_CODE, city_num) %>% 
      summarise(population = sum(population), footprint = sum(footprint)) %>% 
      mutate(footprint_cap = footprint / population)
    
    saveRDS(city_df, paste0("outputs/tmp/footprint/city_", city_num, ".rds"))
  }
}

#getCityFootprint(10)  

for (i in seq_along(cities_num)) {
  city_num <- cities_num[i]
  if (!file.exists(paste0("outputs/tmp/footprint/city_", city_num, ".rds"))) {
    #message("Analysing city ", city_num)
    try(getCityFootprint(city_num))
  }
}

# Compile results ---------------------------------------------------------


cities_footprint <- 
  list.files(path = "outputs/tmp/footprint", pattern = "^city", full.names = T) %>% 
  map(~ readRDS(.)) %>% 
  bind_rows() %>% 
  arrange(desc(population)) %>%
  #Countries not in the EU
  filter(!CNTR_CODE %in% c("TR", "IS", "NO", "SE", "CH", "MK", "ME")) %>% 
  #Countries not considered by Ivanova
  filter(!(CNTR_CODE %in% c("NL", "HR"))) 

saveRDS(cities_footprint, "outputs/cities_footprint.rds") 


city_shapes <- 
  list.files(path = "outputs/tmp/city_shape", full.names = T) %>% 
  map(readRDS) %>% 
  reduce(rbind)
saveRDS(city_shapes, file = "outputs/city_shapes.rds")


# Find city names ---------------------------------------------------------

get_centroid <- function(file) {
  centroid <- readRDS(file) %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    st_point() 
}

#Get the list of city numbers and their centroids
city_geom <- cities_footprint$city_num %>% 
  paste0("outputs/tmp/city_shape/city_", ., "_shape.rds") %>%
  map(get_centroid) %>% 
  st_sfc(crs = moll) %>% 
  st_transform(crs = wgs) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(city_number = cities_footprint$city_num) %>% 
  select(city_number, lat = Y, lon = X)

#library(googleway)

key <- "AIzaSyC7hqQoHp9Ant-nWa_qG3A1cK990snOhcs"

#Get city name with Google request
get_city_name <- function(city_number, lat, lon) {
  if (!file.exists(paste0("outputs/tmp/location/city_", city_number, ".rds"))) {
    address <- google_reverse_geocode(location = c(lat,lon),
                                      result_type = c("street_address"),
                                      location_type = "rooftop",
                                      key = key,
                                      simplify = T)
    saveRDS(address, paste0("outputs/tmp/location/city_", city_number, ".rds"))
    print(paste("Location of city", city_number, "retrieved"))
    Sys.sleep(3)
  }
}

city_geom[1:31,] %>% pwalk(get_city_name)

#Extract results from google request
extract_address <- function(city_number) {
  file <- paste0("outputs/tmp/location/city_", city_number, ".rds")
  google_results <- readRDS(file)

  if (google_results$status == "ZERO_RESULTS") {
    stop(paste0("No result for city ", city_number))
  }
  address_components <- google_results$results$address_components[[1]]
  
  outputs <- address_components %>% 
    mutate(city_number) %>% 
    select(city_number, everything())
  
  outputs
}

city_names <- city_geom[1:5,1]$city_number %>% 
  map(extract_address) %>% 
  bind_rows() %>% 
  rowwise() %>% 
  mutate(types = paste(types, collapse = "|")) %>% 
  filter(str_detect(types, c("country|area_level_1|area_level_2"))) %>% 
  select(-short_name) %>% 
  group_by(city_number) %>% 
  summarise(name = paste(long_name, collapse = ", "))
  spread(types, long_name) %>% 
  unite(col = -city_number, sep = ", ")


  


