library(raster)
library(sf)
library(mapview)
library(readxl)
library(tidyverse)

# source('prepare.R')
# source('urban_rural.R')

dir.create("outputs/tmp", showWarnings = F)
dir.create("outputs/tmp/commune_footprints", showWarnings = F)
dir.create("outputs/tmp/commune_population", showWarnings = F)

footprint_raster <- raster("outputs/footprint_raster.tif")

cities_footprint <- readRDS("outputs/cities_footprint.rds")
country_list <- cities_footprint$CNTR_CODE %>% unique() %>% sort() %>% as.character()

moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"
EU_extent <- readRDS("outputs/EU_extent.rds")

population <- 
  raster("inputs/JRC/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0.tif") %>%  
  crop(., EU_extent)

communes <- st_read("inputs/Eurostat/COMM_01M_2013_SH/data/COMM_RG_01M_2013.shp") %>% 
  filter(substring(COMM_ID, 1, 2) %in%  country_list) %>% 
  st_transform(moll) %>% 
  st_crop(EU_extent) %>% 
  mutate(COMM_ID = as.character(COMM_ID))

nuts <- st_read("inputs/Eurostat/NUTS/ref-nuts-2013-01m.shp/NUTS_RG_01M_2013_3035_LEVL_3.shp/NUTS_RG_01M_2013_3035_LEVL_3.shp") %>% 
  st_transform(moll) %>% 
  mutate(NUTS_ID = as.character(NUTS_ID)) %>% 
  st_buffer(dist = 0) %>%  #pour éviter les problèmes de topologie
  st_crop(extent(c(-1.35*10^6, 3*10^6, 3.6*10^6, 1*10^7))) 


# Functions ---------------------------------------------------------------

get_commune_footprint <- function(country_code) {
  
  print(paste("Compute footprint of communes in", country_code))
  
  country_communes <- communes %>% 
    filter(str_detect(COMM_ID, substring(country_code, 1, 2))) 
  
  #plot(country_communes["COMM_ID"])

  commune_footprint <- 
    raster::extract(x = footprint_raster,
                    y = country_communes,
                    fun = sum, df = T) %>% 
    mutate(COMM_ID = country_communes$COMM_ID) %>% 
    select(COMM_ID, footprint = layer)
  
  saveRDS(commune_footprint, file = paste0("outputs/tmp/commune_footprints/commune_footprint_", country_code, ".rds"))
  
  return(commune_footprint)
}


get_commune_pop <- function(country_code) {
  
  print(paste("Compute footprint of communes in", country_code))
  
  country_communes <- communes %>% 
    filter(str_detect(COMM_ID, substring(country_code, 1, 2))) 

  commune_pop <- 
    raster::extract(x = population,
                    y = country_communes,
                    fun = sum, df = T) %>% 
    mutate(COMM_ID = country_communes$COMM_ID) %>% 
    select(COMM_ID, population = GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0)
  
  saveRDS(commune_pop, file = paste0("outputs/tmp/commune_population/commune_pop_", country_code, ".rds"))
  
  return(commune_pop)
}

for (country_code in country_list) {
  start_time <- Sys.time()
  #try(get_commune_footprint(country_code))
  try(get_commune_pop(country_code))
  end_time <- Sys.time()
  print(end_time - start_time)
}

communes_pop <- 
  list.files("outputs/tmp/commune_population", full.names = T) %>% 
  map(readRDS) %>% 
  reduce(rbind) 

communes_footprint <- 
  list.files("outputs/tmp/commune_footprints", full.names = T) %>% 
  map(readRDS) %>% 
  reduce(rbind) %>% 
  distinct() %>% 
  full_join(communes, by = "COMM_ID") %>% 
  st_as_sf() %>% 
  select(COMM_ID, footprint) %>% 
  left_join(communes_pop, by = "COMM_ID")
saveRDS(communes_footprint, "outputs/communes_footprint.rds")

communes_footprint <- readRDS("outputs/communes_footprint.rds")

write.csv2(communes_footprint %>% st_set_geometry(NULL), "outputs/municipalities.csv", row.names = F)


communes_footprint %>% 
  #filter(str_detect(COMM_ID, "^FR")) %>% 
  mapview(zcol = "footprint")

communes_footprint %>% 
  ggplot()+
  geom_sf(aes(fill = footprint))







# Match LAU1 LAU2 ---------------------------------------------------------

# Get LAU Names
load_country_code <- function(sheet_name) {
  
  nuts_names <- nuts %>% st_set_geometry(NULL) %>% select(NUTS_ID, NUTS_NAME)
  
  df <- read_excel("inputs/Eurostat/LAU_Names/EU-28_LAU_2016_cleaned.xlsx", 
                   sheet_name, na = "n.a.") %>% 
    mutate(LAU1_NAT_CODE = as.character(LAU1_NAT_CODE),
           LAU2_NAT_CODE = as.character(LAU2_NAT_CODE)) %>% 
    select(-c(POP, AREA, CHANGE)) %>% 
    left_join(nuts_names, by = c("NUTS_3" = "NUTS_ID")) %>% 
    select(NUTS_3, NUTS_NAME, everything())
  
  return(df)
}

LAU_names <- map_dfr(excel_sheets("inputs/Eurostat/LAU_Names/EU-28_LAU_2016_cleaned.xlsx")[-c(1,30)],
                 load_country_code) %>% 
  mutate(country = str_sub(NUTS_3, 1, 2))

adapat_LAU_names <- function() {
  #' adapt LAU names for better matching with the 'guess_LAU2' function
  LAU_names <- 
    LAU_names %>%
    mutate(LAU2_NAT_CODE = case_when(
      str_detect(NUTS_3, "^DK") ~ paste0(LAU1_NAT_CODE,LAU2_NAT_CODE),
      str_detect(NUTS_3, "^HU") ~ str_sub(LAU2_NAT_CODE, 1, -2),
      str_detect(NUTS_3, "^PL") ~ str_sub(LAU2_NAT_CODE, 5),
      str_detect(NUTS_3, "^RO") ~ formatC(as.integer(LAU2_NAT_CODE), width = 6, flag = "0", format = "d"),
      str_detect(NUTS_3, "^SI") ~ formatC(as.integer(LAU2_NAT_CODE), width = 3, flag = "0", format = "d"),
      TRUE ~ LAU2_NAT_CODE))
}

LAU_names <- adapat_LAU_names()


# Match COMM_ID / LAU2

# For France
# communes_fr <- communes %>% 
#   filter(str_detect(COMM_ID, "^SI")) %>% 
#   mutate(country = str_sub(COMM_ID, 1, 2)) 

communes$country <- str_sub(communes$COMM_ID, 1, 2) 
  
#communes_fr <- communes %>% filter(country == "UK")

guess_LAU2 <- function(COMM_ID) {
  country_code <- str_sub(COMM_ID, 1,2)
  LAU2 <- case_when(
    country_code == "AT" ~ str_sub(COMM_ID, 3),
    country_code == "BE" ~ str_sub(COMM_ID, 4),
    country_code == "BG" ~ str_sub(COMM_ID, -5, -1),
    country_code == "CY" ~ str_sub(COMM_ID, 3),
    country_code == "CZ" ~ str_sub(COMM_ID, 7),
    country_code == "DE" ~ paste0(str_sub(COMM_ID, 3,4), str_sub(COMM_ID, 6,8),str_sub(COMM_ID, 12,14)),
    country_code == "DK" ~ str_sub(COMM_ID, 7),
    country_code == "EE" ~ str_sub(COMM_ID, -4, -1),
    #country_code == "EL" ~ str_sub(COMM_ID, -4, -1),
    country_code == "ES" ~ str_sub(COMM_ID, 5),
    country_code == "FI" ~ str_sub(COMM_ID, -3, -1),
    country_code == "FR" ~ paste0(str_sub(COMM_ID, 5,6), str_sub(COMM_ID, -3, -1)),
    country_code == "HU" ~ str_sub(COMM_ID, -4, -1),
    country_code == "IE" ~ paste0(str_sub(COMM_ID, 5,6), str_sub(COMM_ID, -3, -1)),
    country_code == "IT" ~ str_sub(COMM_ID, 6),
    country_code == "LT" ~ str_sub(COMM_ID, -4, -1),
    country_code == "LU" ~ str_sub(COMM_ID, 3, 6),
    country_code == "LV" ~ paste0(c("0"), str_sub(COMM_ID, 3)),
    country_code == "MT" ~ str_replace_all(COMM_ID, "MT1", "MT0"),
    #country_code == "PL" ~ str_sub(COMM_ID,4,-2),
    country_code == "PT"~ str_sub(COMM_ID, 4),
    country_code == "RO"~ str_sub(COMM_ID, -6, -1),
    country_code == "SI"~ str_sub(COMM_ID, -3, -1),
    country_code == "SK"~ str_sub(COMM_ID, 6),
    country_code == "UK"~ str_sub(COMM_ID, 3)
  )
  return(LAU2)
}

#guess_LAU2(communes_fr$COMM_ID[1])

communes$LAU2 <- map_chr(communes$COMM_ID, guess_LAU2)

LAU <- left_join(communes,
                 LAU_names,
                 by = c("LAU2" = "LAU2_NAT_CODE", c("country"))) %>% 
  rename(LAU2_NAT_CODE = LAU2, NAME_2 = NAME_2_LAT) %>% 
  select(country, NUTS_3, NUTS_NAME, COMM_ID, LAU1_NAT_CODE, LAU2_NAT_CODE, NAME_1, NAME_2)

saveRDS(LAU, "outputs/LAU.rds")

LAU_df <- LAU %>% st_set_geometry(NULL)


