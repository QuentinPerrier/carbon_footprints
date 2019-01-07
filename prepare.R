library(raster)
library(sf)
library(readxl)
library(tidyverse)
library(mapview) #To save sf plots

#unlink("tmp", recursive = TRUE)
dir.create("tmp", showWarnings = F)
dir.create("outputs", showWarnings = F)


# Get Ivanova data -----------------------------------------------------------------

Ivanova <- read_excel("inputs/Ivanova/erl_12_5_054013_supptables.xlsx", 
                                 sheet = "A6 Regional dataset", skip = 1) %>% 
  drop_na() %>% 
  gather(key = item, value = value, -c(1:4)) %>% 
  select(-contains("Collection")) %>% 
  set_names(c("NUTS_Iva", "Region_name", "NUTS_level", "item", "value")) %>% 
  arrange(NUTS_Iva)



# Get NUTS regions as shape file -----------------------------------------------------

wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"

nuts <- st_read("inputs/Eurostat/NUTS/ref-nuts-2013-01m.shp/NUTS_RG_01M_2013_3035_LEVL_3.shp/NUTS_RG_01M_2013_3035_LEVL_3.shp") %>% 
  st_transform(moll) %>% 
  mutate(NUTS_ID = as.character(NUTS_ID)) %>% 
  st_buffer(dist = 0) %>%  #pour éviter les problèmes de topologie
  st_crop(extent(c(-1.35*10^6, 3*10^6, 3.6*10^6, 1*10^7))) 

plot(nuts["FID"], main = "EU Nuts3")

EU_extent <- extent(nuts)
saveRDS(EU_extent, "outputs/EU_extent.rds")

# Match Ivanova NUTS with recent nuts -------------------------------------

EU_2006_2010 <- 
  read_excel("inputs/Eurostat/nuts_2006_2010.xls", skip = 1) %>% 
  filter(!is.na(X__1)) %>% 
  select(2:3) %>% 
  #correct for Italy
  mutate(`Code 2010` = case_when(
    `Code 2006` == "ITD" ~ "ITH", 
    `Code 2006` == "ITD5" ~"ITH5",
    `Code 2006` == "ITD59" ~"ITH59",
    str_detect(`Code 2006`, "^ITE") & is.na(`Code 2010`) ~ str_replace(`Code 2006`, pattern = "ITE", replacement = "ITI"),
    `Code 2006` == "FI13" ~"FI1D",
    `Code 2006` == "FI18" ~"FI1B",
    `Code 2006` == "FI1A" ~"FI1D",
    TRUE ~ `Code 2010`)
    ) %>% 
  rbind(c("FI18", "FI1C"))

EU_2010_2013 <- 
  read_excel("inputs/Eurostat/nuts_2010_2013.xls", 
             sheet = "NUTS2010-NUTS2013",
             skip = 1) %>% 
  select(2:3) 

EU_nuts_versions <- 
  inner_join(EU_2006_2010, EU_2010_2013, 
             by = "Code 2010", na_matches = "never") 

remove(EU_2006_2010, EU_2010_2013)


# We combine Ivanova with EU nuts, correcting for minor differences
nuts_Iva <- 
  Ivanova %>% select(1:3) %>% unique() %>% 
  mutate(`Code 2006` = NUTS_Iva) 

nuts_versions <- 
  left_join(nuts_Iva, EU_nuts_versions, by = "Code 2006")

# List and count issues
nuts_issues <- nuts_versions$NUTS_Iva[which(is.na(nuts_versions$`Code 2010`))] %>% unique() %>% sort() 
nuts_issues 
length(nuts_issues)

nuts_Iva$`Code 2006` <- ifelse(nuts_Iva$`Code 2006` %in% nuts_issues & str_detect(nuts_Iva$`Code 2006`, "0$"),
                               substring(nuts_Iva$`Code 2006`, 1, 3),
                               nuts_Iva$`Code 2006`)

nuts_versions <- 
  left_join(nuts_Iva, EU_nuts_versions, by = "Code 2006")

# List and count issues after first correction
nuts_issues <- nuts_versions$NUTS_Iva[which(is.na(nuts_versions$`Code 2010`))] %>% unique() %>% sort() 
nuts_issues 
length(nuts_issues)

remove(nuts_Iva, EU_nuts_versions)

# Combine nuts_Iva to nuts

find_Iva_region <- function(nuts_2013) {
  #' input= nuts 2013 at the level 3
  #' looking for the level 1 or 2 corresponding region (the level being defined in Ivanova)
  region <- nuts_versions$`Code 2013` %>% unique()
  df <- tibble(test = c(substring(nuts_2013, 1, 4), 
                        substring(nuts_2013, 1, 3), 
                        paste0(substring(nuts_2013, 1, 3), 0)))
  df$position <- NA
  for (i in 1:3) df$position[i] <- length(which(df$test[i] == region))
  df <- unique(df)
  if (length(which(df$position == 1)) != 1 ) {  #This would mean there is zero or several equivalent... not good!
    equiv <- NA}
  else { 
    equiv <- df$test[which(df$position == 1)]
  }
  return(equiv)
}

nuts$`Code 2013` <- NA
for (i in 1:nrow(nuts)) {
  try(nuts$`Code 2013`[i] <- find_Iva_region(nuts$NUTS_ID[i]))
}

nuts <- nuts %>% 
  select(CNTR_CODE, FID, NUTS_ID, NUTS_NAME, `Code 2013`) %>% 
  left_join(nuts_versions %>% select(NUTS_Iva, Region_name, `Code 2013`) %>% 
              drop_na(), 
            by = "Code 2013")

if (sum(is.na(nuts$`Code 2013`)) > 0) {
  message(paste0("warning: No equivalence was not found between nuts_2013 and Ivanova nuts for ", sum(is.na(nuts$`Code 2013`)), " nuts 3 levels"))
}


# Plot Ivanova regions

Ivanova_plot <- nuts %>% 
  group_by(NUTS_Iva, Region_name) %>% summarise() %>% 
  left_join(Ivanova %>% filter(item == "Household carbon footprint (kgCO2e/cap)"), 
            by = c("NUTS_Iva", "Region_name"))

png(filename = "outputs/Ivanova_plot.png", width = 500, height = 350)
plot(Ivanova_plot["value"], main = "Carbon footprint (kgCO2e/cap), Ivanova")
dev.off()

saveRDS(Ivanova, "outputs/Ivanova.rds")
saveRDS(nuts, "outputs/nuts.rds")


# Get revenues by NUTS region -----------------------------------------------------
gdp_cap <- read.csv("inputs/Eurostat/GDP/nama_10r_3gdp/nama_10r_3gdp_1_Data.csv") %>% 
  filter(UNIT == "Euro par habitant" & TIME == 2015) %>% 
  mutate(Value = replace(Value, Value == ":", NA),
         Value = as.numeric(as.character(Value)),
         GEO = as.character(GEO)) %>% 
  rename(nuts_2013 = GEO, spending = Value) 

#We complete the missing value for Luxembourg: 
#According to the world bank, the GDP per capita (Constant LCU) increased from 80.905 in 2014 to 81279 in 2015, or 0.46%
#And the value in the Eurostat data gdp_cap was 89.500 in 2014
gdp_cap[which(gdp_cap$nuts_2013 == "LU000"),"spending"] <-  89.500 * (1.0046)

#We correct the missing data for Ireland in 2015 (IE021 and IE025)
gdp_2014 <- read.csv("inputs/Eurostat/GDP/nama_10r_3gdp/nama_10r_3gdp_1_Data.csv") %>% 
  filter(UNIT == "Euro par habitant" & TIME %in% 2014) %>% 
  mutate(Value = replace(Value, Value == ":", NA),
         Value = as.numeric(as.character(Value)),
         GEO = as.character(GEO)) %>% 
  rename(nuts_2013 = GEO, spending = Value) %>% 
  filter(str_detect(nuts_2013, "^IE02")) 
#According to the World Bank, the GDP per capita (constant LCU) increased from 40 812 in 2014 to 50761 in 2015
#An average increase of 24.3%
gdp_inc <- 1.243

gdp_cap[which(gdp_cap$nuts_2013 == "IE021"), "spending"] <- gdp_2014 %>% filter(nuts_2013 == "IE021") %>% .$spending * gdp_inc
gdp_cap[which(gdp_cap$nuts_2013 == "IE025"), "spending"] <- gdp_2014 %>% filter(nuts_2013 == "IE025") %>% .$spending * gdp_inc

remove(gdp_2014, gdp_inc)

saveRDS(gdp_cap, "outputs/gdp_cap.rds")

#Plot revenues by region
revenues_plot <- left_join(nuts, gdp_cap, by = c("FID" = "nuts_2013"))

png(filename = "outputs/gdp_plot.png", width = 500, height = 350)
plot(revenues_plot["spending"], main = "Revenue per capita")
#Note that there are a few missing data in Eurostat (eg Dublin)
dev.off()


# Compute carbon footprint per revenue ------------------------------------------------

footprint <- Ivanova %>% 
  filter(item %in% c("Household carbon footprint (kgCO2e/cap)", 
                     "Net income (1000EUR/cap)")) %>% 
  mutate(item = str_replace_all(item, "Household carbon footprint \\(kgCO2e/cap\\)", "CF_cap"),
         item = str_replace_all(item, "Net income \\(1000EUR/cap\\)", "Income")) %>% 
  spread(key = item, value = value) %>% 
  mutate(CF = CF_cap / Income) %>% 
  select(NUTS_Iva, CF)



# Population raster -------------------------------------------------------

population <- 
  raster("inputs/JRC/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0.tif") %>%  
  crop(., EU_extent)

# population <- 
#   raster("inputs/JRC/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif") %>%  
#   crop(., EU_extent)

plot(population)
plot(st_union(nuts), add = T)

png(filename = "outputs/population.png", width = 1000, height = 1000)
plot(population, main = "population")
plot(st_union(nuts), add = T)
#Note that there are a few missing data in Eurostat (eg Dublin)
dev.off()

# Cities and towns raster -----------------------------------------------------------
# (GHS settlement grids from JRC)

cities_raster <- raster("inputs/JRC/GHS_SMOD_POP2015HDC_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015HDC_GLOBE_R2016A_54009_1k_v1_0.tif") %>% 
  crop(., nuts)
plot(cities_raster)
plot(st_union(nuts), add = T)

png(filename = "outputs/cities_plot.png", width = 500, height = 350)
plot(cities_raster)
plot(st_union(nuts), add = T)
dev.off()

towns_raster <- raster("inputs/JRC/GHS_SMOD_POP2015LDC_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015LDC_GLOBE_R2016A_54009_1k_v1_0.tif") %>% 
  crop(., nuts)
plot(towns_raster)
plot(st_union(nuts), add = T)
