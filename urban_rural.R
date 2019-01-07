library(sf)
library(raster)
library(readxl)
library(tidyverse)

source("prepare.R")
dir.create("outputs/tmp", showWarnings = F)
dir.create("outputs/tmp/regional_footprint", showWarnings = F)
dir.create("outputs/tmp/urban_shape", showWarnings = F)


# URBAN VS RURAL ----------------------------------------------------------
# Decompose for each nuts regions, using population and wealth


# Get consumption patterns (urban vs rural) -------------------------------

country_codes <- read_excel("inputs/Codes2.xlsx") %>% 
  mutate(ISO2 = str_replace_all(ISO2, "GB", "UK"))

consumption_patterns <- read_csv("inputs/Eurostat/COICOP/hbs_str_t226/hbs_str_t226_1_Data.csv") %>% 
  mutate(GEO = str_remove_all(GEO, " \\(until 1990 former territory of the FRG\\)" )) %>% 
  left_join(., country_codes %>% rename(GEO = Name) %>% dplyr::select(GEO, ISO2), 
            by = "GEO") %>% 
  dplyr::select(TIME, GEO, ISO2, everything()) %>% 
  mutate(Value = replace(Value, Value == ":", NA),
         Value = as.numeric(Value) / 10) %>%  # to get percentage instead of per 1000
  rename(share = Value) %>% 
  dplyr::select(-UNIT) %>% 
  mutate(DEG_URB = str_replace_all(DEG_URB, "Towns and suburbs", "Towns"))

#We choose the best year (eg for Denmark, there is no data for 2015)
# test <- consumption_patterns %>% group_by(TIME, GEO) %>% summarise(na = sum(!is.na(share))) %>% 
#   group_by(GEO) %>% arrange(desc(na), desc(TIME)) 


# Total expenditure of urban vs rural --------------------------------------------

# We compute total expenditure, region by region, using Ivanova regions


region_list <- Ivanova %>% select(NUTS_Iva, NUTS_level) %>% unique() %>% arrange(NUTS_Iva)

get_spending <- function(region, urban_degree) {
  
  #' The urbanisation raster is "cities raster" for example
  #' Regions are those of Ivanova
  
  nuts <- readRDS("outputs/nuts.rds")
  gdp_cap <- readRDS("outputs/gdp_cap.rds") %>% 
    filter(nuts_2013 != "LVZZZ")
  
  nuts_lvl <- region_list$NUTS_level[which(region_list$NUTS_Iva == region)]
  region_2013 <- nuts_versions %>% filter(NUTS_Iva == region) %>% .$`Code 2013`
  
  if (length(region_2013 > 1)) region_2013 <- paste(region_2013, collapse = "|")
  
  if (nuts_lvl == 1) region_shape <- nuts %>% filter(str_detect(FID, substr(region_2013, 1, 3))) 
  if (nuts_lvl == 2) region_shape <- nuts %>% filter(str_detect(FID, region_2013)) 
  if (!(nuts_lvl %in% c(1,2))) stop("Error identifying the nuts level of the region")
  
  if (nrow(region_shape) == 0) {
    #' test that region is not empty
    stop("Empty shape for region ", region)
  }
  
  #plot(region_shape["FID"])
  
  if (urban_degree == "total") {
    urban_shape <- region_shape
  } else {
    if (urban_degree == "city") urbanisation_raster <- cities_raster
    if (urban_degree == "town") urbanisation_raster <- towns_raster
    
    urban_raster <- 
      crop(urbanisation_raster, region_shape) %>%
      mask(., region_shape)    
    
    urban_shape <- 
      if (max(values(urban_raster), na.rm = T) > 0) {
        urban_raster %>% 
          rasterToPolygons(fun=function(x){x>0}) %>% 
          st_as_sf() %>% 
          set_names("city_num", "geometry") %>% 
          st_intersection(., region_shape) %>% 
          # on regroupe
          group_by(NUTS_ID) %>% 
          summarise() 
      } else {
        NULL
      }
    
  }
  
  
  if (!is.null(urban_shape)) {
    
    plot(region_shape["FID"] %>% st_union(), main = paste0("region ", region,", ", urban_degree))
    plot(urban_shape["NUTS_ID"], add = T)
    
    
    pop <- raster::extract(x = population, 
                           y = urban_shape, 
                           fun = sum, na.rm = T, df =T) %>% 
      mutate(nuts_ID = urban_shape$NUTS_ID) %>% 
      select(3,2) %>% 
      set_names("nuts_2013", "pop") %>% 
      mutate(pop = pop / 10^6)
    
    total_spending <- left_join(pop, gdp_cap, by = "nuts_2013") %>% 
      mutate(total_spending = pop * spending) %>% 
      .$total_spending %>% sum()
    
    pop_total <- sum(pop$pop)
    
    urban_shape %>% 
      mutate(region = region, urban_degree = urban_degree) %>% 
      select(region, urban_degree) %>% 
      saveRDS(., paste0("outputs/tmp/urban_shape/urban_shape_", region, "_", urban_degree, ".rds"))
    
    
  } else {
    pop_total <- 0
    total_spending <- 0
  }
  
  out <- tribble(
    ~region, ~urban_degree, ~population, ~total_spending,
    region, urban_degree, pop_total, total_spending
  )
  
  return(out)
  
}

get_detailed_footprint <- function(region) {
  
  #' regions are those of Ivanova
  
  region_data <- tibble(region = region,
                        urban_degree = c("city", "town", "total")) %>% 
    pmap(get_spending) %>% 
    bind_rows()
  
  spending <- region_data %>%
    select(-population) %>% 
    #Towns include cities: we have to make the substraction
    spread(key = urban_degree, value = total_spending) %>% 
    mutate(town = town - city, 
           rural = total - city - town) %>% 
    select(-total) %>% 
    gather(key = DEG_URB, value = total_spending, -region) %>% 
    mutate(DEG_URB = case_when(
      DEG_URB == "city" ~"Cities",
      DEG_URB == "town" ~ "Towns",
      DEG_URB == "rural" ~ "Rural areas" 
    )) 
  
  region_pop <- region_data %>% 
    select(-total_spending) %>% 
    spread(key = urban_degree, value = population) %>% 
    mutate(town = town - city, 
           rural = total - city - town) %>% 
    select(-total) %>% 
    gather(key = DEG_URB, value = population, -region) %>% 
    mutate(DEG_URB = case_when(
      DEG_URB == "city" ~"Cities",
      DEG_URB == "town" ~ "Towns",
      DEG_URB == "rural" ~ "Rural areas" 
    )) 
  
  #saveRDS(spending, paste0("outputs/tmp/spending_region_", region, ".rds"))
  
  # Disagregate total spending 
  
  #Find best year for consumption patterns
  
  best_consumption_year <- 
    consumption_patterns %>% 
    filter(ISO2 == substring(region, 1, 2) & TIME %in% c(2005, 2010, 2015)) %>% 
    group_by(TIME, DEG_URB) %>% 
    summarise(value = sum(is.na(share))) %>% 
    group_by(DEG_URB) %>% 
    arrange(value, desc(TIME)) %>% slice(1) %>% select(-value)
  
  best_consumption_patterns <- 
    consumption_patterns %>% 
    filter(ISO2 == substring(region, 1, 2)) %>% 
    right_join(best_consumption_year, by = c("TIME", "DEG_URB"))
  
  spending_disag <- 
    best_consumption_patterns %>% 
    left_join(., spending, by = "DEG_URB") %>% 
    mutate(spending = share * total_spending / 100) %>% 
    #We aggregate more, to find a common aggregation level with Ivanova
    mutate(category = case_when(
      str_detect(COICOP, "Food and non-alcoholic beverages|Alcoholic beverages, tobacco and narcotics") ~ "Food",
      str_detect(COICOP, "Clothing and footwear") ~ "Clothing",
      str_detect(COICOP, "Housing, water, electricity, gas and other fuels|Furnishings, household equipment and routine household maintenance") ~ "Shelter",
      str_detect(COICOP, "Transport") ~ "Mobility",
      str_detect(COICOP, "Health|Communications|Recreation and culture|Education|Restaurants and hotels") ~ "Services",
      str_detect(COICOP, "Miscellaneous goods and services") ~ "Man production",
      TRUE ~ "Missing data"
    )) %>% 
    group_by(DEG_URB, category) %>% 
    summarise(spending = sum(spending)) %>% 
    group_by(category) %>% 
    mutate(spending_share = spending / sum(spending)) %>% 
    select(category, DEG_URB, spending_share)
  
  
  # Aggregate total regional footprint 
  
  aggregate_footprint <- Ivanova %>% 
    filter(str_detect(NUTS_Iva, region)) %>% 
    filter(str_detect(item, "^direct|indirect")) %>% 
    # read_excel("inputs/Ivanova/erl_12_5_054013_supptables.xlsx", 
    #                       sheet = "A6 Regional dataset", skip = 1) %>% 
    # dplyr::select(c(1,2, starts_with("direct"), starts_with("indirect"))) %>% 
    # gather(key = item, value = value, -c(1,2)) %>% 
    # set_names(c("NUTS2_code", "NUTS2_name", "item", "value")) %>% 
    mutate(type = ifelse(str_detect(item, "^direct"), "direct", "indirect"),
           item = str_remove_all(item, "direct kgCO2e/cap |indirect kgCO2e/cap "),
           category = case_when(
             str_detect(item, "^FOOD") ~ "Food",
             str_detect(item, "^CLOTHING") ~  "Clothing",
             str_detect(item, "^MOBILITY") ~ "Mobility",
             str_detect(item, "^SERVICES") ~ "Services",
             str_detect(item, "^MAN PROD") ~ "Man production",
             str_detect(item, "SHELTER") ~ "Shelter",
             TRUE ~ "error?")) %>% 
    group_by(NUTS_Iva, category) %>% 
    summarise(CO2_per_cap = sum(value)) %>% 
    # Now we have per capita footprint
    # We multiply by population to get total footprint
    mutate(pop = max(region_data$population),
           total_footprint = CO2_per_cap * pop) %>% 
    ungroup() %>% 
    select(category, total_footprint)
  
  split_footprint <- 
    aggregate_footprint %>% 
    left_join(., spending_disag, by = "category") %>% 
    mutate(footprint = total_footprint * spending_share) %>% 
    select(-c(total_footprint, spending_share)) %>% 
    ungroup() %>% 
    # We divide the total footprint by population in each urban degree to compute footprint per cap
    left_join(., 
              region_pop, 
              by = "DEG_URB") %>% 
    mutate(footprint_cap = footprint / population) %>% 
    select(region, DEG_URB, population, everything())
  
  if (sum(is.na(split_footprint$footprint_cap)) == nrow(split_footprint)) warning(paste("Only NAs in region", region))
  
  saveRDS(split_footprint, paste0('outputs/tmp/regional_footprint/detailed_footprint_cap_', region,'.rds'))
  
  invisible(split_footprint)
  
}



# Compute, read and save results ---------------------------------------------------


for (i in 1:nrow(region_list)) {
region <- region_list[[i,1]]  # code of Ivanova
  region_2013 <- nuts_versions %>% filter(NUTS_Iva == region) %>% .$`Code 2013`
  
  if (!file.exists(paste0("outputs/tmp/regional_footprint/detailed_footprint_cap_", region, ".rds"))) {
    message("Analyzing region ", region, " (", region_2013, ")")
    try(get_detailed_footprint(region))
  }
}


detailed_footprint <- list.files(path = "outputs/tmp/regional_footprint/", pattern = "^detailed", full.names = T) %>% 
  map(readRDS) %>% bind_rows() 

setdiff(unique(Ivanova$NUTS_Iva), unique(detailed_footprint$region))

footprint_deg_urb <- 
  detailed_footprint %>% 
  group_by(region, DEG_URB, population) %>% 
  summarise(footprint = sum(footprint)) %>% 
  mutate(footprint_cap = footprint/population) %>% 
  ungroup()

saveRDS(footprint_deg_urb, "outputs/footprint_deg_urb.rds")

map_footprint <- left_join(nuts, nuts_versions) %>% 
  left_join(footprint_deg_urb, by = c("NUTS_Iva" = "region")) %>% 
  filter(DEG_URB == "Rural areas") #Rural ares, Cities or Towns

plot(map_footprint["footprint_cap"], main = "Footprint per nuts region")



#Graph
order <- footprint_deg_urb %>% 
  filter(DEG_URB == "Rural areas") %>% 
  ungroup() %>% 
  arrange(footprint_cap) %>% 
  select(region) %>% 
  mutate(order = 1:nrow(.))

footprint_deg_urb %>%
  left_join(., order, by = "region") %>% 
  arrange(order) %>% 
  ggplot(aes(x = reorder(region, order), y = footprint_cap, colour = DEG_URB)) + geom_point()


# Heatmap -----------------------------------------------------------------

#Get the urban shapes (cities, towns and rural areas)
urban_shapes <-
  list.files("outputs/tmp/urban_shape", full.names = T) %>% 
  map(function(x) {readRDS(x) %>% group_by(region, urban_degree) %>% summarise(.) }) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(region = as.character(region),
         urban_degree = as.character(urban_degree)) 

get_region_spatial_diff <- function(reg) {
  #' get the difference spatial borders of cities, towns and rural areas
  shapes <- urban_shapes %>% filter(region == reg)
  city <- shapes %>% filter(urban_degree == "city")
  
  if (nrow(city) == 0) {
    town <- shapes %>% filter(urban_degree == "town") 
  } else {
    town <- st_difference(shapes %>% filter(urban_degree == "town"), 
                          city) %>% 
      select(region, urban_degree = urban_degree)
  }

  if (nrow(town) == 0) {
    rural <- shapes %>% filter(urban_degree == "total") %>% 
      mutate(urban_degree = "rural areas")
  } else {
    rural <- st_difference(shapes %>% filter(urban_degree == "total"), 
                           shapes %>% filter(urban_degree == "town")) %>% 
      select(region, urban_degree = urban_degree) %>% 
      mutate(urban_degree = "rural areas")
  }
  
  out <- rbind(city, town, rural) 
  #plot(out)
  return(out)
}

urban_shape_diff <- unique(urban_shapes$region) %>% 
  map(get_region_spatial_diff) %>% 
  do.call("rbind", .) %>% 
  left_join(footprint_deg_urb %>% 
              rename(urban_degree = DEG_URB) %>% 
              mutate(urban_degree = case_when(
                urban_degree == "Cities" ~ "city",
                urban_degree == "Towns" ~ "town",
                urban_degree == "Rural areas" ~ "rural areas"
              )),
            by = c("region", "urban_degree"))

#plot(urban_shape_diff %>% filter(str_detect(region, "^FR10")))

plot(urban_shape_diff["urban_degree"])

saveRDS(urban_shape_diff, "outputs/urban_shape_diff.rds")
urban_shape_diff <- readRDS("outputs/urban_shape_diff.rds")

#Combine population raster with urban shapes and CF, to get final footprint raster
footprint_cap_raster <-  rasterize(urban_shape_diff, 
                                   population, 
                                   field = "footprint_cap")
saveRDS(footprint_cap_raster, "outputs/footprint_cap_raster.rds")

footprint_raster <- population * footprint_cap_raster
saveRDS(footprint_raster, "outputs/footprint_raster.rds")
writeRaster(footprint_raster, filename = "outputs/footprint_raster.tif", 
            format = "GTiff", overwrite = TRUE)

# plot(footprint_raster)
# plot(st_union(nuts), add = T)
# 
# png(filename = "outputs/footprint_raster.png", width = 10000, height = 5000)
# plot(footprint_raster)
# plot(st_union(nuts), add = T)
# dev.off()