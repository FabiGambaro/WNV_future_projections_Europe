############################################################
##   Download AROPE 2024 (NUTS2) and assign it to nutsM   ##
############################################################

library(giscoR)
library(terra)
library(sf)
library(eurostat)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

nutsM_sf <- readRDS("WNV/Processed_data/nutsM_sf.rds")
nuts2 <- gisco_get_nuts(year = "2024",epsg = "4326",resolution = "10",nuts_level = "2")
plot(sf::st_geometry(nuts2))

st_crs(nutsM_sf) == st_crs(nuts2)

nutsM_pts <- st_point_on_surface(nutsM_sf) 
joined <- st_join(nutsM_pts, nuts2["NUTS_ID"], join = st_within) #matching rows by a geometric relationship

nutsM_sf$NUTS2_code <- joined$NUTS_ID

no_match <- nutsM_sf %>% filter(is.na(NUTS2_code))
unique(no_match$CountryNam)
nutsM_sf$NUTS2_code[which(nutsM_sf$LocationCo == "EL411")] <- "EL41" #it was falling outside.

#only missing for Vatican, Andorra and UK, San Marino.
nutsM_nuts2_correspondance <- nutsM_sf[,c("LocationCo", "NUTS2_code")] %>% st_drop_geometry()

arope_nuts2 <- get_eurostat("ilc_peps11n", time_format = "num") #people at risk of poverty or social exclusion by NUTS2
saveRDS(arope_nuts2, "WNV/Processed_data/2026-10-31_ilc_peps11n_raw.rds")

#A - freauency = annual
#unit = PC = percentage
arope_nuts2_2024 <- arope_nuts2[which(arope_nuts2$TIME_PERIOD == 2024 & arope_nuts2$unit == "PC"),c("geo", "values")]
stopifnot(!any(duplicated(arope_nuts2_2024$geo)))
nutsM_AROPE <- left_join(nutsM_sf, arope_nuts2_2024, by = c("NUTS2_code" = "geo"))
sum(is.na(nutsM_AROPE$values))
stopifnot(nrow(nutsM_AROPE) == nrow(nutsM_sf))

#we dont have data for B&H, Anodrra, San Marino, Finland (FI19), VA, Kosovo, Liechtenstein UK (both bot part from EU)
to_check <- nutsM_AROPE[which(is.na(nutsM_AROPE$values)),]
unique(to_check$CountryNam)

saveRDS(nutsM_AROPE,"WNV/Processed_data/arope_nuts2_2024.rds")


