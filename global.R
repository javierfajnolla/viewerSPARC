library(tidyverse)
library(raster)
library(rgdal)

# Load data to display

# Zonation solution
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed.tif") %>%
#   aggregate(fact = 10, fun = mean)
# proj4string(solution) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(solution, "data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed_agg10.tif")
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed_agg10.tif")

## Correct solution
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.rank.compressed.tif") %>%
#   aggregate(fact = 10, fun = mean)
# proj4string(solution) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(solution, "data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.rank.compressed_agg10.tif")
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.rank.compressed_agg10.tif")
# solution <- raster("data_Africa/solutions/AT_Birds_gf8570_BLP1_noPA.CAZ_EBLP100.rank.compressed.tif")
solution <- raster("data_Africa/solutions/AT_Birds_gf8570_BLP1_noPA.CAZ_EBLP100.rank.compressed_x2.tif")
# Test with different resolutions
solution_2 <- solution %>%
  aggregate(fact = 2, fun = "mean")
solution_3 <- solution %>%
  aggregate(fact = 3, fun = "mean")

mask <- solution / solution

# Carbon storage
# carbon_stor <- raster("data/solutions/total_carbon.tif") %>%
#   aggregate(fact = 10, fun = sum) %>%
#   crop(mask) %>%
#   `*` (mask)   # There is data in pixels that are not in the zonation solution
# proj4string(carbon_stor) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(carbon_stor, "data/solutions/total_carbon_agg10.tif")
# carbon_stor <- raster("data/solutions/total_carbon_agg10.tif")
# carbon_stor <- raster("data_Africa/solutions/AT_total_carbon_2_5m.tif")
carbon_stor <- raster("data_Africa/solutions/AT_total_carbon_2_5m_x2.tif")
# Test with different resolutions
# carbon_stor <- carbon_stor %>% 
#   aggregate(fact = 2, fun = "sum")

## Max
max_carbon <- carbon_stor %>% values %>% sum(na.rm = T)

################## Table of carbon offset for each percentage
# tbl <- tibble(perc_pixels = seq(0.01, 1, by = 0.01))
# tbl$cutoff_sol <- NA
# 
# # Sorted priority values (to select x% with higher priority)
# sol_values_sort <- solution %>% values %>% sort %>% rev
# 
# # Cutoff in solution to retain each percentage of the study area
# for (j in 1:nrow(tbl)){
#   tbl$cutoff_sol[j] <- sol_values_sort[(length(sol_values_sort) * tbl$perc_pixels[j]) %>% round(0)]
# }
# 
# # Carbon storage saved by protecting each percentage of the study area
# tbl$carbon_strg <- NA
# for(i in tbl$perc_pixels){#[is.na(tbl$carbon_strg)]){
#   sel_thr <- tbl %>%
#     filter(perc_pixels == i) %>%
#     pull(cutoff_sol)
# 
#   sol_rcl <- solution %>%
#     reclassify(tibble(from = c(0, sel_thr),
#                       to = c(sel_thr, max(values(solution), na.rm = T)),
#                       becomes = c(NA, 1)))
# 
#   tbl$carbon_strg[tbl$perc_pixels == i] <- (sol_rcl * carbon_stor) %>% values %>% sum(na.rm = T)
# }
# 
# tbl <- tbl %>%
#   mutate(prop_carbon_strg = carbon_strg / max_carbon,
#          prop_carbon_strg_rounded = round(prop_carbon_strg, 2))

### DO NOT RUN THIS PART -from here to saveRDS- , it was just testing... but it doesn't seem to solve the problems, although I might want to go back to this later.
## Fill values of the carbon storage slider that did not correspond to any particular threshold of the solution (including values between the max carbon thr and 100)
# perc_pixels_carbon <- seq(0.01, 1, by = 0.01)
# tbl_carboff <- setdiff(perc_pixels_carbon, tbl$prop_carbon_strg_rounded) %>%
#   as_tibble %>% setNames("prop_carbon_strg_rounded")
# new_tbl_carboff <- tibble()
# for (k in 1:nrow(tbl_carboff)){
#   tbl_temp <- tbl %>%
#     filter(abs(prop_carbon_strg_rounded - tbl_carboff$prop_carbon_strg_rounded[k]) == min(abs(prop_carbon_strg_rounded - tbl_carboff$prop_carbon_strg_rounded[k]))) %>%
#     mutate(prop_carbon_strg_rounded = tbl_carboff$prop_carbon_strg_rounded[k])
#   new_tbl_carboff <- new_tbl_carboff %>% bind_rows(tbl_temp)
# }
#
## Combine both tables
# tbl <- tbl %>% bind_rows(new_tbl_carboff)

# saveRDS(tbl, "data_Africa/tables/tbl_Africa.rds")
# write_csv(tbl, "data_Africa/tables/tbl_Africa.csv")
# saveRDS(tbl, "data_Africa/tables/tbl_Africa_x2.rds")
# write_csv(tbl, "data_Africa/tables/tbl_Africa_x2.csv")
  
### Load precalculated table to speed up
# tbl <- readRDS("data/tables/tbl.rds")
# tbl <- read_csv("data/tables/tbl.csv")
# tbl <- readRDS("data_Africa/tables/tbl_Africa.rds")
tbl <- readRDS("data_Africa/tables/tbl_Africa_x2.rds")

## Add continuous ID to tbl to calculate how many rows of difference is required 
## by any change, so we can tag small differences
# tbl$id <- 1:nrow(tbl)

tbl <- tbl %>% 
  mutate(perc_pixels = 100 * perc_pixels,
         prop_carbon_strg = 100 * prop_carbon_strg) %>% 
  mutate(prop_carbon_strg = round(prop_carbon_strg, digits = 0))

tbl <- tbl %>% 
  bind_rows(tibble(perc_pixels = 0, cutoff_sol = 0, carbon_strg = 0, prop_carbon_strg = 0, prop_carbon_strg_rounded = 0))


# tbl_sol <- tbl %>% 
#   dplyr::select(perc_pixels, prop_carbon_strg) %>% 
#   mutate(perc_pixels = 100 * perc_pixels,
#          prop_carbon_strg = 100 * prop_carbon_strg)
# 
# tbl_crb <- tbl_sol %>% 
#   dplyr::select(prop_carbon_strg, perc_pixels) %>% 
#   mutate(prop_carbon_strg = round(prop_carbon_strg, digits = 0))



# Color Palettes for leaflet
pal <- colorNumeric(c("yellow", "orange", "red"), values(carbon_stor),
                   na.color = "transparent")