library(sf)

# Clean up ArcGIS Pro service area analysis layer

lrt_sa <- 
  st_read("lrtServiceAreas.geojson")

stations <- unlist(strsplit(lrt_sa$Name, " :"))[seq(1, 46, 2)]

lrt_sa$Name <- stations

lrt_sa <- 
  lrt_sa %>%
  select(Name, geometry)

st_write(lrt_sa, "lrtServiceAreas_clean.geojson", append = FALSE)
