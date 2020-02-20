# save to binary
# this auxiliary script can be useful to store data
# in binary format that can be more easily read by the app
# when uploaded


library("tidyverse")
library("raster")
library("sp")


# vertex data
vertex_data = read_csv("data/vertex_data.csv")
saveRDS(vertex_data, "data/vertex_data.RDS")


# shapefiles
taz <- shapefile("./data/shapefiles/TAZs.shp")
taz@data$ID <- as.integer(taz@data$ID)
saveRDS(taz, "data/TAZs.RDS")


# splitlevels
splitlevels = scan("data/splitlevels.csv", sep=",")
saveRDS(splitlevels, "data/splitlevels.RDS")


# read probabilities
dir = "data/output_smooth_probs/"
files = list.files(dir)
nbins = length(splitlevels) - 1
d = length(files)
idx = files %>% 
  map_chr(~str_split(.x, "_")[[1]][2]) %>% 
  map_chr(~str_sub(.x, end=-5)) %>% 
  as.integer()
probs_cols = vector("list", length=d)
for (i in seq_along(files)) {
  file = files[i]
  pos = idx[i]
  x = scan(paste0(dir, file), sep=",")
  x = matrix(x, nrow=nbins, byrow=TRUE)
  probs_cols[[i]] = x
}
probs = reduce(probs_cols, cbind)
saveRDS(probs, "data/probs.RDS")
