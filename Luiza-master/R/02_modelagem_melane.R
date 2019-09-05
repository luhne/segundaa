
devtools::install_github("Model-R/modelr_pkg")

library(dplyr)
library(raster)
library(modleR)

registros <- read.table("./registros/insetos.csv", h = T, sep = ",", stringsAsFactors = F)

head(registros)
str(registros)
table(registros$sp)

species <- sort(unique(registros$sp))
species

# ausencias <- read.table("./registros/ausencias.csv", h = T, sep = ";", stringsAsFactors = F)
# ausencias <- ausencias[,-1]
# head(ausencias)

# clean(
#   occurrences = ausencias,
#   predictors = env,
#   clean_dupl = TRUE,
#   clean_nas = TRUE,
#   clean_uni = TRUE
# )


env = brick("./env/pca_5km.tif")
env = env[[1:6]]

BR <- rgdal::readOGR("./shapes/brasil.shp")
crs(BR) <- crs(env)

env <- mask(crop(env,BR),BR)

plot(env)

species[7]
occs <- filter(registros, sp == species[7]) %>% dplyr::select(lon, lat)
head(occs)

result_folder <- "./resultados2"

sdmdata_1sp <- setup_sdmdata(
  species_name = species[7],
  occurrences = occs,
  #real_absences = ausencias,
  predictors = env,
  models_dir = result_folder,
  clean_dupl = T,
  clean_nas = T,
  clean_uni = T,
  partition_type = "crossvalidation",
  cv_partitions = 5,
  cv_n = 1,
  seed = 512,
  buffer_type = "max",
  dist_min = 0.1,
  n_back = 1000, #nrow(occs) * 100
  plot_sdmdata = T
)

do_many(species_name = species[7],
        sdmdata = sdmdata_1sp,
        occurrences = occs,
        predictors = env,
        plot_sdmdata = T,
        models_dir = result_folder,
        write_png = T,
        write_bin_cut = T,
        bioclim = T,
        maxent = T,
        rf = T,
        svmk = T,
        svme = F,
        brt = F,
        domain = F,
        glm = F,
        maxnet = F,
        mahal = F,
        equalize = T)


final_model(species_name = species[7],
            algorithms = NULL, #if null it will take all the in-disk algorithms
            models_dir = result_folder,
            select_partitions = TRUE,
            select_par = "TSS",
            select_par_val = 0.7,
            which_models = c("raw_mean", "bin_consensus"),
            consensus_level = 0.5,
            uncertainty = T,
            overwrite = T)

ensemble_model(species_name = species[7],
               occurrences = occs,
               which_models = c("raw_mean", "bin_consensus"),
               write_png = T,
               models_dir = result_folder,
               consensus = T,
               consensus_level = 0.5)




