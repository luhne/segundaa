# Without vignette
remotes::install_github("Model-R/modelr_pkg", build = TRUE)

# With vignette
remotes::install_github("Model-R/modelr_pkg", build = TRUE,
                        build_opts = c("--no-resave-data", "--no-manual"))

devtools::install_github("Model-R/modelr_pkg")

#install.packages('modleR')
#install.packages('rJava')
#install.packages('raster')
#install.packages('rlang')
#install.packages('dplyr')

library(rlang)
library(rJava)


library(dplyr)
library(raster)
library(modleR)

registros <- read.table("./registros/registros.csv", h = T, sep = ",", stringsAsFactors = F)

head(registros)
str(registros)

species <- sort(unique(registros$sp))
species

env = brick("./env/pca_5km.tif")
env = env[[1:6]]

table(registros$sp)

reg.clean = c()
for (i in species) {
  temp  = modleR::clean(registros[registros$sp == i,2:3], predictors = env, clean_dupl = T, clean_nas = T, clean_uni = T)
  temp$sp = i
  reg.clean = rbind(reg.clean, temp)
}

table(reg.clean$sp)


species[7]
occs <- filter(reg.clean, sp == species[7]) %>% dplyr::select(lon, lat)
head(occs)

result_folder <- "./testes/results"

sdmdata_1sp <- setup_sdmdata(
  species_name = species[7],
  occurrences = occs,
  predictors = env,
  models_dir = result_folder,
  partition_type = "crossvalidation",
  cv_partitions = 4,
  cv_n = 1,
  seed = 512,
  buffer_type = "mean",
  dist_min = 0.1,
  plot_sdmdata = T,
  n_back = nrow(occs) * 100
)

# do_any(species_name = species[7],
#        sdmdata = sdmdata_1sp,
#        occurrences = occs,
#        algo = "maxent",
#        seed = 512,
#        predictors = env,
#        plot_sdmdata = T,
#        models_dir = result_folder,
#        write_png = T,
#        write_bin_cut = T,
#        equalize = T)

do_many(species_name = species[7],
        sdmdata = sdmdata_1sp,
        occurrences = occs,
        predictors = env,
        plot_sdmdata = T,
        models_dir = result_folder,
        write_png = T,
        write_bin_cut = F,
        bioclim = T,
        brt = T,
        maxent = T,
        rf = T,
        svmk = T,
        svme = T,
        domain = F,
        glm = F,
        maxnet = F,
        mahal = F,
        equalize = T)

args(final_model)

final_model(species_name = species[7],
            algorithms = NULL, #if null it will take all the in-disk algorithms
            models_dir = result_folder,
            select_partitions = TRUE,
            select_par = "TSS",
            select_par_val = 0.4,
            which_models = c("raw_mean", "bin_consensus"),
            consensus_level = 0.5,
            uncertainty = T,
            overwrite = T)

final.folder <- list.files(test_folder,
                           recursive = T,
                           pattern = "final_models",
                           include.dirs = T,
                           full.names = T)
final.folder
final_mods <- list.files(final.folder, full.names = T, pattern = "raw_mean.+tif$", recursive = T)
final_mods
#install.packages('stringr')
library(raster)
final_models <- stack(final_mods)
library(stringr)
library(dplyr)
names(final_models) <- str_split(names(final_models), "_", simplify = T) %>%
  data.frame() %>% dplyr::select(3) %>% dplyr::pull()
plot(final_models)

args(ensemble_model)
ens <- ensemble_model(species[1],
                      occurrences = occs,
                      which_models = "raw_mean",
                      models_dir = test_folder)


ensemble_files <-  list.files(paste0(test_folder,"/", species[1],"/present/ensemble"),
                              recursive = T,
                              pattern = "raw_mean.+tif$",
                              full.names = T)

ensemble_files
ens_mod <- raster::stack(ensemble_files)
names(ens_mod) <- c("mean", "median", "range", "st.dev")
raster::plot(ens_mod)

args(do_many)
args(setup_sdmdata)
especies <- unique(registros$sp)

for (i in 1:length(especies)) {
  especie <- especies[i]
  occs <- registros[registros$sp == especie, c("lon", "lat")]
  setup_sdmdata(
    species_name = especie,
    models_dir = "~/alphuss/forlooptest",
    occurrences = occs,
    predictors = example_vars,
    buffer_type = "distance",
    dist_buf = 4,
    write_buffer = T,
    clean_dupl = T,
    clean_nas = T,
    clean_uni = T,
    plot_sdmdata = T,
    n_back = 1000,
    partition_type = "bootstrap",
    boot_n = 5,
    boot_proportion = 0.7
  )
}
# recovers the list of sdmdata
sdmdata_files <- list.files("~/alphuss/forlooptest", recursive = T, pattern = "sdmdata.txt", full.names = T)
sdm_list <- purrr::map(.x = sdmdata_files, ~read.table(.))
for (i in 1:length(especies)) {
  especie <- especies[i]
  occs <- registros[registros$sp == especie, c("lon", "lat")]
  sdmdata <- sdm_list[[i]]
  do_many(species_name = especie,
          sdmdata = sdmdata,
          predictors = example_vars,
          models_dir = "~/alphuss/forlooptest",
          write_png = T,
          bioclim = T,
          maxent = T,
          maxnet = F,
          rf = T,
          svmk = T,
          svme = T,
          brt = T,
          glm = T,
          domain = F,
          mahal = F,
          equalize = T,
          plot_sdmdata = T,
          write_bin_cut = T)
}
for (i in 1:length(especies)) {
  especie <- especies[i]
  occs <- registros[registros$sp == especie, c("lon", "lat")]
  final_model(species_name = especie,
              select_partitions = TRUE,
              select_par = "TSS",
              select_par_val = 0.5,
              consensus_level = 0.3,
              models_dir = "~/alphuss/forlooptest",
              which_models = c("raw_mean", "bin_consensus"),
              uncertainty = T,
              overwrite = T)
}
for (i in 1:length(especies)) {
  especie <- especies[i]
  occs <- registros[registros$sp == especie, c("lon", "lat")]
  ensemble_model(species_name = especie,
                 occurrences = occs,
                 which_models = "bin_consensus",
                 write_png = T,
                 models_dir = "~/alphuss/forlooptest")
}


args(final_model)
args(do_many)
args(setup_sdmdata)


library(purrr)
sdmdata_list <- registros %>% split(.$sp) %>%
  purrr::map(~ setup_sdmdata(species_name = unique(.$sp),
                             occurrences = .[, c("lon", "lat")],
                             partition_type = "bootstrap",
                             boot_n = 5,
                             boot_proportion = 0.7,
                             clean_nas = T,
                             clean_dupl = T,
                             clean_uni = T,
                             buffer_type = "distance",
                             dist_buf = 4,
                             predictors = example_vars,
                             models_dir = "~/alphuss/temp_purrr",
                             n_back = (dim(ocorrencias)[1]) * 100,
                             write_png = T))
registros %>% split(.$sp) %>%
  purrr::map2(.x = .,
              .y = sdmdata_list,
              ~ do_many(species_name = unique(.x$sp),
                        sdmdata = .y,
                        occurrences = .x[, c("lon", "lat")],
                        predictors = example_vars,
                        models_dir = "~/alphuss/temp_purrr",
                        bioclim = T,
                        maxent = T,
                        rf = T,
                        svme = T,
                        svmk = T,
                        domain = F,
                        glm = T,
                        mahal = F,
                        brt = T,
                        equalize = T))

registros %>%
  split(.$sp) %>%
  purrr::map(~ final_model(species_name = unique(.$sp),
                           select_partitions = T,
                           select_par = "TSS",
                           select_par_val = 0.3,
                           consensus_level = 0.3,
                           models_dir = "~/alphuss/temp_purrr",
                           which_models = "raw_mean",
                           overwrite=TRUE)

registros %>%
  split(.$sp) %>%
  purrr::map(~ ensemble_model(species_name = unique(.$sp),
                              occurrences = .[, c("lon", "lat")],
                              which_models = "raw_mean",
                              write_png = T,
                              models_dir = "~/alphus/temp_purrr"
  ))

library(parallel)
