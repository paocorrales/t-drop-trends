library(data.table)
library(lubridate)
library(rcdo)
library(here)

scenarios <- c("historical", "ssp585")
percentiles <- c(90, 92.5, 95, 97.5, 98, 99, 99.5, 99.9)

purrr::map(percentiles, function(p) {
  message(p)

purrr::map(scenarios, function(s) {
  
  message(s)
  
  outfile <- here(paste0("data/series/serie_freq_deltap", p, "_", s, "_100km.nc"))
  
  # if (file.exists(outfile)) {
  #   return(outfile)
  # }
  
  file_list <- Sys.glob(here(paste0("data/temp/100km/deltap", p, "_year_", models_in_gadi, "_", s, "*")))
  
  cdo_ensmean(file_list) |> 
    cdo_execute(outfile, options = "-L -O")
  
})

})

##############
##############
##############

models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

cdo_ensmean(Sys.glob(here("data/cmip/eke/eke_season_*_historical*"))) |>
  cdo_execute("data/cmip/eke/eke_season_all_historical_300.nc", options = "-L")

cdo_ensmean(Sys.glob(here(paste0("data/cmip/eke/eke_year_", models_in_gadi, "_hist*")))) |>
  cdo_execute("data/cmip/eke/eke_year_all_historical_300.nc", options = "-L")

cdo_ensmean(Sys.glob(here(paste0("data/cmip/100km/deltap97.5_year_", models_in_gadi, "_ssp*")))) |>
  cdo_execute("data/cmip/series/serie_freq_deltap97.5_ssp585_100km.nc", options = "-L")
# 