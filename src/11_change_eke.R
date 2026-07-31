file_list <- Sys.glob(here::here("data/temp/100km/deltap97.5_year*JJA*"))

purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "{var}_year_{model}_{scenario}_{member}_{date}_{season}.nc")
  
  outfile <- here::here(paste0("data/temp/deltat/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_",
                         meta[[1]][["scenario"]], "_", meta[[1]][["season"]], ".nc"))
  
  if ( meta[[1]][["scenario"]] == "historical") {
    
    cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |>
      cdo_timmean() |>
      cdo_execute(outfile, option = "-L")
    
  } else {
    
    cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |>
      cdo_timmean() |>
      cdo_execute(outfile, option = "-L")
    
  }
  
})

models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

data <- purrr::map(Sys.glob(here("data/temp/eke_mean/*hist*mean.nc")), function(f) {
  
  message(f)
  meta <- unglue::unglue(basename(f), patterns = c("{var}_{model}_{scenario}_mean.nc", "{var}_{model}_{scenario}_mean.nc"))
  
  if (meta[[1]][["var"]] == "eke") {
    var <- "ua"
  } else {
    var <- "tasmax"
  }
  file_scenario <- here(paste0("data/temp/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_ssp585_mean.nc"))
  
  ReadNetCDF(f, vars = c(hist = var)) |> 
    _[, let(ssp585 = ReadNetCDF(file_scenario, vars = c(ssp585 = var), out = "vector")[[1]])] |> 
    _[, let(model = meta[[1]][["model"]],
            var = meta[[1]][["var"]],
            change = ssp585 - hist,
            lat = round(lat, digits = 8),
            plev = NULL)] |> 
    _[]
  
}) |> rbindlist()



# season ------------------------------------------------------------------


file_list <- Sys.glob(here("data/temp/eke/eke_day*"))

purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "{var}_day_{model}_{scenario}_{member}.nc")
  
  outfile_DJF <- here(paste0("data/temp/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_",
                         meta[[1]][["scenario"]], "_DJF.nc"))
  outfile_JJA <- here(paste0("data/temp/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_",
                             meta[[1]][["scenario"]], "_JJA.nc"))
  
  if ( meta[[1]][["scenario"]] == "historical") {
    
    cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |>
      cdo_selseason("DJF") |> 
      cdo_timmean() |> 
      cdo_execute(outfile_DJF, option = "-L")
    
    cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |>
      cdo_selseason("JJA") |> 
      cdo_timmean() |> 
      cdo_execute(outfile_JJA, option = "-L")
    
  } else {
    
    cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |>
      cdo_selseason("DJF") |> 
      cdo_timmean() |> 
      cdo_execute(outfile_DJF, option = "-L")
    
    cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |>
      cdo_selseason("JJA") |> 
      cdo_timmean() |> 
      cdo_execute(outfile_JJA, option = "-L")
    
  }
  
})


file_list <- Sys.glob(here("data/temp/100km/deltap97.5_*J*"))

purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "{var}_{season}_{model}_{scenario}_{member}_20150101-21001231.nc")
  
  outfile <- here(paste0("data/temp/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_",
                             meta[[1]][["scenario"]], "_", meta[[1]][["season"]], ".nc"))
  # outfile_JJA <- here(paste0("data/temp/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_",
  #                            meta[[1]][["scenario"]], "_JJA.nc"))
  
  if ( meta[[1]][["scenario"]] == "historical") {
    
    cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |>
      # cdo_selseason("DJF") |> 
      cdo_timmean() |> 
      cdo_execute(outfile, option = "-L")
    
    # cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |>
      # cdo_selseason("JJA") |> 
      # cdo_timmean() |> 
      # cdo_execute(outfile_JJA, option = "-L")
    
  } else {
  #   
    cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |>
  #     # cdo_selseason("DJF") |> 
      cdo_timmean() |>
      cdo_execute(outfile, option = "-L")
  #   
  #   cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |>
  #     # cdo_selseason("JJA") |> 
  #     cdo_timmean() |> 
  #     cdo_execute(outfile_JJA, option = "-L")
  #   
  }
  
})

data_season <- purrr::map(Sys.glob(here("data/temp/eke_mean/*hist*J*.nc")), function(f) {
  
  message(f)
  meta <- unglue::unglue(basename(f), patterns = c("{var}_{model}_{scenario}_mean.nc", "{var}_{model}_{scenario}_{season}.nc"))
  
  if (meta[[1]][["var"]] == "eke") {
    var <- "ua"
  } else {
    var <- "tasmax"
  }
  file_scenario <- here(paste0("data/cmip/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_ssp585_", meta[[1]][["season"]], ".nc"))
  
  ReadNetCDF(f, vars = c(hist = var)) |> 
    _[, let(ssp585 = ReadNetCDF(file_scenario, vars = c(ssp585 = var), out = "vector")[[1]])] |> 
    _[, let(model = meta[[1]][["model"]],
            var = meta[[1]][["var"]],
            season = meta[[1]][["season"]],
            change = ssp585 - hist,
            plev = NULL)] |> 
    _[]
  
}) |> rbindlist()

data_season |> 
  _[!(model %in% c("TaiESM1", "MRI-ESM2-0"))] |> 
  dcast(lat + lon + model + season ~ var, value.var = "change") |> 
  na.omit() |> 
  _[, FitLm(deltap97.5, eke), by = .(lat, lon, season)] |>
  _[term == "eke"] |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = estimate, fill = after_stat(level)),
                    breaks = c(-Inf, seq(-5, 5, 0.5), Inf)) +
  scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 0.5),
                                   labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  facet_wrap(~season) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Regression map betweem change in EKE and change in frecuency of temperature drop",
       subtitle = "Change 1980-2000 - 2080-2100") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))

data_season |> 
  # _[change < -50] |> 
  _[!(model %in% c("TaiESM1", "MRI-ESM2-0"))] |>
  # dcast(lat + lon + model + season ~ var, value.var = "change") |> 
  na.omit() |> 
  ggplot(aes(change)) +
  geom_density(aes(color = model)) +
  facet_grid(var~season)

# file_list <- Sys.glob(here("data/cmip/eke/eke_season*"))[1:24]
# 
# eke <- purrr::map(file_list, function(f) {
#   message(basename(f))
#   
#   meta <- unglue::unglue(basename(f), "eke_season_{model}_{experiment}_{member}.nc")
#   
#   a <- ReadNetCDF(f, vars = "ua") |> 
#     _[, let(land = MaskLand(lon, lat),
#             region = fcase(lat %between% c(25, 55), "NH",
#                            lat %between% c(-55, -25), "SH",
#                            default = NA))] |> 
#     _[, let(land = factor(land, labels = c("Sea", "Land")))] 
#   
#   if (meta[[1]][["experiment"]] == "historical") {
#     
#     a[time %between% as_datetime(c("1980-01-01 12:00:12", "2000-12-31 12:00:00")) & !is.na(region), 
#       .(mean_eke = mean(ua, na.rm = TRUE)), by = .(region, land, month(time))] |> 
#       _[, let(source_id = meta[[1]][["model"]],
#               experiment = meta[[1]][["experiment"]])]
#     
#   } else {
#     
#     a[time %between% as_datetime(c("2080-01-01 12:00:12", "2100-12-31 12:00:00")) & !is.na(region), 
#       .(mean_eke = mean(ua, na.rm = TRUE)), by = .(region, land, month(time))] |> 
#       _[, let(source_id = meta[[1]][["model"]],
#               experiment = meta[[1]][["experiment"]])]
#   }
#   
#   
# }) |> rbindlist()
# 
# readr::write_rds(eke, "data/cmip/derived/eke_change_seasons.rds")
# 

file_list <- c(Sys.glob(here("data/cmip/100km/deltap97.5_*J*_*ssp585*")),
               Sys.glob(here("data/cmip/100km/deltap97.5_*J*_*historical*")))

deltat <- purrr::map(file_list, function(f) {
  message(basename(f))

  meta <- unglue::unglue(basename(f), "delta{threshold}_{season}_{model}_{experiment}_{member}_20150101-21001231.nc")

  a <- ReadNetCDF(f, vars = "tasmax") |>
    _[, let(land = MaskLand(lon, lat),
            region = fcase(lat %between% c(25, 55), "NH",
                           lat %between% c(-55, -25), "SH",
                           default = NA))] |>
    _[, let(land = factor(land, labels = c("Sea", "Land")))]

  if (meta[[1]][["experiment"]] == "historical") {

    a[time %between% as_datetime(c("1980-01-01 12:00:12", "2000-12-31 12:00:00")) & !is.na(region),
      .(mean_dt = mean(tasmax, na.rm = TRUE)), by = .(region, land)] |>
      _[, let(source_id = meta[[1]][["model"]],
              experiment = meta[[1]][["experiment"]],
              season = meta[[1]][["season"]])]

  } else {

    a[time %between% as_datetime(c("2080-01-01 12:00:12", "2100-12-31 12:00:00")) & !is.na(region),
      .(mean_dt = mean(tasmax, na.rm = TRUE)), by = .(region, land)] |>
      _[, let(source_id = meta[[1]][["model"]],
              experiment = meta[[1]][["experiment"]],
              season = meta[[1]][["season"]])]
  }


}) |> rbindlist()

# readr::write_rds(eke, "data/cmip/derived/eke_change.rds")
readr::write_rds(deltat, "data/cmip/derived/deltap97.5_change_season.rds")
