library(rcmip6)
library(data.table)
library(lubridate)
purrr::walk(Sys.glob(here::here("R/*")), source)

models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", "MRI-ESM2-0",
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4", "TaiESM1")

## tasmax & scenarios
## 
query <- list(
  project     = "CMIP6",
  type        = "Dataset",
  latest      = "true",
  query       = "*",
  activity_id = "ScenarioMIP",
  frequency   = "day",
  variable_id = "tasmax",
  source_id   = models_in_gadi, 
  replica     = "false"
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |>   
  _[experiment_id %in% c("ssp585") & table_id == "day" & nominal_resolution == "100 km"]

# models <- results_simply[, .N, by = .(experiment_id, source_id)] |> 
#   dcast(source_id ~ experiment_id) |> 
#   # _[, let(complete = ssp126 + ssp245 + ssp370 + ssp585)] |> 
#   # _[!is.na(complete), source_id]
#   _[, source_id]

a <- results_simply |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/gn/latest/*"),
                        paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                        source_id, "/",
                        experiment_id, "/",
                        member_id, "/day/", 
                        variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(20150101) | is.na(inidate)) | (enddate < ymd(21001231) | is.na(enddate))))]


# models_in_gadi <- a[in_gadi == TRUE, .N, by = .(experiment_id, source_id)] |>
#   dcast(source_id ~ experiment_id) |> 
#   # _[, let(complete = ssp126 + ssp245 + ssp370 + ssp585)] |> 
#   # _[!is.na(complete), source_id]
#   _[, source_id]

readr::write_rds(a, here::here("data/cmip_file_list_scenario.rds"))

## tasmax & historical
## 

query <- list(
  project       = "CMIP6",
  type          = "Dataset",
  latest        = "true",
  replica       = "false",
  query         = "*",
  activity_id   = "CMIP",
  experiment_id = "historical",
  frequency     = "day",
  source_id     = models_in_gadi,
  variable_id   = "tasmax"
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(18500101) | is.na(inidate)) | (enddate < ymd(20141231) | is.na(enddate))))]

readr::write_rds(results_simply, "data/cmip/cmip_file_list_historical.rds")

## ua,va & scenarios
## 

models_in_gadi <- readr::read_rds(here::here("data/cmip_file_list_historical.rds")) |>
  _[nominal_resolution == "100 km"]$source_id

query <- list(
  project       = "CMIP6",
  type          = "Dataset",
  latest        = "true",
  replica       = "false",
  query         = "*",
  activity_id = "ScenarioMIP",
  experiment_id = "ssp585",
  frequency     = "day",
  source_id     = models_in_gadi,
  variable_id   = c("ua", "va")
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(20150101) | is.na(inidate)) | (enddate < ymd(21001231) | is.na(enddate))))]


results_simply[experiment_id == "ssp585" & in_gadi == TRUE, .N, by = .(experiment_id, source_id)] |>
  dcast(source_id ~ experiment_id) |> 
  _[]

readr::write_rds(results_simply, here::here("data/cmip_file_list_scenarios_uv.rds"))


## ua,va & historical
## 

query <- list(
  project       = "CMIP6",
  type          = "Dataset",
  latest        = "true",
  replica       = "false",
  query         = "*",
  experiment_id = "historical",
  frequency     = "day",
  source_id     = models_in_gadi,
  variable_id   = c("ua", "va")
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(19500101) | is.na(inidate)) | (enddate < ymd(20141231) | is.na(enddate))))]


results_simply[in_gadi == TRUE, .N, by = .(experiment_id, source_id)] |>
  dcast(source_id ~ experiment_id) |> 
  _[]

readr::write_rds(results_simply, here::here("data/cmip_file_list_historical_uv.rds"))

## tos 
## 
query <- list(
  project     = "CMIP6",
  type        = "Dataset",
  latest      = "true",
  query       = "*",
  activity_id = "ScenarioMIP",
  frequency   = "mon",
  variable_id = "tos",
  source_id   = models_in_gadi, 
  replica     = "false"
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |>   
  _[experiment_id %in% c("ssp585") & table_id == "Omon"]

# models <- results_simply[, .N, by = .(experiment_id, source_id)] |> 
#   dcast(source_id ~ experiment_id) |> 
#   # _[, let(complete = ssp126 + ssp245 + ssp370 + ssp585)] |> 
#   # _[!is.na(complete), source_id]
#   _[, source_id]

scenario <- results_simply |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/Omon/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/Omon/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range_monthly(file_list[[1]]), by = path] |> 
  _[, let(inidate = ym(inidate),
          enddate = ym(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(20150101) | is.na(inidate)) | (enddate < ymd(21001231) | is.na(enddate))))]


query <- list(
  project     = "CMIP6",
  type        = "Dataset",
  latest      = "true",
  query       = "*",
  experiment_id = "historical",
  frequency   = "mon",
  variable_id = "tos",
  source_id   = models_in_gadi, 
  replica     = "false"
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |>   
  _[experiment_id %in% c("historical") & table_id == "Omon"]



hist <- results_simply |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/Omon/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/Omon/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range_monthly(file_list[[1]]), by = path] |> 
  _[, let(inidate = ym(inidate),
          enddate = ym(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(19790101) | is.na(inidate)) | (enddate < ymd(20010101) | is.na(enddate))))]


readr::write_rds(rbind(scenario, hist), here::here("data/cmip_file_list_tos.rds"))

## hfss 
## 
query <- list(
  project     = "CMIP6",
  type        = "Dataset",
  latest      = "true",
  query       = "*",
  activity_id = "ScenarioMIP",
  frequency   = "day",
  variable_id = "hfss",
  source_id   = models_in_gadi, 
  replica     = "false"
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |>   
  _[experiment_id %in% c("ssp585") & table_id == "day"]

# models <- results_simply[, .N, by = .(experiment_id, source_id)] |> 
#   dcast(source_id ~ experiment_id) |> 
#   # _[, let(complete = ssp126 + ssp245 + ssp370 + ssp585)] |> 
#   # _[!is.na(complete), source_id]
#   _[, source_id]

scenario <- results_simply |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/", table_id, "/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id,  "/", table_id, "/",
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(20150101) | is.na(inidate)) | (enddate < ymd(21001231) | is.na(enddate))))]


query <- list(
  project     = "CMIP6",
  type        = "Dataset",
  latest      = "true",
  query       = "*",
  experiment_id = "historical",
  frequency   = "day",
  variable_id = "hfss",
  source_id   = models_in_gadi, 
  replica     = "false"
)

results <- cmip_search(query)

results_simply <- results |>
  cmip_simplify() |>   
  _[experiment_id %in% c("historical") & table_id == "day"]

hist <- results_simply |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/", table_id, "/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/", table_id, "/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(19790101) | is.na(inidate)) | (enddate < ymd(20010101) | is.na(enddate))))]


readr::write_rds(rbind(scenario, hist), here::here("data/cmip_file_list_hfss.rds"))
