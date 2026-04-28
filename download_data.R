download <- function(url, file, zipped) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  
  message("Downloading ", file, "...")
  utils::download.file(url, file)
  
  if (zipped) {
    message("Unzpping files...")
    utils::unzip(file, exdir = dirname(file))
    unlink(file)
  }
  
  return(0)
}

datasets <- list(
  data = list(
    url = "https://zenodo.org/records/19840311/files/data.zip?download=1",
    file = here::here("data/data.zip"),
    zipped = TRUE
  )
)
options(timeout = 60*3)

lapply(datasets, function(dataset) {
  download(dataset$url, dataset$file, dataset$zipped)
})
