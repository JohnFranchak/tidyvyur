unzip_opf <- function(file_path) {
  unzip(file_path, files = "db", exdir = dirname(file_path))
  file.rename(from = paste0(dirname(file_path),"/","db"),
            to = paste0(dirname(file_path),"/",gsub(".opf","",basename(file_path))))
}

file_path <- "~/Desktop/4_position_codes.opf"
unzip_opf(file_path)

read_unzipped_opf <- function(file_path) {
  require(readr)
  require(dplyr)
  require(tibble)
  require(tidyr)
  require(stringr)
  require(purrr)
raw_lines <- read_lines(file_path) %>% discard(~ str_starts(.x, pattern = "#4"))
column_name_indices <- which(str_detect(raw_lines, pattern = "\\d\\d:\\d\\d", negate = TRUE))
}

file_path <- "~/Desktop/4_position_codes"
