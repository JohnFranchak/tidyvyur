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
# data_indices <- which(str_detect(raw_lines, pattern = "\\d\\d:\\d\\d", negate = FALSE))

curr_col_index <- 1

# NEEDS TO ITERATE
col_header <- raw_lines[column_name_indices[curr_col_index]]
col_name <- str_split(col_header, pattern = " ")
arg_names <- str_remove_all(col_name[[1]][[2]],pattern = coll("(MATRIX,true,)-")) |>
  str_remove_all(pattern = coll("(MATRIX,false,)-")) |>
  str_remove_all(pattern = coll("|NOMINAL")) |>
  str_split(pattern = ",") |> unlist()

col <- read_csv(I(raw_lines[{column_name_indices[curr_col_index]+1}:{column_name_indices[curr_col_index+1]-1}]),
         col_names = c("onset","offset",arg_names)) |>
  mutate(across(-(onset:offset), ~ str_remove(.x, pattern = "^\\("))) |>
  mutate(across(-(onset:offset), ~ str_remove(.x, pattern = "\\)$")))

}


file_path <- "~/Desktop/4_position_codes"
