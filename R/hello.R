file_path <- "~/Desktop/4_position_codes.opf"

read_opf <- function(file_path) {
  require(readr)
  require(dplyr)
  require(tibble)
  require(tidyr)
  require(stringr)
  require(purrr)
  conn<-unz(file_path, "db")
  raw_lines <- read_lines(conn) %>% discard(~ str_starts(.x, pattern = "#4"))
  column_name_indices <- which(str_detect(raw_lines, pattern = "\\d\\d:\\d\\d", negate = TRUE))
  # data_indices <- which(str_detect(raw_lines, pattern = "\\d\\d:\\d\\d", negate = FALSE))

  col_headers_raw <- map_chr(column_name_indices, ~raw_lines[.x])
  col_headers <- map(col_headers_raw, ~ str_split(.x, pattern = " "))
  col_names <- map_chr(col_headers, ~ .x[[1]][[1]])

  get_arg_names <- .  %>%  str_remove_all(pattern = coll("(MATRIX,true,)-"))  %>%
    str_remove_all(pattern = coll("(MATRIX,false,)-")) %>%
    str_remove_all(pattern = coll("|NOMINAL")) %>%
    str_split(pattern = ",") %>%  unlist()
  arg_names <- map(col_headers, ~ .x[[1]][[2]] %>% get_arg_names) %>% set_names(col_names)

  # NO ITERATION
  curr_col_index <- 1
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

return(col)
}


file_path_uz <- "~/Desktop/4_position_codes"
ds <- read_unzipped_opf(file_path_uz)
