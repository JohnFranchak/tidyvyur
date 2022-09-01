read_opf <- function(file_path) {
  require(readr)
  require(dplyr)
  require(tibble)
  require(tidyr)
  require(stringr)
  require(purrr)
  require(lubridate)
  conn<-unz(file_path, "db")
  raw_lines <- read_lines(conn) %>% discard(~ str_starts(.x, pattern = "#4"))
  column_name_indices <- which(str_detect(raw_lines, pattern = "\\d\\d:\\d\\d", negate = TRUE))

  col_headers_raw <- map_chr(column_name_indices, ~raw_lines[.x])
  col_headers <- map(col_headers_raw, ~ str_split(.x, pattern = " "))
  col_names <- map_chr(col_headers, ~ .x[[1]][[1]])

  get_arg_names <- .  %>%  str_remove_all(pattern = coll("(MATRIX,true,)-"))  %>%
    str_remove_all(pattern = coll("(MATRIX,false,)-")) %>%
    str_remove_all(pattern = coll("|NOMINAL")) %>%
    str_split(pattern = ",") %>%  unlist()
  arg_names <- map(col_headers, ~ .x[[1]][[2]] %>% get_arg_names) %>% set_names(col_names)

  column_name_indices_shift <- c(column_name_indices[2:length(column_name_indices)], length(raw_lines)+1)
  data_raw <- map2(column_name_indices, column_name_indices_shift, ~raw_lines[{.x+1}:{.y-1}]) %>% set_names(col_names)

  clean_codes <- function(ds) {
    if (length(names(ds)) > 2 & "onset" %in% names(ds) & "offset" %in% names(ds)) {
      ds <- ds %>%
        mutate(across(-(onset:offset), ~ str_remove(.x, pattern = "^\\("))) %>%
        mutate(across(-(onset:offset), ~ str_remove(.x, pattern = "\\)$"))) %>%
        separate(onset, into = c("onset_hr","onset_min","onset_sec","onset_msec"), sep = ":", remove = F) %>%
        separate(offset, into = c("offset_hr","offset_min","offset_sec","offset_msec"), sep = ":", remove = F) %>%
        mutate(across(onset_hr:onset_msec, as.numeric),
               across(offset_hr:offset_msec, as.numeric),
               onset_duration = duration(hour = onset_hr, minute = onset_min, second = onset_sec) + dmilliseconds(onset_msec),
               offset_duration = duration(hour = offset_hr, minute = offset_min, second = offset_sec) + dmilliseconds(offset_msec),
               duration = offset_duration - onset_duration)
    }
    return(ds)
  }

  data_tbl <- map2(data_raw, arg_names, ~ read_csv(I(.x), col_names = c("onset","offset",.y)) %>% clean_codes)

return(data_tbl)
}

file_path <- "~/Desktop/4_position_codes.opf"
ds <- read_opf(file_path)


test %>%

