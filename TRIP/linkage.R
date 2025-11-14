# ---- packages ----
library(readr)
library(dplyr)
library(purrr)
library(data.table)   # fallback binder (very forgiving)
library(rlang)

# ---- file paths ----
p1 <- "C:\\Users\\Kiffer\\OneDrive - Simon Fraser University (1sfu)\\HEAL Lab\\2. Projects\\10. TRIP\\data_wave1.csv"
p2 <- "C:\\Users\\Kiffer\\OneDrive - Simon Fraser University (1sfu)\\HEAL Lab\\2. Projects\\10. TRIP\\data_wave2.csv"
p3 <- "C:\\Users\\Kiffer\\OneDrive - Simon Fraser University (1sfu)\\HEAL Lab\\2. Projects\\10. TRIP\\data_wave3.csv"

# ---- helper: read, tag, and normalize a wave ----
read_wave <- function(path, wave_num) {
  df <- read_csv(path, guess_max = 100000, show_col_types = FALSE) |>
    mutate(wave = wave_num)
  
  # make names unique within each import
  names(df) <- make.unique(names(df))
  
  # flatten any list-cols to character
  if (any(map_lgl(df, is.list))) {
    df <- df |> mutate(across(where(is.list), ~ sapply(.x, toString)))
  }
  
  # harmonize factors to character
  df <- df |> mutate(across(where(is.factor), as.character))
  
  df
}

# ---- read all three waves ----
w1 <- read_wave(p1, 1)
w2 <- read_wave(p2, 2)
w3 <- read_wave(p3, 3)

# ---- bind rows (try dplyr first, then data.table fallback) ----
merged_data <- tryCatch(
  bind_rows(w1, w2, w3),
  error = function(e) {
    message("bind_rows failed: ", e$message, " — using data.table::rbindlist(fill=TRUE)")
    as.data.frame(rbindlist(list(w1, w2, w3), use.names = TRUE, fill = TRUE))
  }
)

table(merged_data$wave)
# ---- coalesce duplicate email/response_id columns into single fields ----
# find any columns that look like email or response_id (including ...12 suffixes)
email_cols <- grep("(?i)^email(_address)?(\\.\\.\\d+)?$", names(merged_data), value = TRUE, perl = TRUE)
resp_cols  <- grep("(?i)^response(_)?id(\\.\\.\\d+)?$|^response\\.id(\\.\\.\\d+)?$", names(merged_data), value = TRUE, perl = TRUE)

# create single canonical columns if any candidates exist
if (length(email_cols) > 0) {
  # coalesce in left-to-right order (first non-NA wins)
  merged_data$email_address <- dplyr::coalesce(!!!merged_data[email_cols] |> lapply(as.character))
  # drop the duplicate email columns except the new canonical one
  keep_names <- setdiff(names(merged_data), setdiff(email_cols, "email_address"))
  merged_data <- merged_data[, keep_names, drop = FALSE]
}

if (length(resp_cols) > 0) {
  merged_data$response_id <- dplyr::coalesce(!!!merged_data[resp_cols] |> lapply(as.character))
  keep_names <- setdiff(names(merged_data), setdiff(resp_cols, "response_id"))
  merged_data <- merged_data[, keep_names, drop = FALSE]
}

table(merged_data$wave)
# ---- normalize and build a stable UID (email -> response_id -> placeholder) ----
# do NOT use '.' inside mutate() conditions; handle branching outside then mutate simple transforms
if ("email_address" %in% names(merged_data)) {
  merged_data$email_address <- na_if(trimws(as.character(merged_data$email_address)), "")
} else {
  merged_data$email_address <- NA_character_
}

if ("response_id" %in% names(merged_data)) {
  merged_data$response_id <- trimws(as.character(merged_data$response_id))
} else {
  merged_data$response_id <- NA_character_
}

merged_data <- merged_data |>
  mutate(
    uid = dplyr::coalesce(email_address, response_id, paste0("NA_", row_number()))
  )

table(merged_data$wave)

# ---- keep only UIDs that appear 2 or 3 times across waves ----
merged_data <- merged_data |>
  add_count(uid, name = "num_obs_per_UID") |>
  filter(num_obs_per_UID %in% c(2, 3))

write_csv(merged_data, "C:\\Users\\Kiffer\\OneDrive - Simon Fraser University (1sfu)\\HEAL Lab\\2. Projects\\10. TRIP\\merged_data.csv")

table(merged_data$wave)


## Add response counts to data dictionary
labels <- read.csv( "C:\\Users\\Kiffer\\OneDrive - Simon Fraser University (1sfu)\\HEAL Lab\\2. Projects\\10. TRIP\\merged_labels.csv")

# ---- helper: validity predicate for a vector ----
is_valid_response <- function(x) {
  # Flatten odd types
  if (is.list(x))  x <- sapply(x, toString)
  if (is.factor(x)) x <- as.character(x)
  
  # Basic NA / blank
  is_na     <- is.na(x)
  is_blank  <- is.character(x) & trimws(x) == ""
  
  # Sentinel 999 (allow numeric or character)
  # Coerce to character then numeric safely
  x_char <- as.character(x)
  x_num  <- suppressWarnings(as.numeric(x_char))
  is_999 <- (!is.na(x_num) & x_num == 999) | (trimws(x_char) == "999")
  
  !(is_na | is_blank | is_999)
}

# ---- count valid responses by wave for a single variable ----
count_by_wave <- function(varname, df, waves = 1:3) {
  if (!varname %in% names(df)) {
    # variable not present in merged_data
    return(setNames(as.integer(rep(NA, length(waves))), paste0("n_wave", waves)))
  }
  x <- df[[varname]]
  out <- sapply(waves, function(w) {
    idx <- df$wave == w
    if (!any(idx)) return(NA_integer_)
    sum(is_valid_response(x[idx]), na.rm = TRUE)
  })
  setNames(as.integer(out), paste0("n_wave", waves))
}

# ---- build counts for all variables in labels ----
waves <- sort(unique(merged_data$wave))
waves <- intersect(waves, 1:3)  # ensure we only make n_wave1..3

counts_list <- purrr::map(labels$variable_name, ~ count_by_wave(.x, merged_data, waves = waves))
counts_df   <- dplyr::bind_rows(counts_list)

# If a wave is missing from data, still ensure we have all three columns
for (w in 1:3) {
  col <- paste0("n_wave", w)
  if (!col %in% names(counts_df)) counts_df[[col]] <- NA_integer_
}

# ---- attach counts to data dictionary ----
labels_with_counts <- dplyr::bind_cols(labels, counts_df[, paste0("n_wave", 1:3)])

# ---- save (optional) ----
write.csv(
  labels_with_counts,
  "C:\\Users\\Kiffer\\OneDrive - Simon Fraser University (1sfu)\\HEAL Lab\\2. Projects\\10. TRIP\\merged_labels_with_counts.csv",
  row.names = FALSE
)

# quick peek
head(labels_with_counts)