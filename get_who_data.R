# ==============================================================
# README
# Project: WHO Port List PDF -> data.frame
# Purpose: Download the WHO "IHR Ports" PDF and parse it into a
#          clean data.frame with fixed headers, rules and timestamp over time
# Author:  USP - Public Health Unit, Local Health Unit of Matosinhos
# Contact: albertojose.fernandes@ulsm.min-saude.pt
# Version: v1.0
# Date:    28/10/2025
# Notes:
#  - No Java needed (uses pdftools, string parsing).
#  - Intended for programmatic use in Shiny (pure function; no prints).
#  - Columns returned: Name, Code, SSCC, SSCEC, Extension, Other information, Date
# ==============================================================

suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(gh)
  library(readr)
  library(base64enc)
})

# ---- Public function ---------------------------------------------------------
# Returns a tibble with the parsed data.
# Arguments:
#   pdf_url  : character, the PDF URL (default = WHO endpoint)
#   today_fmt: character, date format for the Date column (default "%d/%m/%Y")
#   quietly  : logical, suppress warnings/messages (default TRUE)

get_who_port_list <- function(
    pdf_url  = "https://extranet.who.int/ihr/poedata/data_entry/ctrl/portListPDFCtrl.php",
    today_fmt = "%d/%m/%Y",
    quietly   = TRUE
) {
  # Download to temp (binary)
  tmp <- tempfile(fileext = ".pdf")
  utils::download.file(pdf_url, tmp, mode = "wb", quiet = quietly)
  
  # Extract text (each page is one string)
  pages_text <- pdftools::pdf_text(tmp)
  
  # Parse a page into non-empty lines
  parse_page_lines <- function(txt_page) {
    lines <- strsplit(txt_page, "\n")[[1]]
    lines <- str_trim(lines)
    lines[lines != ""]
  }
  
  all_lines <- map(pages_text, parse_page_lines) |> unlist()
  
  # Split a line into columns by 2+ spaces (fixed-width heuristic)
  split_line_to_cols <- function(line) {
    cols <- str_split(line, "\\s{2,}", simplify = TRUE)
    cols <- str_trim(cols)
    cols[cols != ""]
  }
  
  # Build rows, pad to max column count
  cols_list <- map(all_lines, split_line_to_cols)
  max_cols  <- max(purrr::map_int(cols_list, length), 1L)
  
  df_rows <- map_dfr(cols_list, function(cols) {
    row <- as.list(c(cols, rep(NA_character_, max_cols - length(cols))))
    names(row) <- paste0("V", seq_len(max_cols))
    tibble::as_tibble(row)
  })
  
  # Force header names for the first 6 columns
  target_headers <- c("Name", "Code", "SSCC", "SSCEC", "Extension", "Other information")
  names(df_rows)[seq_len(min(6, ncol(df_rows)))] <- target_headers[seq_len(min(6, ncol(df_rows)))]
  
  # Core cleaning & your rules (keeps rows with some NAs; only drops all-blank/NA in 2–6 when requested)
  df_clean <- df_rows %>%
    # Normalize blanks to NA
    mutate(across(everything(), ~ ifelse(. %in% c("", "NA"), NA, .))) %>%
    # Drop header repeats (first column equals literal "Name")
    filter(!(Name %in% c("Name"))) %>%
    # Drop only rows where ALL of columns 2–6 are NA (keep if any has data)
    filter(!if_all(c(Code, SSCC, SSCEC, Extension, `Other information`), is.na)) %>%
    # Remove rows where column 2 has "Page" (keep if Code is NA)
    filter(is.na(Code) | !str_detect(Code, regex("\\bPage\\b", ignore_case = TRUE))) %>%
    # Conditional right-shift when column 2 contains [ ] or [x]
    mutate(
      shift_flag = str_detect(Code, "\\[(x| )\\]"),
      `Other information` = if_else(shift_flag, Extension, `Other information`),
      Extension           = if_else(shift_flag, SSCEC, Extension),
      SSCEC               = if_else(shift_flag, SSCC, SSCEC),
      SSCC                = if_else(shift_flag, Code, SSCC),
      Code                = if_else(shift_flag, NA_character_, Code)
    ) %>%
    select(-shift_flag) %>%
    filter(!is.na(SSCC)) %>% 
    filter(!is.na(SSCEC)) %>% 
    filter(!is.na(Extension)) %>% 
    # Add timestamp column
    mutate(Date = format(Sys.Date(), today_fmt)) %>%
    # Squish whitespace for readability, keep NAs
    mutate(across(everything(), ~ ifelse(is.na(.), NA, str_squish(as.character(.))))) %>%
    # Ensure exactly the expected columns exist (some PDFs may produce fewer columns)
    {
      # Guarantee all target columns + Date exist
      want <- c(target_headers, "Date")
      missing <- setdiff(want, names(.))
      if (length(missing)) {
        for (m in missing) .[[m]] <- NA_character_
      }
      dplyr::select(., all_of(want))
    }
  
  df_clean
}

# ---- GitHub CSV persistence via GitHub Contents API -------------------------
# deps: install.packages(c("gh","readr","base64enc"))
# Uses env vars: GITHUB_PAT, GH_REPO, GH_PATH, GH_BRANCH (default "main")

gh_require_env <- function() {
  if (!nzchar(Sys.getenv("GITHUB_PAT"))) stop("Missing GITHUB_PAT")
  if (!nzchar(Sys.getenv("GH_REPO"))) stop("Missing GH_REPO ('user/repo')")
  if (!nzchar(Sys.getenv("GH_PATH"))) stop("Missing GH_PATH (e.g., 'who_history.csv')")
  invisible(TRUE)
}

# Read a CSV file from GitHub (returns tibble or NULL if file not found)
gh_read_csv <- function(repo = Sys.getenv("GH_REPO"),
                        path = Sys.getenv("GH_PATH"),
                        ref  = Sys.getenv("GH_BRANCH", unset = "main")) {
  gh_require_env()
  if (!requireNamespace("gh", quietly = TRUE)) stop("Install 'gh'")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Install 'readr'")
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Install 'base64enc'")
  
  res <- tryCatch(
    gh::gh("GET /repos/{owner}/{repo}/contents/{path}?ref={ref}",
           owner = strsplit(repo, "/")[[1]][1],
           repo  = strsplit(repo, "/")[[1]][2],
           path  = path,
           ref   = ref,
           .token = Sys.getenv("GITHUB_PAT")),
    error = identity
  )
  
  if (inherits(res, "error")) {
    # 404 → file doesn't exist yet
    if (grepl("404", conditionMessage(res))) return(NULL)
    stop(res)
  }
  
  raw_csv <- base64enc::base64decode(res$content)
  df <- readr::read_csv(rawConnection(raw_csv), show_col_types = FALSE)
  attr(df, "sha") <- res$sha  # needed for updates
  df
}

# Write CSV to GitHub (create or update). Returns new SHA.
gh_write_csv <- function(data,
                         repo = Sys.getenv("GH_REPO"),
                         path = Sys.getenv("GH_PATH"),
                         branch = Sys.getenv("GH_BRANCH", unset = "main"),
                         commit_message = "Update who_history.csv",
                         prev_sha = NULL) {
  gh_require_env()
  if (!requireNamespace("gh", quietly = TRUE)) stop("Install 'gh'")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Install 'readr'")
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Install 'base64enc'")
  
  # Serialize to CSV
  tf <- tempfile(fileext = ".csv")
  readr::write_csv(data, tf)
  
  # Read and base64-encode
  b64 <- base64enc::base64encode(tf)
  
  owner <- strsplit(repo, "/")[[1]][1]
  repoN <- strsplit(repo, "/")[[1]][2]
  
  body <- list(
    message = commit_message,
    content = b64,
    branch  = branch
  )
  # include 'sha' only for updates (not on create)
  if (!is.null(prev_sha)) body$sha <- prev_sha
  
  res <- gh::gh("PUT /repos/{owner}/{repo}/contents/{path}",
                owner = owner, repo = repoN, path = path,
                .token = Sys.getenv("GITHUB_PAT"),
                .send_json = TRUE, !!!body)
  res$content$sha
}

.who_content_cols <- c("Name","Code","SSCC","SSCEC","Extension","Other information")

append_who_history <- function(existing = NULL, new_snapshot) {
  cols <- c(.who_content_cols, "Date")
  
  # 1) If no existing file (first run) or wrong shape, create a 0-row tibble with the expected columns
  if (is.null(existing) || !all(cols %in% names(existing))) {
    empty_cols <- setNames(rep(list(character()), length(cols)), cols)
    existing <- tibble::as_tibble(empty_cols)
  } else {
    # keep only expected columns (in case the file has extras)
    existing <- dplyr::select(existing, dplyr::all_of(cols))
  }
  
  # 2) Ensure the new snapshot has exactly the expected columns (add missing as NA, drop extras)
  missing_in_new <- setdiff(cols, names(new_snapshot))
  if (length(missing_in_new)) {
    for (m in missing_in_new) new_snapshot[[m]] <- NA_character_
  }
  new_snapshot <- dplyr::select(new_snapshot, dplyr::all_of(cols))
  
  # 3) Bind + dedupe on all content columns + Date
  dplyr::bind_rows(existing, new_snapshot) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(cols)), .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(cols))
}

# Public: read existing history, append new snapshot, dedupe (per Date), and write back
update_history_github <- function(new_snapshot,
                                  repo   = Sys.getenv("GH_REPO"),
                                  path   = Sys.getenv("GH_PATH"),
                                  branch = Sys.getenv("GH_BRANCH", unset = "main")) {
  existing <- gh_read_csv(repo, path, ref = branch)
  prev_sha <- if (!is.null(existing)) attr(existing, "sha") else NULL
  
  combined <- append_who_history(existing, new_snapshot)
  
  new_sha <- gh_write_csv(
    combined,
    repo   = repo,
    path   = path,
    branch = branch,
    commit_message = sprintf("Append WHO snapshot %s", unique(new_snapshot$Date)),
    prev_sha = prev_sha
  )
  
  attr(combined, "sha") <- new_sha
  combined
}
# -----------------------------------------------------------------------------
