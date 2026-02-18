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
    pdf_url  = "https://extranet.who.int/ihr/poedata/public/php/csvversion.php?lang=en&POEpage=0",
    today_fmt = "%d/%m/%Y",
    quietly   = TRUE
) {
  #### Download to temp (binary) ####
  tmp <- tempfile(fileext = ".csv")
  utils::download.file(pdf_url, tmp, mode = "wb", quiet = quietly)
  
  #### Manage csv content ####
  txt <- readLines(tmp, warn = FALSE)
  txt <- gsub(", ", ";", txt, fixed = FALSE)
  all_lines <- read.csv(textConnection(txt))
  all_lines[] <- lapply(all_lines, function(x) gsub(";", ", ", x, fixed = TRUE))
  all_lines <- fix_singletons_into_prev_last(all_lines)
  shift_cols <- function(df) {
    for (i in seq_len(nrow(df))) {
      # If all first three columns are NA, shift
      if (all(df[i, 6]=="")) {
        df[i, 4:6] <- df[i, 3:5]
        df[i, 3:5] <- NA
      }
    }
    df
  }
  all_lines <- shift_cols(all_lines)
  
  # Normalize column names to match what app.R expects
  if ("EXTENSION" %in% names(all_lines)) {
    names(all_lines)[names(all_lines) == "EXTENSION"] <- "Extension"
  }
  if ("Other.Information" %in% names(all_lines)) {
    names(all_lines)[names(all_lines) == "Other.Information"] <- "Other.Information"
  }
  
  all_lines$Date <- format(Sys.Date(), today_fmt)
  all_lines
}

# ---- GitHub CSV persistence via GitHub Contents API -------------------------
# deps: install.packages(c("gh","readr","base64enc"))
# Uses env vars: GITHUB_PAT, GH_REPO, GH_PATH, GH_BRANCH (default "main")
fix_singletons_into_prev_last <- function(df, sep = " ") {
  # treat blanks like missing
  is_used <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  last_col <- ncol(df)
  
  # make sure last column can hold pasted text
  df[[last_col]] <- as.character(df[[last_col]])
  
  keep <- rep(TRUE, nrow(df))
  anchor <- NA_integer_
  
  for (i in seq_len(nrow(df))) {
    used_count <- sum(is_used(df[i, ]))
    
    if (used_count == 1L) {
      # value present in exactly one column on this row
      idx <- which(is_used(df[i, ]))[1]
      val <- as.character(df[i, idx])
      
      if (!is.na(anchor)) {
        # append to the last column of the most recent non-singleton row
        cur <- df[anchor, last_col]
        df[anchor, last_col] <-
          if (!is_used(cur)) val else paste0(cur, sep, val)
        keep[i] <- FALSE                       # drop this singleton row
      } else {
        # first row is a singleton — nothing above to attach to; keep it
        anchor <- i
      }
      
    } else {
      # normal row (0 cells used is weird, 2+ is normal) — becomes new anchor
      anchor <- i
    }
  }
  
  df[keep, , drop = FALSE]
}

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
  
  # normalize commit_message to single non-empty string
  if (length(commit_message) == 0L || all(is.na(commit_message))) {
    commit_message <- "Update who_history.csv"
  } else {
    commit_message <- paste0(commit_message[!is.na(commit_message)], collapse = "; ")
  }
  
  tf <- tempfile(fileext = ".csv")
  readr::write_csv(data, tf)
  b64 <- base64enc::base64encode(tf)
  
  owner <- strsplit(repo, "/")[[1]][1]
  repoN <- strsplit(repo, "/")[[1]][2]
  
  body <- list(
    message = commit_message,
    content = b64,
    branch  = branch
  )
  if (!is.null(prev_sha)) {
    body$sha <- prev_sha
  }
  
  res <- gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner      = owner,
    repo       = repoN,
    path       = path,
    .token     = Sys.getenv("GITHUB_PAT"),
    .send_json = TRUE,
    !!!body
  )
  res$content$sha
}

.who_content_cols <- c("Country","Name","Code","SSCC","SSCEC","Extension","Other.Information")

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
  existing <- existing %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character))
  
  new_snapshot <- new_snapshot %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character))
  
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
  # --- build a safe scalar commit message ---
  dates <- unique(new_snapshot$Date)
  dates <- dates[!is.na(dates)]
  
  if (length(dates) >= 1) {
    date_str <- paste(dates, collapse = ", ")
    commit_msg <- sprintf("Append WHO snapshot %s", date_str)
  } else {
    commit_msg <- "Append WHO snapshot"
  }
  
  # small helper that actually does: read -> append -> write
  do_update <- function() {
    existing <- gh_read_csv(repo, path, ref = branch)
    prev_sha <- if (!is.null(existing)) attr(existing, "sha") else NULL
    
    combined <- append_who_history(existing, new_snapshot)
    
    new_sha <- gh_write_csv(
      combined,
      repo           = repo,
      path           = path,
      branch         = branch,
      commit_message = commit_msg,
      prev_sha       = prev_sha
    )
    
    attr(combined, "sha") <- new_sha
    combined
  }
  
  # Try once; on 409, re-read and try again
  tryCatch(
    do_update(),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("409", msg)) {
        warning("GitHub 409 conflict detected; re-reading file and retrying once...")
        return(do_update())
      } else {
        stop(e)
      }
    }
  )
}
# -----------------------------------------------------------------------------
