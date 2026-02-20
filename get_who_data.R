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
  library(httr2)
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
  
  # Normalize column names to match canonical names
  col_renames <- c(
    "EXTENSION" = "Extension",
    "Other.information" = "Other.Information"  # Ensure it uses dot, not space
  )
  for (old in names(col_renames)) {
    if (old %in% names(all_lines)) {
      names(all_lines)[names(all_lines) == old] <- col_renames[[old]]
    }
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
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Install 'httr2'")
  
  owner <- strsplit(repo, "/")[[1]][1]
  repo_name <- strsplit(repo, "/")[[1]][2]
  
  # Use raw.githubusercontent.com to download the file directly (supports large files)
  raw_url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s", 
                     owner, repo_name, ref, path)
  
  # Download the file to a temporary file instead of loading into memory
  temp_file <- tempfile(fileext = ".csv")
  
  response <- tryCatch({
    httr2::request(raw_url) |>
      httr2::req_headers(Authorization = paste("token", Sys.getenv("GITHUB_PAT"))) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = temp_file)
  }, error = function(e) {
    return(NULL)
  })
  
  # Check if successful
  if (is.null(response)) {
    cat("Download failed (NULL response)\n")
    return(NULL)
  }
  
  status <- httr2::resp_status(response)
  if (status == 404) {
    cat("File not found (404)\n")
    return(NULL)
  }
  if (status >= 400) {
    stop("Failed to download file from GitHub: HTTP ", status)
  }
  
  # Check if file was written
  if (!file.exists(temp_file)) {
    stop("Failed to write temporary file")
  }
  
  file_size <- file.info(temp_file)$size
  cat("Downloaded", round(file_size / 1024 / 1024, 2), "MB to temp file\n")
  
  # Read the CSV from the file with explicit encoding handling
  cat("Reading CSV...\n")
  df <- readr::read_csv(temp_file, 
                        show_col_types = FALSE,
                        locale = readr::locale(encoding = "UTF-8"))
  
  cat("Read", nrow(df), "rows and", ncol(df), "columns\n")
  
  # Clean up temp file
  unlink(temp_file)
  
  # Get the file SHA for updates (need a separate API call)
  # Note: This uses the regular API which works for metadata even on large files
  cat("Getting file SHA...\n")
  sha_res <- tryCatch({
    gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
           owner = owner,
           repo = repo_name,
           path = path,
           ref = ref,
           .token = Sys.getenv("GITHUB_PAT"))
  }, error = function(e) {
    cat("Warning: Could not get SHA, file might be too large for Contents API\n")
    return(list(sha = NA))
  })
  
  attr(df, "sha") <- sha_res$sha
  df
}

# Write CSV to GitHub (handles files of any size using Git Blob API)
gh_write_csv <- function(data,
                         repo = Sys.getenv("GH_REPO"),
                         path = Sys.getenv("GH_PATH"),
                         branch = Sys.getenv("GH_BRANCH", unset = "main"),
                         commit_message = "Update who_history.csv",
                         prev_sha = NULL) {  # prev_sha is ignored but kept for compatibility
  
  if (!requireNamespace("gh", quietly = TRUE)) stop("Install 'gh'")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Install 'readr'")
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Install 'base64enc'")
  
  gh_require_env()
  
  owner <- strsplit(repo, "/")[[1]][1]
  repo_name <- strsplit(repo, "/")[[1]][2]
  
  # 1. Write CSV to temp file
  tf <- tempfile(fileext = ".csv")
  readr::write_csv(data, tf)
  file_size <- file.info(tf)$size
  cat("File size:", round(file_size / 1024 / 1024, 2), "MB\n")
  
  # 2. Read as base64
  b64 <- base64enc::base64encode(tf)
  
  # Retry logic for handling concurrent updates
  max_retries <- 3
  retry_count <- 0
  
  while (retry_count < max_retries) {
    tryCatch({
      # 3. Get the current branch reference (FRESH each retry)
      ref_res <- gh::gh("GET /repos/{owner}/{repo}/git/ref/heads/{branch}",
                        owner = owner,
                        repo = repo_name,
                        branch = branch,
                        .token = Sys.getenv("GITHUB_PAT"))
      
      current_sha <- ref_res$object$sha
      
      # 4. Get the current commit
      commit_res <- gh::gh("GET /repos/{owner}/{repo}/git/commits/{sha}",
                           owner = owner,
                           repo = repo_name,
                           sha = current_sha,
                           .token = Sys.getenv("GITHUB_PAT"))
      
      tree_sha <- commit_res$tree$sha
      
      # 5. Create a blob for the file content
      blob_res <- gh::gh("POST /repos/{owner}/{repo}/git/blobs",
                         owner = owner,
                         repo = repo_name,
                         content = b64,
                         encoding = "base64",
                         .token = Sys.getenv("GITHUB_PAT"))
      
      blob_sha <- blob_res$sha
      
      # 6. Create a new tree with the updated file
      tree_res <- gh::gh("POST /repos/{owner}/{repo}/git/trees",
                         owner = owner,
                         repo = repo_name,
                         base_tree = tree_sha,
                         tree = list(
                           list(
                             path = path,
                             mode = "100644",
                             type = "blob",
                             sha = blob_sha
                           )
                         ),
                         .token = Sys.getenv("GITHUB_PAT"))
      
      new_tree_sha <- tree_res$sha
      
      # 7. Create a new commit
      new_commit_res <- gh::gh("POST /repos/{owner}/{repo}/git/commits",
                               owner = owner,
                               repo = repo_name,
                               message = commit_message,
                               tree = new_tree_sha,
                               parents = list(current_sha),
                               .token = Sys.getenv("GITHUB_PAT"))
      
      new_commit_sha <- new_commit_res$sha
      
      # 8. Update the branch reference
      gh::gh("PATCH /repos/{owner}/{repo}/git/refs/heads/{branch}",
             owner = owner,
             repo = repo_name,
             branch = branch,
             sha = new_commit_sha,
             .token = Sys.getenv("GITHUB_PAT"))
      
      # Success! Return the new commit SHA
      return(new_commit_sha)
      
    }, error = function(e) {
      error_msg <- conditionMessage(e)
      
      # Check if it's a fast-forward error
      if (grepl("422|fast forward", error_msg, ignore.case = TRUE)) {
        retry_count <<- retry_count + 1
        if (retry_count < max_retries) {
          cat("⚠ Conflict detected, retrying (", retry_count, "/", max_retries, ")...\n")
          Sys.sleep(1)  # Wait 1 second before retrying
          return(NULL)  # Continue the while loop
        } else {
          stop("Failed to commit after ", max_retries, " retries due to conflicts")
        }
      } else {
        # Different error, re-throw it
        stop(e)
      }
    })
  }
  
  stop("Should not reach here")
}

.who_content_cols <- c("Country","Name","Code","SSCC","SSCEC","Extension","Other.Information")

append_who_history <- function(existing = NULL, new_snapshot) {
  cols <- c(.who_content_cols, "Date")
  
  # DEBUG: Print what we're working with
  cat("Expected columns:", paste(cols, collapse=", "), "\n")
  if (!is.null(existing)) {
    cat("Existing columns:", paste(names(existing), collapse=", "), "\n")
  }
  cat("New snapshot columns:", paste(names(new_snapshot), collapse=", "), "\n")
  
  # 1) If no existing file (first run), create a 0-row tibble with the expected columns
  if (is.null(existing)) {
    cat("Creating new empty history (no existing file)\n")
    empty_cols <- setNames(rep(list(character()), length(cols)), cols)
    existing <- tibble::as_tibble(empty_cols)
  } else {
    # CHECK: Do we have the critical columns?
    missing_critical <- setdiff(cols, names(existing))
    if (length(missing_critical) > 0) {
      cat("WARNING: Missing columns in existing file:", paste(missing_critical, collapse=", "), "\n")
      cat("Will add missing columns instead of resetting everything\n")
      
      # Add missing columns to existing data instead of destroying it
      for (m in missing_critical) {
        existing[[m]] <- NA_character_
      }
      # Reorder to match expected columns
      existing <- dplyr::select(existing, dplyr::all_of(cols))
    } else {
      # keep only expected columns (in case the file has extras)
      existing <- dplyr::select(existing, dplyr::all_of(cols))
    }
  }
  
  # 2) Ensure the new snapshot has exactly the expected columns (add missing as NA, drop extras)
  missing_in_new <- setdiff(cols, names(new_snapshot))
  if (length(missing_in_new)) {
    cat("Adding missing columns to new snapshot:", paste(missing_in_new, collapse=", "), "\n")
    for (m in missing_in_new) new_snapshot[[m]] <- NA_character_
  }
  new_snapshot <- dplyr::select(new_snapshot, dplyr::all_of(cols))
  
  # Convert everything to character to avoid type mismatches
  existing <- existing %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character))
  
  new_snapshot <- new_snapshot %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character))
  
  # 3) Bind + dedupe on all content columns + Date
  cat("Before binding - existing rows:", nrow(existing), ", new rows:", nrow(new_snapshot), "\n")
  
  result <- dplyr::bind_rows(existing, new_snapshot) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(cols)), .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(cols))
  
  cat("After deduplication - result rows:", nrow(result), "\n")
  
  result
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
    
    # Safety guard: never write back fewer rows than we already have
    # if (!is.null(existing) && nrow(existing) > 0 && nrow(combined) < nrow(existing)) {
    #   warning(sprintf(
    #     "Aborting write: combined rows (%d) < existing rows (%d). No data written.",
    #     nrow(combined), nrow(existing)
    #   ))
    #   return(existing)
    # }
    
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
  
  # Wrap the entire cycle in tryCatch to prevent any error from propagating
  tryCatch(
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
    ),
    error = function(e) {
      warning("update_history_github failed: ", conditionMessage(e),
              ". Returning existing data unchanged.")
      tryCatch(gh_read_csv(repo, path, ref = branch), error = function(e2) tibble::tibble())
    }
  )
}
# -----------------------------------------------------------------------------