# job_fetch_who.R
suppressPackageStartupMessages({
  library(gh)
  library(readr)
  library(base64enc)
})

# Reuse your parsing + GitHub helpers
source("get_who_data.R")  # defines get_who_port_list(), update_history_github(), gh_write_csv()

# --- Fetch the current snapshot ---
snap <- get_who_port_list()

# --- Build a timestamped path like snapshots/who_ports_20251028-0830.csv ---
repo   <- Sys.getenv("GH_REPO")
branch <- Sys.getenv("GH_BRANCH", "main")
dir    <- Sys.getenv("GH_SNAPSHOT_DIR", "snapshots")

# Use Europe/Lisbon time for filenames
ts_lx  <- format(as.POSIXlt(Sys.time(), tz = "Europe/Lisbon"), "%Y%m%d-%H%M")
snap_path <- sprintf("%s/who_ports_%s.csv", dir, ts_lx)

# --- Write the timestamped CSV to GitHub (new file each run) ---
# gh_write_csv() will create the file (no prev_sha), including nested dirs
invisible(gh_write_csv(
  data = snap,
  repo = repo,
  path = snap_path,
  branch = branch,
  commit_message = sprintf("Snapshot %s", ts_lx),
  prev_sha = NULL
))

# --- Update the rolling history CSV (append + dedupe by your logic) ---
invisible(update_history_github(
  new_snapshot = snap,
  repo   = repo,
  path   = Sys.getenv("GH_PATH"),
  branch = branch
))

cat(sprintf("OK: wrote %s and updated %s on branch %s\n", snap_path, Sys.getenv("GH_PATH"), branch))