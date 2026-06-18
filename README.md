# WHO Port List — Automated Data Tracker

[![Last Update](https://img.shields.io/github/last-commit/albertofernandes/WHO-PortList-App?label=Last%20Update&color=blue)](https://github.com/albertofernandes/WHO-PortList-App/commits/main)
[![GitHub Actions](https://github.com/albertofernandes/WHO-PortList-App/actions/workflows/who_ports.yml/badge.svg)](https://github.com/albertofernandes/WHO-PortList-App/actions)

**USP · Public Health Unit, Local Health Unit of Matosinhos**  
**Maintainer:** Alberto José Fernandes · albertojose.fernandes@ulsm.min-saude.pt  
**Version:** v1.1 · February 2026

---

## Overview

This project fetches and parses the **WHO International Health Regulations (IHR) Ports List** from the WHO public CSV endpoint and tracks its evolution over time. A **GitHub Actions cron job** runs three times daily and commits updated data directly to the repository. A **Shiny dashboard** provides interactive exploration of the historical dataset.

---

## Key Features

- Automated download and parsing of the [WHO IHR Ports CSV](https://extranet.who.int/ihr/poedata/public/php/csvversion.php?lang=en&POEpage=0)
- Clean tabular output with columns: `Country`, `Name`, `Code`, `SSCC`, `SSCEC`, `Extension`, `Other information`, `Date`
- Automated updates 3× per day via GitHub Actions (08:30, 15:00, 22:00 Europe/Lisbon)
- Each run writes a timestamped snapshot (`snapshots/who_ports_YYYYmmdd-HHMM.csv`) and appends to the rolling `who_history.csv`
- Shiny dashboard to filter ports by country, search by name or code, and visualise certification status over time

---

## Repository Structure

```
WHO-PortList-App/
├── app.R                  # Shiny application
├── get_who_data.R         # Core functions: fetch, parse, and GitHub I/O
├── who_history.csv        # Rolling history file (updated automatically)
├── cron/
│   └── job_fetch_who.R    # Script executed by GitHub Actions on schedule
├── snapshots/             # Timestamped CSV snapshots (one per run)
└── .github/
    └── workflows/
        └── who_ports.yml  # Cron workflow configuration
```

---

## Shiny Application

The dashboard (`app.R`) loads `who_history.csv` directly from GitHub and provides:

- **Country selector** — filter ports by country (defaults to Portugal)
- **Port selector** — searchable dropdown by name or UNLOCODE; defaults to Leixões (PTLEI)
- **Date range filter** — optionally restrict the table and charts to a date window
- **Data information panel** — shows last update date, total ports, countries covered, and date range of the dataset
- **Three time-series plots** (last 6 months) — one per IHR criterion:
  - **SSCC** (blue) — Ship Sanitation Control Certificate
  - **SSCEC** (red) — Ship Sanitation Control Exemption Certificate
  - **Extension** (green) — Extension of SSCC/SSCEC
- **Filtered data table** — shows all rows for the selected port

Each plot shows `Yes [x]` (1) or `No [ ]` (0) for each observation date.

---

## Automated Data Updates (GitHub Actions)

The scheduled workflow runs `cron/job_fetch_who.R` on the following UTC cron schedule (equivalent to 08:30, 15:00, 22:00 Europe/Lisbon):

```yaml
schedule:
  - cron: '30 8,15,22 * * *'
```

**Each run:**
1. Downloads and parses the latest WHO IHR Ports CSV
2. Writes a timestamped snapshot: `snapshots/who_ports_YYYYmmdd-HHMM.csv`
3. Appends new rows and deduplicates the rolling `who_history.csv`
4. Commits and pushes changes back to the repository

**Environment variables used by the workflow:**

| Variable | Description | Default |
|---|---|---|
| `GH_REPO` | Target repository (`owner/repo`) | — |
| `GH_BRANCH` | Branch to update | `main` |
| `GH_PATH` | Path to rolling history file | — |
| `GH_SNAPSHOT_DIR` | Directory for timestamped snapshots | `snapshots` |
| `GITHUB_PAT` | GitHub API token (mapped from `GITHUB_TOKEN`) | — |

---

## Setting Up Your Own Fork

1. **Fork** the repository on GitHub.
2. Go to the **Actions** tab and click **Enable workflows**.
3. *(Optional)* Edit `.github/workflows/who_ports.yml` to adjust the cron schedule.
4. Push — the workflow will run automatically on schedule.

**Example run output:**
```
OK: wrote snapshots/who_ports_20251028-0830.csv and updated who_history.csv on branch main
```

---

## Manual Execution

Run the data update script locally:

```r
# From R
source("cron/job_fetch_who.R")
```

```bash
# From terminal
Rscript cron/job_fetch_who.R
```

**Required R packages:** `shiny`, `DT`, `gh`, `ggplot2`, `readr`, `base64enc`, `pdftools`, `dplyr`, `stringr`, `purrr`, `tidyr`, `httr2`

---

## Notes

- The Shiny app reads data directly from the GitHub-hosted CSV, so it always reflects the latest committed snapshot.
- Timestamped snapshots in `snapshots/` are retained for reproducibility and historical analysis.
- The cron job creates `who_history.csv` automatically on first run if it does not exist.

---

## Credits

**Author:** USP — Public Health Unit, ULS Matosinhos  
**Maintainer:** Alberto José Fernandes  
**Contact:** albertojose.fernandes@ulsm.min-saude.pt

---

## License

This project is open-source under the [MIT License](LICENSE). Pull requests are welcome.
