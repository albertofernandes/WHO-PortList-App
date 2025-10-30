# WHO Port List — Automated Data Tracker  
[![Last Update](https://img.shields.io/github/last-commit/albertofernandes/WHO-PortList-App?label=Last%20Update&color=blue)](https://github.com/albertofernandes/WHO-PortList-App/commits/main)
[![GitHub Actions](https://github.com/albertofernandes/WHO-PortList-App/actions/workflows/who_ports.yml/badge.svg)](https://github.com/albertofernandes/WHO-PortList-App/actions)

### USP · Public Health Unit, Local Health Unit of Matosinhos  
**Contact:** albertojose.fernandes@ulsm.min-saude.pt  
**Version:** v1.0 · **Date:** 28/10/2025  

---

**Overview**

This project parses the **World Health Organization (WHO) International Health Regulations (IHR) Ports List** PDF into a structured dataset and tracks its evolution over time.

The **Shiny application** allows interactive exploration of the dataset, while a **GitHub Actions cron job** automatically fetches and updates the data three times per day — ensuring that your published data stays up to date without manual intervention.

No Java is required — all parsing is done in pure R using the `pdftools` package.

---

**Key Features**

- 🔎 **Live parsing** of WHO’s official *IHR Ports List* PDF  
- 🗃️ **Clean tabular output** — columns:  
  `Name`, `Code`, `SSCC`, `SSCEC`, `Extension`, `Other information`, `Date`
- 🕐 **Automated updates** 3× per day via GitHub Actions  
- 📈 **Interactive dashboard** in Shiny  
- 💾 **Versioned history** — each run stores a timestamped snapshot and maintains a rolling `who_history.csv`

---

**Repository Structure**

WHO-PortList-App/
├── app.R # Shiny application (interactive dashboard)
├── get_who_data.R # Core functions: fetch, parse, and GitHub I/O
├── cron/
│ └── job_fetch_who.R # Script executed by GitHub Actions on schedule
└── .github/
└── workflows/
└── who_ports.yml # Cron job workflow configuration

---

**Shiny Application**

The **Shiny app** provides an interface for viewing and filtering port data.

**How it works:**
1. Filter the table to find a specific port.
2. Select one row (one port).
3. The chart displays **three colored lines** over time representing:
   - **SSCC**
   - **SSCEC**
   - **Extension**

Each line indicates if the port met the criterion on that date:
- `[x]` → Yes (1)  
- `[ ]` → No (0)

The app automatically reads from the GitHub-hosted CSV (`who_history.csv`) to display up-to-date results.

---

**Automated Data Updates (GitHub Actions)**

This repository includes a **scheduled GitHub Actions workflow** that keeps the data synchronized with WHO’s public PDF source.

**Schedule**

Runs daily at **08:30**, **15:00**, and **22:00** (Europe/Lisbon time):

schedule:
  - cron: '30 8,15,22 * * *'
  - 
**Workflow Summary**
Each scheduled run performs:
Checkout the repository
Install R and required system libraries (libpoppler, etc.)
Run the update script cron/job_fetch_who.R
Downloads and parses the latest WHO IHR Ports PDF
Writes a timestamped CSV snapshot: snapshots/who_ports_YYYYmmdd-HHMM.csv
Updates and deduplicates the rolling file: who_history.csv
Commit and push changes back to the repository automatically

**Authentication & Environment Variables**
The workflow uses GitHub’s built-in GITHUB_TOKEN, automatically mapped to GITHUB_PAT for GitHub API access.

Variable	Description	Default / Example
  GH_REPO	Target repository (owner/repo)
  GH_BRANCH	Branch to update
  GH_PATH	Rolling history file
  GH_SNAPSHOT_DIR	Directory for timestamped snapshots	snapshots
  GITHUB_PAT	Authentication token for gh API

**Setting Up Your Own Automated Job**
You can fork this repository and instantly have the same automation running.

1️⃣ Fork the repository
Click Fork in GitHub to create your own copy.

2️⃣ Enable GitHub Actions
Go to the Actions tab in your fork → click Enable workflows.

3️⃣ (Optional) Adjust schedule
Edit .github/workflows/who_ports.yml and modify the cron: line if you want a different update frequency.

4️⃣ Commit and push
Once pushed, GitHub Actions will automatically start running the workflow on schedule.

5️⃣ View results
Go to the Actions tab → select WHO Ports snapshots → view logs and output.

Example run output:
OK: wrote snapshots/who_ports_20251028-0830.csv and updated who_history.csv on branch main

**Manual Execution**
To test or run the update manually:
  From R:
    source("cron/job_fetch_who.R")
  From terminal:
    Rscript cron/job_fetch_who.R

**Notes**
Parsing uses pdftools + regex/string cleaning (no Java).
The cron workflow automatically creates missing CSVs on the first run.
Each timestamped file preserves historical snapshots for reproducibility.
The Shiny app always reads directly from the GitHub data, ensuring consistency with the latest snapshot.

**Credits**
Author: USP - Public Health Unit, Local Health Unit of Matosinhos
Maintainer: Alberto José Fernandes
Contact: albertojose.fernandes@ulsm.min-saude.pt

**License**
This project is open-source under the MIT License.
Pull requests - welcome

This project is open-source under the MIT License.
