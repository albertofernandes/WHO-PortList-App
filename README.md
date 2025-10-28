# 🧭 WHO Designated Ports – Shiny App

This Shiny app automatically downloads and parses the **World Health Organization (WHO) Designated Ports List** PDF, converting it into a structured dataset.  
Each time the app runs, it appends new data with a timestamp, allowing the creation of a **historical evolution** of WHO-designated ports.

Built by: **USP – Public Health Unit**, Local Health Unit of Matosinhos  
Author: **Alberto Fernandes** (albertojose.fernandes@ulsm.min-saude.pt)  
Version: **v1.0** (28/10/2025)

---

## ✨ Features
- Fetches and parses the WHO PDF directly
- Cleans and structures the data into 6 core columns:
  - Name, Code, SSCC, SSCEC, Extension, Other information
- Adds a timestamp (`dd/mm/yyyy`) for historical tracking
- Optionally persists data to a GitHub CSV file (for free, no database needed)
- Can be hosted free on **[shinyapps.io](https://www.shinyapps.io/)**

---

## 📁 Project structure

WHO-PortList-App/
├── app.R # Main Shiny app
├── get_who_data.R # WHO PDF parsing + persistence helpers
├── DESCRIPTION # Package dependencies
├── secrets.R # Environment variables (GITHUB_PAT etc.) – DO NOT COMMIT
├── .gitignore # Excludes secrets and local junk
└── README.md # This file

---

## 🧩 Requirements

You need R ≥ 4.2 and the following packages:

```r
install.packages(c(
  "shiny", "DT", "pdftools", "dplyr", "stringr",
  "tidyr", "purrr", "gh", "readr", "base64enc"
))
The DESCRIPTION file already lists these dependencies for deployment on shinyapps.io.

⚙️ Running locally
Clone the repository:

bash
Copiar código
git clone https://github.com/<your-username>/WHO-PortList-App.git
cd WHO-PortList-App
Create a file named secrets.R in the same folder:

r
Copiar código
# secrets.R  -- DO NOT COMMIT THIS FILE
Sys.setenv(
  GITHUB_PAT = "ghp_your_token_here",            # classic PAT with 'repo' scope
  GH_REPO    = "youruser/who-ports-history",     # e.g. "observatorio-saude/who-ports-history"
  GH_PATH    = "who_history.csv",                # file path in repo
  GH_BRANCH  = "main"                            # or 'master' if that’s your branch
)

⚠️ Keep this file private. Add it to .gitignore (see below).

Run the app:

r
Copiar código
shiny::runApp("WHO-PortList-App")
🧠 How the GitHub persistence works
Every time the app fetches new WHO data:

It merges it with an existing CSV file in your GitHub repo (who_history.csv)

Removes duplicates within the same timestamp

Commits the updated CSV back using your Personal Access Token (GITHUB_PAT)

Example target repository
https://github.com/observatorio-saude/who-ports-history
The repo only needs one file: who_history.csv.

The app handles the rest.

☁️ Deploying to ShinyApps.io
Create an account at shinyapps.io.

In R, install the rsconnect package:
install.packages("rsconnect")
Authorize your R environment (one-time setup):

rsconnect::setAccountInfo(
  name='your-account-name',
  token='xxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
  secret='xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
)

You can get these values from your shinyapps.io dashboard → Account → Tokens.

Deploy the app:
library(rsconnect)
rsconnect::deployApp(appFiles = c("app.R","get_who_data.R","DESCRIPTION","secrets.R"))
Once deployed, shinyapps.io will give you a public link like:
https://observatorio-saude-matosinhos.shinyapps.io/WHOPortList/

🔒 Security notes
Never commit secrets.R or your GitHub token.
Add it to .gitignore:

# local secrets
secrets.R

# RStudio artifacts
.Rproj.user
.Rhistory
.RData
.Ruserdata

# Environment configs
.Renviron


Use a classic GitHub Personal Access Token with repo scope.
Fine-grained tokens often fail in R’s gh package.

🕐 Data update logic
Each run of the app does:

Download WHO PDF
Parse and clean data into a data.frame
Add current date (dd/mm/yyyy)
Merge with existing history file
Remove duplicates for same day
Save back to GitHub CSV

🧾 License
This code is released for public health research and education.

Attribution required:
Developed by USP – Public Health Unit, Local Health Unit of Matosinhos (ULSM), Portugal.

📬 Contact
Lead author: Alberto José Fernandes
Institution: Unidade de Saúde Pública, ULS Matosinhos
Email: albertojose.fernandes@ulsm.min-saude.pt

GitHub Issues / Pull requests: welcome
