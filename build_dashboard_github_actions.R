# Re-scrape the FCA register and rebuild the dashboard. Run by the GitHub
# Actions workflow in .github/workflows/monthly_update.yml; also works locally.
#
# Each step runs in its own R process and stops the build if it fails, so a
# failed scrape never overwrites the committed data.

run_step <- function(message, command, args) {
  print(paste("Progress update:", message))
  status <- system2(command, args)
  if (status != 0) stop(message, " failed (exit status ", status, ")")
}

run_step("Look up firms by FRN", "Rscript", "frn_to_csv.R")
run_step("Scrape current roles at each firm", "Rscript", "firm_roles_to_csv.R")
run_step("Render dashboard", "quarto", "render")
