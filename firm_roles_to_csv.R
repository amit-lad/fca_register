# For every firm in firms.csv, list every SMF role held there, current and
# historic, one row per role held by a person, with start and end dates.
# Non-SMF roles are not collected.
#
# Dates are only on each person's own register page, so this runs in two passes:
#   1. Fetch each firm and note everyone whose current or previous roles there
#      include an SMF role.
#   2. Fetch each of those people once and keep their SMF roles at our firms.
#
# start_date is the date the role took effect. end_date is blank for roles held
# now. scraped_on is the UTC date the person's page was fetched.
#
# Usage:  Rscript firm_roles_to_csv.R [firms.csv] [smf_roles.csv]

source("fca_register.R")

args <- commandArgs(trailingOnly = TRUE)
in_file <- if (length(args) > 0) args[1] else "firms.csv"
out_file <- if (length(args) > 1) args[2] else "smf_roles.csv"

firms <- read.csv(in_file, colClasses = "character", encoding = "UTF-8")
firms <- firms[firms$company_id != "", ]

is_smf <- function(role) startsWith(role %||% "", "SMF")

# Pass 1: people with an SMF role at any of our firms.
failed_firms <- character(0)
candidates <- character(0)
for (i in seq_len(nrow(firms))) {
  firm <- firms[i, ]
  cat(sprintf("[firm %d/%d] %s %s\n", i, nrow(firms), firm$frn, firm$firm_name))
  rv <- tryCatch(get_firm(firm$company_id), error = function(e) {
    message("  failed: ", conditionMessage(e))
    failed_firms <<- c(failed_firms, firm$frn)
    NULL
  })
  for (p in c(rv$currentIndividualsList, rv$historicIndividualsList)) {
    roles <- vapply(p$Approved_Controlled_Functions__r, function(f) f$ShGl_InvolvementType__r$Name, "")
    if (any(is_smf(roles))) candidates <- c(candidates, p$Id)
  }
  Sys.sleep(0.5) # be gentle with the register
}
candidates <- unique(candidates)
cat(sprintf("\n%d people have held an SMF role at these firms\n\n", length(candidates)))

# One row per SMF role at one of our firms.
smf_rows <- function(rv, scraped_on) {
  as_rows <- function(functions, current) {
    keep <- Filter(function(f) {
      is_smf(f$ShGl_InvolvementType__r$Name) && (f$ShGl_Account__c %||% "") %in% firms$company_id
    }, functions)
    lapply(keep, function(f) data.frame(
      company_id = f$ShGl_Account__c,
      smf_role = f$ShGl_InvolvementType__r$Name,
      name = rv$cont$Name,
      individual_id = rv$cont$Id,
      individual_ref = rv$cont$ShGl_IRN__c %||% NA_character_,
      start_date = f$ShGl_EffectiveDate__c %||% NA_character_,
      end_date = if (current) NA_character_ else f$ShGl_InvolvementStatusEndDate__c %||% NA_character_,
      scraped_on = scraped_on
    ))
  }
  c(as_rows(rv$ContFunctionsList, TRUE), as_rows(rv$PreContFunctionsList, FALSE))
}

# Pass 2: each person's SMF roles, with dates.
failed_people <- character(0)
rows <- list()
for (i in seq_along(candidates)) {
  if (i %% 100 == 0 || i == length(candidates)) {
    cat(sprintf("[person %d/%d]\n", i, length(candidates)))
  }
  rv <- tryCatch(get_individual(candidates[i]), error = function(e) {
    message("  failed ", candidates[i], ": ", conditionMessage(e))
    failed_people <<- c(failed_people, candidates[i])
    NULL
  })
  if (!is.null(rv)) {
    rows <- c(rows, smf_rows(rv, format(Sys.time(), "%Y-%m-%d", tz = "UTC")))
  }
  Sys.sleep(0.5) # be gentle with the register
}

out <- merge(firms[, c("frn", "firm_name", "company_id")], do.call(rbind, rows), by = "company_id")
out <- out[, c(
  "frn", "firm_name", "company_id", "smf_role", "name", "individual_id",
  "individual_ref", "start_date", "end_date", "scraped_on"
)]
out <- out[order(tolower(out$firm_name), out$smf_role, out$start_date, out$name), ]
write.csv(out, out_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")

cat(sprintf(
  "\nWrote %d SMF roles (%d current, %d historic) for %d firms to %s\n",
  nrow(out), sum(is.na(out$end_date)), sum(!is.na(out$end_date)),
  length(unique(out$frn)), out_file
))
no_smf <- setdiff(firms$frn, c(out$frn, failed_firms))
if (length(no_smf) > 0) cat("Firms with no SMF roles:", no_smf, "\n")
if (length(failed_firms) > 0 || length(failed_people) > 0) {
  if (length(failed_firms) > 0) cat("Firms that failed to load:", failed_firms, "\n")
  if (length(failed_people) > 0) cat("People that failed to load:", failed_people, "\n")
  # Fail so an automated run doesn't commit a partial result.
  quit(status = 1)
}
