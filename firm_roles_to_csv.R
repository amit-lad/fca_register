# For every firm in firms.csv, list the roles held by individuals currently
# involved with the firm, one row per person per role.
#
# Every current role is included (not only SMF ones), and a role listed twice
# for the same person is kept twice, as the register returns it. Each row's
# scraped_on column is the UTC date its firm was fetched from the register.
#
# Usage:  Rscript firm_roles_to_csv.R [firms.csv] [smf_roles.csv]

source("fca_register.R")

args <- commandArgs(trailingOnly = TRUE)
in_file <- if (length(args) > 0) args[1] else "firms.csv"
out_file <- if (length(args) > 1) args[2] else "smf_roles.csv"

firms <- read.csv(in_file, colClasses = "character", encoding = "UTF-8")
firms <- firms[firms$company_id != "", ]

empty_roles <- data.frame(
  smf_role = character(0), name = character(0),
  individual_id = character(0), individual_ref = character(0)
)

# One row per (person, role) for a firm's current individuals.
current_roles <- function(rv) {
  rows <- lapply(rv$currentIndividualsList, function(p) {
    roles <- vapply(
      p$Approved_Controlled_Functions__r,
      function(f) f$ShGl_InvolvementType__r$Name, ""
    )
    if (length(roles) == 0) return(NULL)
    data.frame(
      smf_role = roles,
      name = p$Name,
      individual_id = p$Id,
      individual_ref = p$ShGl_IRN__c %||% NA_character_
    )
  })
  do.call(rbind, c(list(empty_roles), rows))
}

failed <- character(0)
results <- lapply(seq_len(nrow(firms)), function(i) {
  firm <- firms[i, ]
  cat(sprintf("[%d/%d] %s %s\n", i, nrow(firms), firm$frn, firm$firm_name))
  roles <- tryCatch(
    current_roles(get_firm(firm$company_id)),
    error = function(e) {
      message("  failed: ", conditionMessage(e))
      failed <<- c(failed, firm$frn)
      empty_roles
    }
  )
  Sys.sleep(0.5) # be gentle with the register
  if (nrow(roles) == 0) return(NULL)
  cbind(
    firm[rep(1, nrow(roles)), c("frn", "firm_name", "company_id")],
    roles,
    scraped_on = format(Sys.time(), "%Y-%m-%d", tz = "UTC")
  )
})

out <- do.call(rbind, results)
write.csv(out, out_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")

no_people <- setdiff(firms$frn, c(out$frn, failed))
cat(sprintf(
  "\nWrote %d rows for %d firms to %s\n",
  nrow(out), length(unique(out$frn)), out_file
))
if (length(no_people) > 0) cat("Firms with no current individuals:", no_people, "\n")
if (length(failed) > 0) {
  cat("Firms that failed to load:", failed, "\n")
  # Fail so an automated run doesn't commit a partial result.
  quit(status = 1)
}
