# Look up each FRN in a text file (one per line) and write a CSV of
# FRN, firm name and company id.
#
# Usage:  Rscript frn_to_csv.R [frns.txt] [firms.csv]

source("fca_register.R")

args <- commandArgs(trailingOnly = TRUE)
in_file <- if (length(args) > 0) args[1] else "frns.txt"
out_file <- if (length(args) > 1) args[2] else "firms.csv"

frns <- trimws(readLines(in_file))
frns <- frns[frns != ""]

rows <- lapply(seq_along(frns), function(i) {
  cat(sprintf("[%d/%d] %s\n", i, length(frns), frns[i]))
  row <- tryCatch(
    find_firm_by_frn(frns[i]),
    error = function(e) {
      message("  failed: ", conditionMessage(e))
      list(frn = frns[i], firm_name = NA_character_, company_id = NA_character_)
    }
  )
  Sys.sleep(0.5) # be gentle with the register
  row
})

firms <- do.call(rbind, lapply(rows, as.data.frame))
write.csv(firms, out_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")

missing <- firms$frn[is.na(firms$company_id)]
cat(sprintf("\nWrote %d rows to %s (%d not found)\n", nrow(firms), out_file, length(missing)))
if (length(missing) > 0) cat("Not found:", missing, "\n")
