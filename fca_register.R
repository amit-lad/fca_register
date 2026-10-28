# Proof of concept: scrape the FCA register.
#   - an individual id (003...) -> their name, reference number and SMF roles
#   - a company id (001...)     -> every individual at the firm, with their ids
#
# The register (register.fca.org.uk) is a Salesforce Lightning site, so the
# page HTML has no data in it. Each page fills itself in by POSTing to the Aura
# endpoint and calling an Apex initMethod. We call those methods directly.
#
# Usage:  Rscript fca_register.R 003Sk00000dUoqbIAC
#         Rscript fca_register.R 001b000000MfHhKAAV [current|previous|all]

library(httr2)
library(jsonlite)

# Rscript can start in the "C" locale, where write.csv mangles accented firm
# names (e.g. Covéa becomes Cov<U+00E9>a). Force a UTF-8 locale.
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

`%||%` <- function(x, y) if (is.null(x)) y else x

base_url <- "https://register.fca.org.uk"
user_agent <- "Mozilla/5.0 (compatible; fca-register-poc)"

# The Aura request needs the framework id (fwuid) and the app version. Both
# change when the FCA redeploys, so read them from the page HTML once per run.
aura_context_cache <- new.env()

get_aura_context <- function(page_path) {
  if (!is.null(aura_context_cache$ctx)) return(aura_context_cache$ctx)

  html <- request(paste0(base_url, page_path)) |>
    req_user_agent(user_agent) |>
    req_perform() |>
    resp_body_string() |>
    curl::curl_unescape()

  fwuid <- regmatches(html, regexpr('"fwuid":"[^"]+"', html))
  app_ver <- regmatches(
    html,
    regexpr('"APPLICATION@markup://siteforce:communityApp":"[^"]+"', html)
  )
  if (length(fwuid) == 0 || length(app_ver) == 0) {
    stop("Could not find Aura fwuid/app version in page HTML")
  }

  aura_context_cache$ctx <- list(
    mode = "PROD",
    fwuid = sub('.*:"([^"]+)"$', "\\1", fwuid),
    app = "siteforce:communityApp",
    loaded = setNames(
      list(sub('.*:"([^"]+)"$', "\\1", app_ver)),
      "APPLICATION@markup://siteforce:communityApp"
    ),
    dn = list(),
    globals = setNames(list(), character(0)),
    uad = TRUE
  )
}

# Call one Apex method through the Aura endpoint, as the page itself does,
# and return its returnValue.
aura_call <- function(page_path, descriptor, calling_descriptor, params) {
  message <- list(actions = list(list(
    id = "1;a",
    descriptor = descriptor,
    callingDescriptor = calling_descriptor,
    params = params
  )))

  resp <- request(paste0(base_url, "/s/sfsites/aura")) |>
    req_user_agent(user_agent) |>
    req_retry(max_tries = 3) |>
    req_body_form(
      message = toJSON(message, auto_unbox = TRUE),
      aura.context = toJSON(get_aura_context(page_path), auto_unbox = TRUE),
      aura.pageURI = page_path,
      aura.token = "null"
    ) |>
    req_perform()

  body <- resp_body_json(resp, simplifyVector = FALSE)
  action <- body$actions[[1]]
  if (action$state != "SUCCESS") {
    stop("Aura call failed: ", toJSON(action$error, auto_unbox = TRUE))
  }
  action$returnValue
}

get_individual <- function(individual_id) {
  aura_call(
    page_path = paste0("/s/individual?id=", individual_id),
    descriptor = "apex://ShPo_LEX_Reg_IndividualDetailsController/ACTION$initMethod",
    calling_descriptor = "markup://c:ShPo_LEX_Reg_IndividualPage",
    params = list(indiId = individual_id)
  )
}

get_firm <- function(company_id) {
  aura_call(
    page_path = paste0("/s/firm?id=", company_id),
    descriptor = "apex://ShPo_LEX_Reg_FirmDetailController/ACTION$initMethod",
    calling_descriptor = "markup://c:ShPo_LEX_Reg_FirmDetails",
    params = list(orgId = company_id)
  )
}

# Look up a firm by FRN using the register's firm search. Returns the firm's
# name and company id, or NAs if no firm has exactly that FRN.
find_firm_by_frn <- function(frn) {
  frn <- as.character(frn)
  rv <- aura_call(
    page_path = paste0("/s/search?q=", frn, "&type=Companies"),
    descriptor = "apex://ShPo_LEX_Reg_SearchController/ACTION$getFirmDetails",
    calling_descriptor = "markup://c:ShPo_LEX_Reg_SearchContainer",
    params = list(
      searchValues = list(frn),
      pageSize = "20",
      pageNo = 1,
      typeOfSearch = "Companies",
      location = list(longitude = NA, latitude = NA),
      orderBy = "",
      sectorCriteria = " includes ('Investment','Pensions','Mortgage')",
      # The website hides firms that are no longer authorised; we want them.
      hideUnauthFirm = FALSE,
      hideIntroARVal = FALSE,
      investmentTypes = list()
    )
  )
  # The search is free text, so keep only the result with this exact FRN.
  for (d in rv$accDetails) {
    if (identical(d$acc$ShGl_FRN__c, frn)) {
      return(list(frn = frn, firm_name = d$acc$Name, company_id = d$acc$Id))
    }
  }
  list(frn = frn, firm_name = NA_character_, company_id = NA_character_)
}

# One row per individual linked to the firm. "current" matches the default
# view on the firm's Individuals tab; "previous" is the Previous toggle.
parse_firm_individuals <- function(rv, which = c("current", "previous", "all")) {
  which <- match.arg(which)
  to_df <- function(people, status) {
    if (length(people) == 0) return(NULL)
    data.frame(
      name = vapply(people, `[[`, "", "Name"),
      reference_number = vapply(people, function(p) p$ShGl_IRN__c %||% NA_character_, ""),
      individual_id = vapply(people, `[[`, "", "Id"),
      roles = vapply(people, function(p) paste(
        vapply(p$Approved_Controlled_Functions__r, function(f) f$ShGl_InvolvementType__r$Name, ""),
        collapse = "; "
      ), ""),
      status = status
    )
  }
  out <- rbind(
    if (which != "previous") to_df(rv$currentIndividualsList, "current"),
    if (which != "current") to_df(rv$historicIndividualsList, "previous")
  )
  attr(out, "company") <- rv$accnt$Name
  out
}

# Pull out the person and their SMF roles as a list.
parse_smf_roles <- function(rv) {
  roles <- Filter(
    function(f) startsWith(f$ShGl_InvolvementType__r$Name, "SMF"),
    rv$ContFunctionsList
  )
  list(
    name = rv$cont$Name,
    reference_number = rv$cont$ShGl_IRN__c,
    individual_id = rv$cont$Id,
    roles = lapply(roles, function(f) list(
      role = f$ShGl_InvolvementType__r$Name,
      company = f$ShGl_Account__r$Name,
      company_reference_number = f$ShGl_Account__r$ShGl_FRN__c,
      company_id = f$ShGl_Account__r$Id
    ))
  )
}

print_individual <- function(x) {
  cat(sprintf("Name: %s\n", x$name))
  cat(sprintf("Reference Number: %s\n", x$reference_number))
  cat(sprintf("Individual id: %s\n", x$individual_id))
  for (i in seq_along(x$roles)) {
    r <- x$roles[[i]]
    cat(sprintf("Role %d: %s\n", i, r$role))
    cat(sprintf("Company %d: %s\n", i, r$company))
    cat(sprintf("Company reference number %d: %s\n", i, r$company_reference_number))
    cat(sprintf("Company id %d: %s\n", i, r$company_id))
  }
}

print_firm_individuals <- function(df) {
  cat(sprintf("Company: %s\n", attr(df, "company")))
  cat(sprintf("Individuals: %d\n\n", nrow(df)))
  # Pad the short columns and leave roles unpadded at the end, so one person
  # with a long list of roles doesn't push the table into wrapped blocks.
  cols <- c("name", "reference_number", "individual_id", "status")
  fixed <- lapply(cols, function(col) formatC(c(col, df[[col]]), width = -max(nchar(c(col, df[[col]])))))
  lines <- paste(do.call(paste, fixed), c("roles", df$roles))
  cat(lines, sep = "\n")
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  id <- if (length(args) > 0) args[1] else "003Sk00000dUoqbIAC"

  # Salesforce id prefixes: 003 = Contact (individual), 001 = Account (firm).
  if (startsWith(id, "001")) {
    which <- if (length(args) > 1) args[2] else "current"
    get_firm(id) |> parse_firm_individuals(which) |> print_firm_individuals()
  } else {
    get_individual(id) |> parse_smf_roles() |> print_individual()
  }
}
