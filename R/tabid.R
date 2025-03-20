
example = function() {
  tabtitle <- c(
    "Table 1-Descriptive Statistics",
    "Table 1-A Descriptiobe Statistics",
    "Table A: Regression Results",
    "Table 1.2 Summary",
    "Table 1.A-Summary",
    "TABLE 1-2: Summary",
    "TABLE IX-A Summary",
    "Appendix Table 4 Summary"
  )

  tabname = tabtitle_to_tabname(tabtitle)
  tabname
  tabname_to_tabid(tabname)
}

tabtitle_to_tabname <- function(titles) {
  # This regex now accepts an optional "Appendix " before "Table ".
  # Breakdown:
  #   (?i)                             => case-insensitive
  #   ((?:Appendix\s+)?Table\s+)       => group 1: either "Table " or "Appendix Table "
  #   ([0-9A-Za-z]+(?:\.[0-9A-Za-z]+)?)  => group 2: the base identifier (e.g., "1", "1.2", "A", "IX")
  #   (?:[-:]\s*([0-9A-Za-z])(?![0-9A-Za-z]))? => optional group capturing a single alphanumeric extension
  pattern <- "^(?i)((?:Appendix\\s+)?Table\\s+)([0-9A-Za-z]+(?:\\.[0-9A-Za-z]+)?)(?:[-:]\\s*([0-9A-Za-z])(?![0-9A-Za-z]))?"

  # Apply the regex vectorized over the input titles.
  m <- stri_match_first_regex(titles, pattern)
  # m is a matrix with:
  #   Column 1: full match,
  #   Column 2: prefix ("Table " or "Appendix Table "),
  #   Column 3: base identifier,
  #   Column 4: optional single-character extension.

  # For rows that don't match, return the original title.
  no_match <- is.na(m[,1])

  # Base result: prefix plus base identifier.
  result <- stri_c(m[,2], m[,3])

  # Identify rows with an extension.
  has_ext <- !is.na(m[,4]) & m[,4] != ""

  # Check if both the base and the extension are purely numeric.
  base_numeric <- stri_detect_regex(m[,3], "^[0-9]+$")
  ext_numeric  <- stri_detect_regex(m[,4], "^[0-9]+$")

  # If both are numeric, join with a period; otherwise, with a dash.
  combined_ext <- ifelse(base_numeric & ext_numeric,
                         stri_c(m[,3], ".", m[,4]),
                         stri_c(m[,3], "-", m[,4]))

  # For rows with an extension, update the result.
  result[has_ext] <- stri_c(m[has_ext, 2], combined_ext[has_ext])

  # For rows that did not match the pattern, keep the original title.
  result[no_match] <- titles[no_match]

  result
}

tabname_to_tabid = function (tabname){
  restore.point("tabname_to_tabid")
  tabid = str.right.of(tabname, "able ") %>% trimws()
  tabid = gsub("\\.$", "", tabid) %>% trimws()
  rows = which(startsWith(tolower(tabname), "appendix"))
  tabid[rows] = paste0("app.", tabid[rows])
  tabid = stringi::stri_replace_all_regex(tabid, "[^a-zA-Z0-9_]",
                                          "")
  tabid = stringi::stri_replace_all_fixed(tabid, "TABLE", "")
  tabid
}

tabtitle_to_tabid = function(tabtitle) {
  tabname = tabtitle_to_tabname(tabtitle)
  tabname_to_tabid(tabname)
}


tabid_normalize = function(tabid) {
  restore.point("tabid_normalize")
  tabid <- stri_replace_all_regex(tabid, "([0-9])[ ]*\\.[ ]*([0-9])", "$1_$2")
  tabid <- stri_replace_all_regex(tabid, "[^a-zA-Z0-9_]", "")
  tabid
}

example = function() {
  tabid_to_otabid(c("I", "IIA", "IIB", "III"))
  tabid_to_otabid(c("i", "vA", "vB", "vIII"))
  tabid_to_otabid(c("I", "2", "3", "4"))

}

tabid_to_otabid <- function(tabid, digits = 3) {
  restore.point("tabid_to_otabid")
  if (length(tabid)==0) return(tabid)
  # Extract the longest contiguous roman numeral prefix (case-insensitive)
  roman_pos <- stri_locate_first_regex(tabid, "^(?i)[IVX]+")
  roman_rows = !is.na(roman_pos[,1])
  if (length(roman_rows)>=0.5*length(tabid)) {
    val = as.integer(as.roman(stri_sub(tabid[roman_rows],roman_pos[roman_rows,1], roman_pos[roman_rows,2])))
    tabid[roman_rows] = paste0(val, stri_sub(tabid[roman_rows],roman_pos[roman_rows,2]+1))
  }


  # Vectorized extraction of the first sequence of digits
  matches <- stri_extract_first_regex(tabid, "[0-9]+")

  # For each non-NA match, create a padded version
  padded <- ifelse(is.na(matches), NA, sprintf(paste0("%0", digits, "d"), as.numeric(matches)))

  # Replace the first occurrence of digits with the padded number where a match was found
  result <- ifelse(is.na(matches), tabid, stri_replace_first_regex(tabid, "[0-9]+", padded))

  return(result)
}



