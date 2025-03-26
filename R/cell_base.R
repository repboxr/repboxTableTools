cells_update_cell_base = function(cell_df, rows, add_tests=TRUE) {
  if (!cells_have_cell_base(cell_df)) {
    cell_df = cells_add_cell_base(cell_df, add_tests=FALSE)
  }
  scells = cell_df[rows,]
  scells =  cells_add_cell_base(scells, add_tests=FALSE)
  cell_df[rows,] = scells
  # Need to add tests for complete cell df
  if (add_tests) {
    cell_df = cells_add_all_tests(cell_df)
  }
  cell_df
}

cells_have_cell_base = function(cell_df) {
  all(has.col(cell_df,c("num_str","has_deci","bracket")))
}

cells_add_cell_base = function(cell_df, add_tests = TRUE, split_multi_num = FALSE) {
  restore.point("cells_add_cell_base")

  # We only extract number if it is at the beginning
  # of in a bracket
  fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
  fp_start = paste0("^", fp)
  cell_df = cell_df %>% mutate(
    nchar = nchar(text),
    nchar_letters = stri_count_regex(text, "[a-zA-Z]"),
    # A num column should not start with any other text strings
    text_temp = text %>%
      stri_replace_all_fixed("−","-"),
    num_str_raw = find_number_in_string(text_temp, from_start=TRUE),
    num_str = stri_replace_all_regex(num_str_raw,"[ ,]*",""),
    multi_num_str = create_sep_num_string(text_temp,"|"),
    has_multi_num = stri_detect_fixed(multi_num_str, "|"),
    num = suppressWarnings(as.numeric(num_str)),
    has_num = !is.na(num),
    has_deci = stri_detect_fixed(num_str, "."),
    num_deci = ifelse(!is.na(num),nchar(str.right.of(num_str,".", not.found=rep("", n()))), NA_integer_)
  )
  temp = cell_df %>% filter(tabid=="3")

  cell_df = cell_df %>% mutate(
    has_paren = stri_detect_regex(text, "\\(.*\\d+.*\\)"),
    has_bracket = stri_detect_regex(text, "\\[.*\\d+.*\\]"),
    has_curley = stri_detect_regex(text, "\\{.*\\d+.*\\}"),
    sig_star_str = find_stars_str(text),
    has_sig_star = sig_star_str != "",
    bracket = case_when(
      has_paren ~ "()",
      has_bracket ~ "[]",
      has_curley ~ "{}",
      TRUE ~ ""
    )
  )
  # if (length( setdiff(prod$vars, colnames(cell_df)) ) > 0) {
  #   stop("Not all variables in cell_base generated.")
  # }
  if (split_multi_num) {
    cell_df = cell_base_split_multi_num(cell_df, add_tests = add_tests)
    #cat("split_multi_num")
    restore.point("post_split_multi_num")
  }

  if (add_tests) {
    cell_df = cells_add_all_tests(cell_df)
  }
  cell_df
  #df_to_prod_df(cell_df,  prod)

}



find_stars_str = function (big_str) {
  restore.point("find_stars_str")
  if (length(big_str) == 0)
    return(NULL)
  digit_pos = stringi::stri_locate_last_regex(big_str, "[0-9]")[,
                                                                1]
  digit_pos[is.na(digit_pos)] = 0
  right_str = substring(big_str, digit_pos + 1)
  right_str = gsub(" ", "", right_str)
  as.vector(stringi::stri_match_first_regex(right_str, "[*]+")) %>%
    na.val("")
}

create_sep_num_string = function(str,num_sep="|",allow_pre = "[\\[\\(\\{, ]*", from_start=TRUE, normalize=FALSE) {
  restore.point("create_sep_num_string")

  # Regex for a number (with optional leading negative sign, thousand separators, and optional decimal)
  number_regex <- "(-\\s*)?\\d{1,3}(?:[ ,](?=\\d{3}(?!\\d))\\d{3})*(?:\\.\\d+)?"

  # Full pattern: capture the optional allow_pre and the number
  pattern <- paste0(allow_pre, "(", number_regex, ")")

  # Place num_sep before the allow_pre prefix, then allow_pre (if any) and the number
  replacement <- paste0(num_sep, "$0")

  org_str = str

  # Apply replacement to all elements of the string vector
  str = stringi::stri_replace_all_regex(str, pattern, replacement)
  str = stri_replace_first_fixed(str, num_sep, "")

  # Only assume multistring if starts as proper number
  if (from_start) {
    org_inds = !stri_detect_regex(org_str, paste0("^",pattern))
    str[org_inds] = org_str[org_inds]
  }

  str
}

