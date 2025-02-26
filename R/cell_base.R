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

cells_add_cell_base = function(cell_df, add_tests = TRUE) {
  restore.point("cell_df_to_cell_base_prof")
  cell_df = cell_list
  prod$vars

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
    rest_str = stri_replace_first_fixed(text_temp, num_str_raw, ""),
    other_num_str = find_number_in_string(rest_str,normalize = TRUE),
    num = suppressWarnings(as.numeric(num_str)),
    has_num = !is.na(num),
    has_deci = stri_detect_fixed(num_str, "."),
    num_deci = ifelse(!is.na(num),nchar(str.right.of(num_str,".", not.found=rep("", n()))), NA_integer_)
  )
  temp = cell_df %>% filter(tabid=="3")

  cell_df = cell_df %>% mutate(
    has_paren = stri_detect_fixed(text,"("),
    has_bracket = stri_detect_fixed(text,"["),
    has_curley = stri_detect_fixed(text,"{"),
    sig_star_str = find_stars_str(text),
    has_sig_star = sig_star_str != "",
    bracket = case_when(
      has_paren ~ "()",
      has_bracket ~ "[]",
      has_curley ~ "{}",
      TRUE ~ ""
    )
  )
  if (length( setdiff(prod$vars, colnames(cell_df)) ) > 0) {
    stop("Not all variables in cell_base generated.")
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
