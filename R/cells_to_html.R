# Functions to show cell test results as HTML

cell_df_to_simple_tabhtml = function(cell_df) {
  restore.point("cell_df_to_simple_tabhtml")
  html_df = cell_df %>%
    group_by(cellid) %>%
    mutate(td_code = paste0(
      '<td id = "cell-', cellid,'"',
      ' class = "row-',row,' col-', col,'" ',
      ifelse(is.true(rowspan>1),paste0(' rowspan="', rowspan,'" '),""),
      ifelse(is.true(colspan>1),paste0(' colspan="', colspan,'" '),""),
      '>', content,'</td>')
    ) %>%
    group_by(row) %>%
    arrange(col) %>%
    summarize(
      tr_code = paste0('<tr>', paste0(td_code, collapse="\n  "),'</tr>')
    )
  tabhtml = paste0('<table>\n',paste0(html_df$tr_code, collapse="\n  "), '\n</table>')
  tabhtml
}



cells_to_tabhtml = function(cell_df, tabid_prefix="tab-", add_flags = FALSE) {
  restore.point("cells_to_tabhtml")

  if (add_flags) {
    cell_df = cells_add_flag_cols_html(cell_df)
  }

  if (!has_col(cell_df, "text")) cell_df$text = cell_df$content
  if (!has_col(cell_df, "class")) cell_df$class = ""
  if (!has_col(cell_df, "title")) cell_df$title = ""
  if (!has_col(cell_df, "style")) cell_df$style = ""
  if (!has_col(cell_df, "tab_class")) cell_df$tab_class = ""

  names(cell_df)
  tab_df = cell_df %>%
    group_by(tabid, cellid) %>%
    mutate(td_code = paste0(
      '<td id = "cell-', cellid,'"',
      ' class = "row-',row,' col-', col, ' ', class,'" ',
      ' style = "', style,'" ',
      ' title = "', paste0(cellid, "\nrow ", row, " col ",col,"\n", title,'" '),
      ifelse(is.true(rowspan>1),paste0(' rowspan="', rowspan,'" '),""),
      ifelse(is.true(colspan>1),paste0(' colspan="', colspan,'" '),""),
      '>', text,'</td>')
    ) %>%
    group_by(tabid, row, tab_class) %>%
    arrange(tabid, col) %>%
    summarize(
      tr_code = paste0('<tr>', paste0(td_code, collapse="\n  "),'</tr>')
    ) %>%
    group_by(tabid, tab_class) %>%
    summarize(
      tabhtml = paste0('<table id="', tabid_prefix,first(tabid),'" class = "', first(tab_class),'">\n',paste0(tr_code, collapse="\n  "), '\n</table>')
    )

  tab_df
}

cells_add_flag_cols_html = function(df, just_cols=NULL) {
  restore.point("flag_cols_to_html_spec")
  flag_cols = get_flag_cols(df, just_cols)
  tests = stri_sub(flag_cols, 6)
  bg_colors = named_bg_colors(tests)

  test_str = flag_cols_to_str(df, flag_cols)
  uni_test_str = unique(test_str)
  uni_test_class = stri_replace_all_fixed(uni_test_str, "-"," ")
  names(uni_test_class) = uni_test_str

  uni_test_li = stri_split_fixed(uni_test_str,"-")
  names(uni_test_li) = uni_test_str

  uni_bg_color = sapply(uni_test_li, function(els) {
    html_color_grad(bg_colors[els])
  })
  html_cell_df = df %>%
    select(cellid) %>%
    mutate(
      test_str = test_str,
      class = uni_test_class[test_str],
      bg_color = uni_bg_color[test_str],
      title = stri_replace_all_fixed(test_str, "-","\n"),
      style = bg_color
    )
}

get_flag_cols = function(df, just_cols = NULL) {
  cols = names(df)
  flag_cols = cols[startsWith(cols, "flag_")]
  if (!is.null(just_cols)) {
    flag_cols = intersect(flag_cols, just_cols)
  }
  flag_cols
}

flag_cols_to_str = function(df, just_cols = NULL) {
  flag_cols = get_flag_cols(df, just_cols)
  test_str = rep("", NROW(df))
  for (col in flag_cols) {
    test_name = stri_sub(col, 6)
    has_test = is.true(df[[col]]==1)
    test_str = case_when(
      !has_test ~ test_str,
      test_str == "" ~ test_name,
      TRUE ~ paste0(test_str, "-", col)
    )
  }
  test_str

}
