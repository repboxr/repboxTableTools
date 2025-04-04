# AI often combines several numbers into one table cell, which I rather would like to have split

example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  tab_df = readRDS(fLile.path(project_dir,"mistral_ocr", "tab_df.Rds"))
  head(tab_df$cell_df[[1]])


  cell_df = tab_df$cell_df[[1]]

  # mistral
  cell_df$content = cell_df$inner_html
  row_df = split_html_latex_multiline(cell_df$content)
  row_df
  new_df = cell_df_split_rows_and_cols(cell_df, row_df)

}

cell_base_split_multi_num = function(cell_base,add_tests=TRUE) {
  restore.point("cell_base_split_multi_num")
  if (!any(cell_base$has_multi_num)) return(cell_base)
  cell_df_li = split(cell_base, cell_base$tabid)

  li = lapply(cell_df_li, function(cell_df) {
    if (!any(cell_df$has_multi_num)) return(cell_df)
    row_df = cell_df %>% select(row1 = multi_num_str)
    cell_df = cell_df_split_rows_and_cols(cell_df, row_df)
    cell_df$text = cell_df$content
    cell_df
  })
  cell_base = bind_rows(li)
  cell_base = cells_add_cell_base(cell_base,add_tests = add_tests, split_multi_num=FALSE)
  cell_base
}

# Split rows and / or cols and add corresponding extra cells
# we will only adapt the fields, content, cellid, row and col.
cell_df_split_rows_and_cols = function(cell_df,row_df, col_split_char="|") {
  restore.point("cell_df_split_rows_and_cols")
  num_cr = NCOL(row_df)
  # split cell_df into multiple rows
  new_li = vector("list",(num_cr-1)*NROW(cell_df))

  df = cell_df
  if (num_cr>1) {
    new_ind = 0
    cr = num_cr
    for (cr in 2:num_cr) {
      inds = which(row_df[[cr]]!= "")
      rows = sort(unique(df$row[inds]))
      r = rows[1]
      for (r in rows) {
        row_inds = which(df$row == r)
        new_df = df[row_inds, ]
        new_df$row = new_df$row+ cr / (1+num_cr)
        new_df$content = row_df[row_inds, cr]
        new_ind = new_ind+1
        new_li[[new_ind]] = new_df
      }
    }
    df$content = row_df$row1
    df = bind_rows(c(list(df),new_li))
    row_vals = sort(unique(df$row))

    df = df %>%
      mutate(row = match(row, row_vals)) %>%
      arrange(row, col) %>%
      mutate(cellid = paste0("cell-",tabid, "_", 1:n()))

  } else if (num_cr==1) {
    df$content = row_df$row1
  }


  # Now split columns
  col_mat = stringi::stri_split_fixed(df$content, col_split_char, simplify = TRUE)
  num_cc = ncol(col_mat)
  # split df into multiple columns
  new_li = vector("list",(num_cc-1)*NROW(df))

  if (num_cc > 1) {
    new_ind = 0
    cc = num_cc
    for (cc in num_cc:2) {
      inds = which(col_mat[,cc]!= "")
      cols = rev(sort(unique(df$col[inds])))
      c = cols[1]
      for (c in cols) {
        col_inds = which(df$col == c)
        new_df = df[col_inds, ]
        new_df$col = new_df$col+ cc / (1+num_cc)
        new_df$content = col_mat[col_inds, cc]
        new_ind = new_ind+1
        new_li[[new_ind]] = new_df
      }
    }
    df$content = col_mat[,1]
    df = bind_rows(c(list(df),new_li))
    col_vals = sort(unique(df$col))

    df = df %>%
      mutate(col = match(col, col_vals)) %>%
      arrange(row, col) %>%
      mutate(cellid = paste0("cell-",tabid, "_", 1:n()))

  } else if (num_cc==1) {
    df$content = col_mat[,1]
  }
  df
}
