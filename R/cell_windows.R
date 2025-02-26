# range in the sense of excel cell ranges
example = function() {
  library(repboxAI)
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"


  cell_df = repboxAI::rai_load_all_prod_df(project_dir, "cell_base")
  cell_df = cells_prepare_for_window(cell_df)
  cell_df = cell_df %>% filter(tabid=="1", iid==first(iid))

  windows = list(
    b1 = expand.grid(row=0:1, col=0),
    h1 = expand.grid(row=0, col=(-1):1)
  )
  cell_df = cells_windows_value_deci_bracket(cell_df)
  res = compute_windows_str(cell_df, windows)
}

cells_windows_value_deci_bracket = function(cell_df) {
  cell_df %>%
    mutate(
      .value = ifelse(is.true(has_deci), paste0(substr(bracket,1,1),num_str,substr(bracket,2,2) ," "), "")
    )
}

compute_windows_str = function(cell_df, windows) {
  restore.point("compute_windows_str")
  if (!is.list(windows) | is.null(names(windows))) stop("windows must be a named list of windows")
  if (!has.col(cell_df,".value")) stop("Please specify a .value col for cell_df used to compute the windows strings")

  window_labs = names(windows)
  cell_df = cell_df %>%
    group_by(vid, iid, tabid)

  str_vars = paste0("window_str_", names(windows))
  if (is.null(names(windows))) {
    str_vars = "window_str"
  }
  i = 1




  for (i in seq_along(windows)) {
    if (!is.null(names(windows)))
    cell_df = cell_df %>%
      mutate(!!str_vars[i] := cells_window_str(.value, windows[[i]], row, col, .row_col) )
  }

  agg_df = bind_rows(lapply(str_vars, function(str_var) {
    str_var_sym = sym(str_var)
    window_lab = str.right.of(str_var,"window_str_")
    agg_df = cell_df %>%
      group_by(tabid, !!str_var_sym) %>%
      summarize(
        window_lab = window_lab,
        count = n(),
        count_iid = n_distinct(iid),
        count_vid = n_distinct(vid),
        .groups = "drop"  # This explicitly drops all grouping
      ) %>%
      rename(window_str = !!str_var_sym)
    agg_df
  }))


  list(cell_df=cell_df, win_df = agg_df, window_labels = names(windows))
}



cells_prepare_for_window = function(cell_df) {
  .tabid = cell_df$tabid
  if ("iid" %in% colnames(cell_df)) {
    .tabid = paste0(cell_df$iid,"--", .tabid)
  } else if ("vid" %in% colnames(cell_df)) {
    .tabid = paste0(cell_df$iid,"--", .tabid)
  }
  cell_df$.tabid = .tabid
  cell_df$.row_col = paste0(cell_df$row, "-", cell_df$col)
  cell_df
}

cells_window_str = function(value,window, row, col,.row_col, na_val = "", sep="") {
  restore.point("cells_window_str")
  str=rep("", length(value))
  for (i in seq_len(NROW(window))) {
    if (window[i,1]==0 & window[i,2]==0) {
      str=paste0(str,sep,value)
      next
    }
    neigh_row_col = paste0(row+window[i,1],"-", col+window[i,2])
    inds = match(neigh_row_col, .row_col)
    new_val = value[inds]
    if (!is.na(na_val)) new_val = na_val(new_val, na_val)
    str = paste0(str, sep, new_val)
  }
  str
}


cells_neighbour_value = function(value,row_offset, col_offset, row, col,.row_col, na_val = NA) {
  restore.point("cells_neighbour_value")
  neigh_row_col = paste0(row-row_offset,"-", col-col_offset)
  inds = match(neigh_row_col, .row_col)
  res = value[inds]
  if (!is.na(na_val)) res = na_val(res, na_val)
  res
}

