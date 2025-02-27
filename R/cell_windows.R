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
  res = compute_windows_counts(cell_df, windows)
}

cell_windows_value_deci_bracket = function(cell_df) {
  cell_df %>%
    mutate(
      .value = ifelse(is.true(has_deci), paste0(substr(bracket,1,1),num_str,substr(bracket,2,2) ," "), "")
    )
}

cell_windows_compare = function(cell_df, windows) {
  restore.point("windows_compare")
  str_df = cells_to_window_str(cell_df, windows)
  count_df = str_df %>%
    left_join(comp_df, by=c("tabid","window_type","window_str")) %>%
    select(.tabid, cellid, window_type, count_iid, count_vid)
}


cell_windows_comparator = function(cell_df, windows, comp_lab="") {
  restore.point("compute_windows_counts")
  str_df = cells_to_window_str(cell_df, windows)
  comp_df = str_df %>%
    group_by(.tabid, window_type, window_str) %>%
    summarize(
      count = n_distinct(iid)
    )
  i

  field = paste0("in_", comp_lab)
  comp_df[[field]] = 1

  comp_df
}

# Helper function
cells_to_window_str = function(cell_df, windows) {

  if (!is.list(windows) | is.null(names(windows))) stop("windows must be a named list of windows")
  if (!has.col(cell_df,".value")) stop("Please specify a .value col for cell_df used to compute the windows strings")
  window_types = names(windows)
  df = cell_df %>%
    group_by(tabid, .tabid) %>%
    select(tabid, .tabid, cellid, )
  str_df = bind_rows(lapply(seq_along(window_types), function(i) {
    df %>% mutate(
      window_type=window_types[i],
      window_str = make_cell_window_str(.value, windows[[i]], row, col, .row_col)
    )
  })) %>%
    ungroup()
  str_df
}


make_cell_window_str = function(value,window, row, col,.row_col, na_val = "", sep="") {
  restore.point("make_cell_window_str")
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
