example = function() {
  run_dir = "~/repbox/projects_share/aejapp_1_2_4/rai/prod_runs/cell_base/tab_html-n-g2f-0-pdf-1/r0"
  cell_df = readRDS(file.path(run_dir, "prod_df.Rds"))
}

cells_check_single_tab = function(cell_df) {
  if (n_distinct(cell_df$tabid)>0) stop("cell_df contains more than one table.")
  TRUE
}

cells_replace_tab = function(cell_df, tab_cell_df) {
  cell_df %>%
    anti_join(tab_cell_df, by="tabid") %>%
    bind_rows(tab_cell_df)
}

cells_insert_empty_col = function(cell_df, col, text="", colspan=1) {
  cell_df_check_single_tab(cell_df)
  num_rows = max(cell_df$row)
  inds = cell_df$col >= col
  cell_df$col[inds] = cell_df$col[inds]+1
  ncell_df = tibble(tabid=first(cell_df$tabid),otabid=first(cell_df$otabid), cellid="", row = seq_along(num_rows), col=col, text=text,colspan=1, rowspan=1)
  bind_rows(cell_df, ncell_df) %>% arrange(row, col)


}
