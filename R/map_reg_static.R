example = function() {
  library(repboxDB)
  library(repboxTableTools)
  library(repboxAI)
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  project_dir = "~/repbox/projects_share/aejmic_10_1_1"
  rtt_map_reg_static_report(project_dir)
  rstudioapi::filesPaneNavigate(project_dir)

  project_dirs = repboxExplore::get_project_dirs("~/repbox/projects_share")
  project_dirs
  for (project_dir in project_dirs) {
    rtt_map_reg_static_report(project_dir)
    #rtt_copy_extra_files_to_report(project_dir)
  }
}

rtt_copy_extra_files_to_report = function(project_dir) {
  restore.point("rtt_copy_extra_files_to_report")

  dest_dir = file.path(project_dir, "reports")
  file = system.file("www/repbox.css",package = "repboxHtml")
  file.copy(file, file.path(dest_dir, basename(file)),overwrite = TRUE)

  file = system.file("www/static_code_links.js",package = "repboxTableTools")
  file.copy(file, file.path(dest_dir, basename(file)),overwrite = TRUE)


}

rtt_map_reg_static_report = function(project_dir, ver_dir=NULL, doc_type="art") {
  restore.point("rtt_map_reg_static_report")
  if (is.null(ver_dir)) {
    fp_dir = project_dir_to_fp_dir(project_dir, doc_type)
    ver_dir = fp_pick_prod_ver(fp_dir, "map_reg_static")$ver_dir
  }
  if (length(ver_dir)==0) return(NULL)

  df_li = rtt_load_map_reg_static_df_li(ver_dir)
  map_df = df_li$map_df; script_df = df_li$script_df


  # Make table pane
  tab_df = rtt_map_reg_static_tab_df(ver_dir,df_li)
  contents = paste0("<h5>", tab_df$tabtitle,"</h5>",  tab_df$show_tabhtml, "<br><p>", tab_df$tabnotes,"</p>")
  tab_panels = rtt_tabset_panel("tabs",tabids = paste0("tab-",tab_df$tabid),tabnames = paste0("Table ", tab_df$tabid),contents = contents)


  # Make code pane
  names(script_df)
  code_df = script_df %>%
    group_by(script_file, script_num) %>%
    mutate(
      code = list(sep.lines(text))
    ) %>%
    select(script_file, script_num, code) %>%
    ungroup() %>%
    group_by(script_file, script_num) %>%
    tidyr::unnest(code) %>%
    mutate(line = seq_len(n())) %>%
    ungroup() %>%
    mutate(
      code_id = paste0("L", line, "___", script_num),
      html_row = paste0(
        '<tr><td class="code-line-td">', line,"</td>",
        '<td><pre class="script-pre"><code id="', code_id,'" class="noerr-line" title="">',code,'</code></pre></td></tr>'
      )
    )
  code_html_df = code_df %>%
    group_by(script_file, script_num) %>%
    summarize(
      contents = paste0('<table class="code-tab">', paste0(html_row, collapse="\n"),"</table>")
    )
  code_panels = rtt_tabset_panel("script_tabs",tabids = paste0("script_tab-",code_html_df$script_num),tabnames = paste0(code_html_df$script_file),contents = code_html_df$contents)


  library(shiny)
  body_ui = fluidPage(
    div(class="row",style="height: 100vh;",
        div(id="script-col-div", class="col-sm-6", style="overflow-y: scroll; height:100%; padding: 5px",HTML(code_panels)),
        div(id="tabs-col-div",class="col-sm-6", style="overflow-y: scroll; height:100%; padding: 5px", HTML(tab_panels))
    ),
  )
  body = as.character(body_ui) %>% merge.lines()
  html = rtt_html_page(body, "Mapping Regressions to Static Code")
  proc_id = fp_ver_dir_to_proc_id(ver_dir)
  dest_dir = file.path(project_dir, "reports")
  html_file = file.path(dest_dir,paste0("map_static_reg-", proc_id,".html"))
  # Copy script and css files

  rtt_copy_extra_files_to_report(project_dir)


  writeLines(html,html_file)
  rstudioapi::filesPaneNavigate(file.path(project_dir, "reports"))

}



rtt_load_map_reg_static_df_li = function(ver_dir) {
  restore.point("rtt_load_map_reg_static_df")
  pru = fp_load_pru(ver_dir)
  if (is.null(pru)) return(NULL)
  tab_info = pru$tab_main_info
  map_df = fp_load_prod_df(ver_dir)
  map_df = rename.col(map_df, "do_file", "script_file")
  tab_df = fp_load_prod_df(tab_info$ver_dir)

  fp_dir = fp_ver_dir_to_fp_dir(ver_dir)
  cell_ver_dir = fp_pick_prod_ver(fp_dir, "cell_base", proc_id = tab_info$proc_id)
  cell_df = fp_load_prod_df(cell_ver_dir$ver_dir)

  project_dir = dirname(dirname(fp_dir))

  parcels = repboxDB::repdb_load_parcels(project_dir, "stata_source")
  script_df = parcels$stata_source$script_source
  script_df = repboxAI::script_df_shorten_file(script_df)

  list(map_df=map_df, tab_df=tab_df, cell_df = cell_df, script_df=script_df)
}

rtt_map_reg_static_tab_df = function(ver_dir, df_li=rtt_load_map_reg_static_df(ver_dir)) {
  restore.point("rtt_map_reg_static_tab_df")
  copy.into.env(df_li)


  script_df = df_li$script_df
  map_df = map_df_add_bg_color(map_df)
  map_df$cellid = stri_split_fixed(map_df$cell_ids, ",")
  map_cell = tidyr::unnest(map_df,cellid) %>% select(-cell_ids)

  extra_cell_df = map_cell %>%
    left_join(select(script_df, script_file,script_num), by="script_file") %>%
    group_by(tabid, cellid) %>%
    summarize(
      script_num = first(script_num),
      code_line = first(code_line),
      style = html_color_grad(bg_color),
      title = paste0(unique(paste0("reg_ind: ",reg_ind,"\nL", code_line, ": ", script_file)), collapse="\n"),
      class = "arttabcell"
    )

  cell_df = cell_df %>%
    left_join_overwrite(extra_cell_df, by = c("tabid", "cellid")) %>%
    mutate(style = na_val(style, ""), title = na_val(title,""))


  new_tab_df = cells_to_tabhtml(cell_df, tab_class="table-mini", data_cols = c("script_num","code_line")) %>% rename(show_tabhtml = tabhtml)

  tab_df = tab_df %>% left_join(new_tab_df, by = "tabid")
  tab_df
}

map_df_add_bg_color = function(map_reg) {
  map_reg$bg_color = cell_bg_colors(NROW(map_reg))
  map_reg
}

rtt_static_code_file_html = function(script_df) {


}
