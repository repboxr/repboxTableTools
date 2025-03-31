example = function() {
  library(repboxDB)
  library(repboxTableTools)
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"

  map_df = readRDS("~/repbox/projects_share/aejapp_1_2_7/fp/prod_art/map_reg_static/g25pe/v0/prod_df.Rds")

  tab_df = readRDS("~/repbox/projects_share/aejapp_1_2_7/fp/prod_art/tab_main/pdf-g2f/v0/prod_df.Rds")

  cell_df = readRDS("~/repbox/projects_share/aejapp_1_2_7/fp/prod_art/cell_base/pdf-g2f/v0/prod_df.Rds")

  parcels = repboxDB::repdb_load_parcels(project_dir, "stata_source")
  script_df = parcels$stata_source$script_source


  map_df = map_df_add_bg_color(map_df)
  map_df$cellid = stri_split_fixed(map_df$cell_ids, ",")
  map_cell = tidyr::unnest(map_df,cellid) %>% select(-cell_ids)

  extra_cell_df = map_cell %>%
    group_by(tabid, cellid) %>%
    summarize(
      bg_css = css_color_gradient(bg_color),
      help = paste0(unique(paste0(do_file,": ", code_line)), collapse="\n")
    )
  cell_df = cell_df %>%
    left

}


css_color_gradient <- function(colors) {
  if (length(colors) == 1) {
    # Only one color, so set it as the background color
    return(paste0("background-color: ", colors, ";"))
  } else {
    # More than one color, so create a linear gradient.
    # This example uses a 90 degree gradient; adjust the angle if needed.
    gradient <- paste0(colors, collapse = ", ")
    return(paste0("background: linear-gradient(45deg, ", gradient, ");"))
  }
}

map_df_add_bg_color = function(map_reg) {
  map_reg$bg_color = repboxHtml::cmd_ind_colors(NROW(map_reg))
  map_reg
}

static_tab_html = function(tab_main)

static_code_file_html = function(script_df) {


}
