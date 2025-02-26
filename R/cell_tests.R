cells_add_all_tests = function(cell_df) {
  cell_df = cell_df %>%
    test_cell_base_no_two_num() %>%
    test_cell_base_miss_bracket_below() %>%
    test_cell_base_miss_num_above_bracket()
}


flag_if_relevant = function(flag, relevant) {
  rows = !is.true(relevant)
  flag[rows] = NA
  flag
}



test_cell_base_no_two_num = function(df,...) {
  restore.point("test_cell_base_no_two_num")
  test_df = df %>% mutate(
    has_deci_other = is.true(stri_detect_fixed(other_num_str, ".")),
    flag_two_num = (!is.na(other_num_str)) %>%
      flag_if_relevant(has_num),
    flag_two_deci =  ((!is.na(other_num_str)) & (has_deci & has_deci_other)) %>%
      flag_if_relevant(has_deci)
  )
  temp = filter(test_df,tabid==3)
  test_df

}

test_cell_base_miss_bracket_below = function(df,...) {
  restore.point("test_cell_base_expect_bracket_below")
  #df = df %>% filter(tabid=="2")
  test_df = df %>%
    group_by(tabid, col) %>%
    arrange(tabid, row) %>%
    mutate(
      has_below_bracket = is.true(lead(bracket) != "" & (lead(row)==row+1) & bracket == "")) %>%
    group_by(tabid, row) %>%
    mutate(
      any_below_bracket = any(has_deci & has_below_bracket)
    ) %>%
    ungroup() %>%
    mutate(
      flag_miss_bracket_below = (!(has_below_bracket)) %>%
        flag_if_relevant(has_deci & any_below_bracket)
    )
  test_df

}

test_cell_base_miss_num_above_bracket = function(df,...) {
  restore.point("test_cell_base_expect_num_above_bracket")
  #df = df %>% filter(tabid=="2")
  test_df = df %>%
    group_by(tabid, col) %>%
    arrange(tabid, row) %>%
    mutate(
      has_above_num = is.true((lag(row)==row-1) & lag(has_num) & lag(bracket=="") & bracket != "" & has_deci )) %>%
    group_by(tabid, row) %>%
    mutate(
      any_above_num = any(has_above_num)
    ) %>%
    ungroup() %>%
    mutate(
      flag_miss_num_above_bracket = (!(has_above_num)) %>%
        flag_if_relevant(has_deci & any_above_num & bracket != "")
    )
  test_df

}
