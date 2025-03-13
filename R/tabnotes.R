extract_paren_type_from_tab_notes = function(tabnotes) {
  restore.point("extract_paren_type_from_tab_notes")
  notes = tolower(tabnotes)
  is_se = stringi::stri_detect_regex(notes, "standard error[ a-z]*parenthes")
  is_t = stringi::stri_detect_regex(notes, "[tz][- ](value|statistic)[ a-z]*parenthes")
  is_p = stringi::stri_detect_regex(notes, "[p][- ](value|statistic)[ a-z]*parenthes")
  
  paren_type = case_when(
    is_se ~ "se",
    is_t ~ "t",
    is_p ~ "p",
    TRUE ~ NA_character_
  )
  paren_type 
}
