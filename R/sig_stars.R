
find_stars_str = function(big_str) {
  restore.point("find_stars_str")
  if (length(big_str)==0) return(NULL)
  digit_pos = stringi::stri_locate_last_regex(big_str, "[0-9]")[,1]
  digit_pos[is.na(digit_pos)] = 0
  right_str =  substring(big_str,digit_pos+1)
  right_str = gsub(" ", "", right_str)
  as.vector(stringi::stri_match_first_regex(right_str, "[*]+")) %>% na.val("")
}
