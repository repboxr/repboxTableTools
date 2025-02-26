
examples = function() {
  text = c("My text : 23", "(-2.2)", "133.3 (2.3)", "362 344.2", "23 45.4")
  find_number_in_string(text,from_start=TRUE, "first")
  find_number_in_string(text,from_start=!TRUE, "first")
  find_number_in_string(text,from_start=!TRUE, "last")

}

na_val = function (x, val = 0) {
  x[is.na(x)] = val
  x
}


find_number_in_string <- function(text, mode="first", allow_pre = "[\\[\\(\\{, ]*", from_start=FALSE, normalize=FALSE) {
  restore.point("find_number_in_string")
  number_regex <- "(-\\s*)?\\d{1,3}(?:[ ,](?=\\d{3}(?!\\d))\\d{3})*(?:\\.\\d+)?"
  pattern = paste0(allow_pre,"(", number_regex,")")
  if (from_start)
    pattern = paste0("^", pattern)

  if (mode=="first") {
    res = stri_match_first_regex(text, pattern)[,2]
  } else if (mode=="last") {
    res = stri_match_last_regex(text, pattern)[,2]
  } else {
    stop("mode must be 'first' or 'last'.")
  }
  if (normalize) {
    res = stri_replace_all_regex(res,"[ ,]*","")
  }
  res
}
