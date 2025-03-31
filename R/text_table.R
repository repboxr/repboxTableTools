
text_table = function(df, header = cols, col_sep = "|", cols=names(df), header_line_char="-", header_line_col_sep="+") {
  restore.point("text_table")
  txt = rep("", NROW(df)+2)
  i = 1
  for (i in seq_along(cols)) {
    col = cols[[i]]
    val = as.character(df[[col]])
    nc = max(nchar(c(header[i],val)))
    val_str = stri_pad_right(val, nc+1)
    header_str = stri_pad_right(header[i],nc+1)
    add_left = (i>1)
    if (add_left) {
      val_str = paste0(col_sep, " ", val_str)
      header_str = paste0(col_sep, " ", header_str)
    }
    header_line = NULL
    if (nchar(header_line_char)>0) {
      header_line = paste0(rep(header_line_char,nc+1+add_left), collapse="")
      if (add_left) {
        header_line = paste0(header_line_col_sep, header_line)
      }
    }
    txt = paste0(txt, c(header_str, header_line, val_str))
  }
  paste0(txt, collapse="\n")
}

