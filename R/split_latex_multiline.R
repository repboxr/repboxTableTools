# Some rather specific functions that are currently necessary to transform Mistral AI OCR
# tables to our desired format. These regex intensive functions have been mainly witten
# by ChatGPT



example = function() {
  # Example usage:
  html_vec <- c(
    "Very close or close to own ethnic or racial group",
    "pre text <span class=\"math inline\">\\(\\begin{gathered} 0.63 \\\\ (0.48) \\end{gathered}\\)</span>",
    "1,126",
    "<span class=\"math inline\">\\[\\begin{gathered} 0.57 & 23 \\\\ (0.50) \\end{gathered}\\]</span>post",
    "749",
    "Example with dollar signs: <span class=\"math inline\">$$\\begin{gathered} 0.75 \\\\ (0.35) \\end{gathered}$$</span>"
  )

  html_vec = c("<span class=\"math inline\">$2$</span>")

  html_cell_to_df(html_vec)


  # Example usage:
  latex_vec <- c(
    "\\begin{gathered}  & 0.90 \\\\  & (0.29) \\end{gathered}\\",
    "",
    "E = mc^2"
  )
  split_latex_multiline(latex_vec)

}

split_latex_multiline <- function(latex_vec, as_matrix=FALSE, remove_empty_cols=TRUE) {
  restore.point("split_latx_multiline")
  # Define supported multiline environments
  envs <- c(
    "aligned",
    "gathered",
    "align(?:\\*?)",
    "gather(?:\\*?)",
    "multline(?:\\*?)",
    "split",
    "flalign(?:\\*?)",
    "alignat(?:\\*?)",
    "eqnarray")
  env_pattern <- paste0(envs, collapse = "|")

  # Detect multiline formulas (vectorized)
  is_multiline <- stringi::stri_detect_regex(
    latex_vec,
    paste0("\\\\begin\\{(", env_pattern, ")\\}")
  )

  # For multiline formulas, remove \begin{...} and \end{...}; for others, keep as is.
  processed <- ifelse(is_multiline,
                      stringi::stri_replace_all_regex(
                        latex_vec,
                        paste0("^\\\\begin\\{(?:", env_pattern, ")\\}\\s*"),
                        ""
                      ),
                      latex_vec)
  processed <- ifelse(is_multiline,
                      stringi::stri_replace_all_regex(
                        processed,
                        paste0("\\s*\\\\end\\{(?:", env_pattern, ")\\}\\s*\\\\?$"),
                        ""
                      ),
                      processed)

  # Split on LaTeX newline command (\\) using vectorized splitting (simplify=TRUE returns a matrix)
  split_matrix <- stringi::stri_split_regex(processed, "\\\\\\\\", simplify = TRUE)

  # Trim whitespace from each element and replace ampersand (with surrounding spaces) with a pipe
  split_matrix[,] <- stringi::stri_trim_both(split_matrix)
  split_matrix[,] <- stringi::stri_replace_all_regex(split_matrix, "\\s*&\\s*", "|")

  # Ensure exactly 3 columns: if there are fewer than 3, pad with empty strings; if more, take the first 3.
  n_cols <- ncol(split_matrix)
  # if(n_cols < 3) {
  #   pad <- matrix("", nrow = nrow(split_matrix), ncol = 3 - n_cols)
  #   split_matrix <- cbind(split_matrix, pad)
  # } else if(n_cols > 3) {
  #   split_matrix <- split_matrix[, 1:3, drop = FALSE]
  # }
  #
  # Create a data frame with columns row1, row2, row3.
  colnames(split_matrix) = paste0("row", 1:n_cols)

  # currently remove empty cols at the beginning
  if (remove_empty_cols) {
    for (i in 1:2) {
      mat = substr(split_matrix,1,1)
      all_start_empty_mat = matrix(rowMeans(mat == "|" | mat == "")==1, nrow(mat), ncol(mat))
      split_matrix = substr(split_matrix, 1+all_start_empty_mat, nchar(split_matrix))
    }
  }

  if (as_matrix) return(split_matrix)
  df <- as.data.frame(split_matrix, stringsAsFactors = FALSE)
  return(df)
}


split_html_latex_multiline <- function(html_vec) {


  # Identify indices that contain the math span.
  math_idx <- which(stringi::stri_detect_fixed(html_vec, '<span class="math inline">'))

  if (length(math_idx)==0) {
    return(data.frame(row1==html_vec))
  }


  # Extract pre-text, span content, and post-text for those elements.
  pre_text <- stringi::stri_match_first_regex(
    html_vec[math_idx],
    "^(.*?)<span class=\"math inline\">"
  )[,2]
  span_content <- stringi::stri_match_first_regex(
    html_vec[math_idx],
    "<span class=\"math inline\">(.*?)</span>"
  )[,2]
  span_content = stri_replace_all_fixed(span_content, "&amp;","&")

  post_text <- stringi::stri_match_first_regex(
    html_vec[math_idx],
    "</span>(.*)$"
  )[,2]

  # Replace any NA with an empty string.
  pre_text[is.na(pre_text)] <- ""
  post_text[is.na(post_text)] <- ""

  # Remove only leading math delimiters: \(, \[, $$, or $
  span_content <- stringi::stri_replace_first_regex(
    span_content, "^(\\\\\\(|\\\\\\[|\\$\\$|\\$)", ""
  )
  # Remove only trailing math delimiters: \), \], $$, or $
  span_content <- stringi::stri_replace_last_regex(
    span_content, "(\\\\\\)|\\\\\\]|\\$\\$|\\$)\\s*$", ""
  )

  # Process the math content with the vectorized latex_to_df.
  math_mat <- split_latex_multiline(span_content, as_matrix=TRUE)

  # Modify only row1 by prepending pre_text and appending post_text,
  # adding a space after pre_text and before post_text when non-empty.
  math_mat[,1]<- paste0(
    ifelse(nzchar(pre_text), paste0(pre_text, " "), ""),
    math_mat[,1],
    ifelse(nzchar(post_text), paste0(" ", post_text), "")
  )

  mat = matrix("", length(html_vec), ncol(math_mat))
  colnames(mat) = paste0("row",1:ncol(math_mat))
  mat[,1] = html_vec
  mat[math_idx, ] <- math_mat
  df = as.data.frame(mat,  stringsAsFactors = FALSE)
  df
}
