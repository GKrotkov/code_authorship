library(tokenizers)
library(stringr)

# ph for "placeholder"

replacements <- c(
  " "     = " space_ph",
  "<-"    = " assign_arrow_left_ph",
  "->"    = " assign_arrow_right_ph",
  "<<-"   = " global_assign_arrow_left_ph",
  "->>"   = " global_assign_arrow_right_ph",
  "%>%"   = " pipe_arrow_right_ph",
  "%<%"   = " pipe_arrow_left_ph",
  "%in%"  = " matching_operators_ph",
  "%*%"   = " matrix_multiplication_ph",
  "%o%"    = " outer_product_ph",
  "%T>%"   = " magrittr_forward_pipe_ph",
  "%<T%"   = " magrittr_backward_pipe_ph",
  "%x%"    = " kronecker_ph",
  "%/%"    = " integer_division_ph",
  "%\\|\\|%"   = " default_value_ph",
  "%%"     = " modulo_ph",
  "::"     = " namespace_ph",
  ":::"    = " triple_colon_ph",
  "#"      = " comment_start_ph",
  "-"      = " hyphen_ph",
  "\\+"    = " plus_sign_ph",
  "\\*"    = " asterisk_ph",
  "="      = " equal_sign_ph",
  "/"      = " backslash_ph",
  "\\\\"   = " forwardslash_ph",
  "\'"     = " single_quote_ph",
  "\""     = " double_quote_ph",
  "!"      = " exclamation_point_ph",
  "\\$"      = " dollar_sign_ph",
  "@"      = " at_symbol_ph",
  "%"      = " percent_symbol_ph",
  "\\^"      = " carrot_symbol_ph",
  "&"      = " and_symbol_ph",
  "\\("    = " open_parentheses_ph",
  "\\)"    = " close_parentheses_ph",
  "\\{"    = " open_brace_ph",
  "\\}"    = " close_brace_ph",
  "\\["    = " open_bracket_ph",
  "\\]"    = " close_bracket_ph",
  ":"      = " colon_ph",
  ";"      = " semicolon_ph",
  "\\?"    = " question_mark_ph",
  "<"      = " less_than_sign_ph",
  ">"      = " greater_than_sign_ph",
  "`"      = " grave_accent_ph",
  "~"      = " approximate_ph",
  "\\.\\.\\." = " elipses_ph",
  "\\."    = " period_ph",
  ","      = " comma_ph"
)

reformat_text_R <- function(path, replacements){
  
  script_lines <- readLines(path)
  
  text <-""
  
  for (i in 1:length(script_lines)){
    text <- paste0(text, script_lines[i], " new_line_ph")
  }
  
  for (pattern in names(replacements)) {
    text <- str_replace_all(text, pattern, replacements[pattern])
  }
  
  return(text)
  
}


