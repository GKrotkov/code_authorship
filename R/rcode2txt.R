library(tokenizers)
library(stringr)

replacements <- c(
  " "     = " space_placeholder ",
  "<-"    = " assign_arrow_left_placeholder ",
  "->"    = " assign_arrow_right_placeholder ",
  "<<-"   = " global_assign_arrow_left_placeholder ",
  "->>"   = " global_assign_arrow_right_placeholder ",
  "%>%"   = " pipe_arrow_right_placeholder ",
  "%<%"   = " pipe_arrow_left_placeholder ",
  "%in%"  = " matching_operators_placeholder ",
  "%*%"   = " matrix_multiplication_placeholder ",
  "%o%"    = " outer_product_placeholder ",
  "%T>%"   = " magrittr_forward_pipe_placeholder ",
  "%<T%"   = " magrittr_backward_pipe_placeholder ",
  "%x%"    = " kronecker_placeholder ",
  "%/%"    = " integer_division_placeholder ",
  "%\\|\\|%"   = " default_value_placeholder ",
  "%%"     = " modulo_placeholder ",
  "::"     = " namespace_placeholder ",
  ":::"    = " triple_colon_placeholder ",
  "#"      = " comment_start_placeholder ",
  "-"      = " hyphen_placeholder ",
  "\\+"    = " plus_sign_placeholder ",
  "\\*"    = " asterisk_placeholder ",
  "="      = " equal_sign_placeholder ",
  "/"      = " backslash_placeholder ",
  "\\\\"   = " forwardslash_placeholder ",
  "\'"     = " single_quote_placeholder ",
  "\""     = " double_quote_placeholder ",
  "!"      = " exclamation_point_placeholder ",
  "\\$"      = " dollar_sign_placeholder ",
  "@"      = " at_symbol_placeholder ",
  "%"      = " percent_symbol_placeholder ",
  "\\^"      = " carrot_symbol_placeholder ",
  "&"      = " and_symbol_placeholder ",
  "\\("    = " open_parentheses_placeholder ",
  "\\)"    = " close_parentheses_placeholder ",
  "\\{"    = " open_brace_placeholder ",
  "\\}"    = " close_brace_placeholder ",
  "\\["    = " open_bracket_placeholder ",
  "\\]"    = " close_bracket_placeholder ",
  ":"      = " colon_placeholder ",
  ";"      = " semicolon_placeholder ",
  "\\?"    = " question_mark_placeholder ",
  "<"      = " less_than_sign_placeholder ",
  ">"      = " greater_than_sign_placeholder ",
  "`"      = " grave_accent_placeholder ",
  "~"      = " approximate_placeholder ",
  "\\.\\.\\." = " elipses_placeholder ",
  "\\."    = " period_placeholder ",
  ","      = " comma_placeholder ",
)

reformat_text_R <- function(path, replacements){
  
  script_lines <- readLines(path)
  
  text <-""
    
  for (i in 1:length(script_lines)){
    text <- paste0(text, script_lines[i], " new_line_placeholder ")
  }
  
  for (pattern in names(replacements)) {
    text <- str_replace_all(text, pattern, replacements[pattern])
  }
  
  return(text)
  
}


