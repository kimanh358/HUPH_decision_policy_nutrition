check_hits <- function(search_terms_full_model, text) {
  any(sapply(search_terms_full_model, function(t) grepl(t, text, ignore.case = TRUE)))
}