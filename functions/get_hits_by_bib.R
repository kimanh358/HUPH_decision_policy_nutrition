get_hits_by_bib <- function(search_terms, bib_texts) {
  lapply(search_terms, function(terms) {
    matching_ids <- names(bib_texts)[sapply(bib_texts, function(txt) {
      any(sapply(terms, function(term) grepl(term, txt, fixed = TRUE)))
    })]
    return(matching_ids)
  })
}
