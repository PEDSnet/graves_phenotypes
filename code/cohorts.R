find_events <- function(event_tbl,
                        domain,
                        cohort = NULL,
                        concept_codeset = NULL,
                        src_codeset = NULL) {
  assertthat::assert_that(!(is.null(concept_codeset) & is.null(src_codeset)),
                          msg = "Must provide one of codeset or src_codeset"
  )
  if (!is.null(concept_codeset)) {
    concept_events <- event_tbl %>%
      inner_join(
        concept_codeset %>%
          select(concept_id, name),
        by = join_by(!!sym(paste0(domain, "_concept_id")) == concept_id)
      )
  }
  if (!is.null(src_codeset)) {
    concept_src_events <- event_tbl %>%
      inner_join(
        src_codeset %>%
          select(concept_id, name),
        by = join_by(!!sym(paste0(domain, "_source_concept_id")) == concept_id)
      )
  }
  if (exists("concept_events") && exists("concept_src_events")) {
    output <- (concept_events %>%
                 union(concept_src_events))
  } else if (exists("concept_events")) {
    output <- concept_events
  } else {
    output <- concept_src_events
  }
  
  if (!is.null(cohort)) {
    output %>%
      semi_join(cohort, by = "person_id")
  } else {
    output
  }
}

find_events_multi_domain <- function(domains,
                                     cohort = NULL,
                                     concept_codeset = NULL,
                                     src_codeset = NULL) {
  all_events <- NULL
  for (domain in domains) {
    prefix <- str_replace(domain, "_occurrence", "")
    events <- find_events(
      results_tbl(domain),
      prefix,
      cohort = cohort,
      concept_codeset = concept_codeset,
      src_codeset = src_codeset
    )
    
    events_standard <- events %>%
      rename_with(.fn = ~ sub(paste0("^", prefix, "_"), "", .x), .cols = everything()) %>%
      rename_with(.fn = ~ sub("^start_", "", .x), .cols = everything()) %>%
      select(
        person_id,
        visit_occurrence_id,
        occurrence_id,
        date,
        datetime,
        concept_id,
        concept_name,
        source_concept_id,
        source_concept_name,
        source_value,
        type_concept_id,
        name
      ) %>%
      mutate(source = {{ domain }})
    
    if(is.null(all_events)){
      all_events <- events_standard
    } else{
      all_events <- all_events %>% union(events_standard)
    }
  }
  all_events
}