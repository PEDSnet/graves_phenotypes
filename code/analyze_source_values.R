#' TODO
#' 1. value_as_number, value_as_concept_id only work for measurement domain
#' 2. allow regex for string search (to allow or's)
#' 3. provide cohort as argument
#' 4. function to look for source concept ids in measurement source values


#' Find potential missing codes
#'
#' @param domain cdm tbl to check e.g. 'measurement'
#' @param strings_to_check filter for source values that contain these strings
#' @param remove_codesets exclude rows with concept ids in these codesets
#' @param exclude_strings exclude source values that contain these strings
#' @param has_value require that the row has a value as number
#' @param has_concept require that the row has a value as concept id
#'
#' @return a table with person count and record count for each source value
#' @export
#'
#' @examples
find_potential_codes <-
  function(domain,
           strings_to_check,
           remove_codesets = NA,
           exclude_strings = NA,
           has_value = FALSE,
           has_concept = FALSE,
           by_site = FALSE) {
    domain_prefix <- domain %>%
      str_split_i("_", 1L)

    domain_concept_name <- paste0(domain_prefix, "_concept_id")
    domain_source_value <- paste0(domain_prefix, "_source_value")

    res_tbl <- results_tbl("cohort_post_attrition") %>%
      select(person_id) %>%
      inner_join(results_tbl({{ domain }}), by = "person_id")

    if (!all(is.na(remove_codesets))) {
      codesets <- union_codesets(remove_codesets)
      res_tbl <- res_tbl %>%
        anti_join(codesets,
                  by = join_by({{ domain_concept_name }} == "concept_id"))
    }

    for (s in strings_to_check) {
      res_tbl <- res_tbl %>%
        filter(grepl({{ s }}, !!sym(domain_source_value), ignore.case = TRUE))
    }

    if (!all(is.na(exclude_strings))) {
      for (s in exclude_strings) {
        res_tbl <- res_tbl %>%
          filter(!grepl({{ s }},
                        !!sym(domain_source_value), ignore.case = TRUE))
      }
    }

    if (has_value) {
      res_tbl <- res_tbl %>% filter(!is.na(value_as_number))
    }

    if (has_concept) {
      res_tbl <- res_tbl %>% filter(!is.na(value_as_concept_id))
    }

    if(by_site){
      potential_codes <-
        res_tbl %>%
        group_by(!!sym(domain_concept_name), !!sym(domain_source_value), site) %>%
        summarize(pc = count(distinct(person_id)), rc = count(person_id)) %>%
        arrange(desc(pc))
    }else{
      potential_codes <-
        res_tbl %>%
        group_by(!!sym(domain_concept_name), !!sym(domain_source_value)) %>%
        summarize(pc = count(distinct(person_id)), rc = count(person_id)) %>%
        arrange(desc(pc))
    }
    potential_codes
  }
