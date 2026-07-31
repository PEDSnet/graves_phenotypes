#' Upload new codesets in specs dir
#' To avoid having to manually upload individual codesets
#' First checks whether a codeset already exists on the database, and if not
#' upload it.
#'
#' @param col_types Optional param for the column types. The default setting is
#' for an export from PEDSnet Atlas. This assumes all codesets in the directory
#' have the same col_types.
#'
#' @return NA
#' @export
#'
#' @examples
load_codeset_dir <- function(col_types = "icicccccDD") {
  codesets <- list.files("specs")
  files_no_extension <- sub("\\.[^.]*$", "", codesets)
  for (codeset in files_no_extension) {
    tryCatch(
      {
        results_tbl(codeset)
        message(codeset, " already existed, skipping")
      },
      error = function(e) {
        message("Uploading codeset: ", codeset)
        tryCatch(
          {
            load_codeset(codeset, col_types = col_types, table_name = codeset)
          },
          error = function(e2) {
            message(codeset,
                    " failed to auto upload.",
                    " Is the index column 'concept_id' and are the col types: ",
                    col_types)
          }
        )
      }
    )
  }
}
