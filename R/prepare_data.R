#' Ingest all data
#'
#' @description
#' Ingest all Stata data in a directory into a list
#'
#' @param dir Character. Directory containing Stata files.
#'
#' @return List of data frames.
#' Names correspond to file names without extension.
#' Contents correspond to data in those files.
#'
#' @importFrom fs dir_ls path_file path_ext_remove
#' @importFrom purrr set_names map
#' @importFrom haven read_dta
ingest_dfs <- function(dir) {

  dfs_list <-
    # get the path of all dta files in the target directory
    fs::dir_ls(dir, glob = "*.dta") |>
    # remove `assignment__actions.dta`
    # because it is the only file where `interview__id` isn't a key
    (\(x) {
      grep(
        x = x,
        pattern = "assignment__actions.dta",
        fixed = TRUE,
        invert = TRUE,
        value = TRUE
      )
    })() |>
    # make the file name the name of each path
    purrr::set_names(nm = ~ fs::path_ext_remove(fs::path_file(.x))) |>
    # replace the path with the data
    purrr::map(haven::read_dta)

  return(dfs_list)

}

#' Filter all data frames to the observations of interest
#'
#' @param dfs_list List of data frames.
#' @param interviews Data frame of interviews of interest,
#' consisting of just `interview__id`.
#'
#' @return List of data frames
#'
#' @importFrom purrr map
#' @importFrom dplyr semi_join
filter_dfs <- function(
  dfs_list,
  interviews
) {

  dfs_filtered <- purrr::map(
    .x = dfs_list,
    .f = \(x) {
      dplyr::semi_join(
        x = x,
        y = interviews,
        by = "interview__id"
      )
    }
  )

  return(dfs_filtered)

}

#' Prepare interview stats for API request
#'
#' @param diagnostics_df Data frame containing the `interview__diagnostics` file
#'
#' @return Data frame with column names renamed to match the API.
#'
#' @importFrom dplyr rename select
prepare_interview_stats <- function(diagnostics_df) {

  # extract number of questions unanswered
  # use `interview__diagnostics` file rather than request stats from API
  interview_stats <- path |>
    # rename to match column names from GET /api/v1/interviews/{id}/stats
    dplyr::rename(
      NotAnswered = n_questions_unanswered,
      WithComments = questions__comments,
      Invalid = entities__errors
    ) |>
    dplyr::select(
      interview__id, interview__key,
      NotAnswered, WithComments, Invalid
    )

  return(interview_stats)

}
