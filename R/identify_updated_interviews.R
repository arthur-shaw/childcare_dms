#' Create an hash of interview actions
#'
#' @description
#' Create a cryptographic hash from the high-level interview transactions log
#' found in `interview__actions`, letting the user decide which actions to
#' include.
#'
#' @param actions_path Character. Full path to the `interview__actions.dta`.
#' @param actions Numeric vector. Actions to include when creating a hash.
#' By default, for data validation use cases, only interview completion.
#'
#' @return Data frame with the following columns:
#'
#' - `interview__id`. Character. Unique interview identifier.
#' - `event_string`. Character. Concatenation of the actions. Actions
#' are separated by `||`. Attributes of actions are separated by `|`.
#' - `event_hash`. Character. Cryptographic hash of `event_string`.
#'
#' @importFrom haven read_dta
#' @importFrom dplyr filter rowwise mutate ungroup group_by summarise
#' @importFrom secretbase siphash13
create_inteview_hash <- function(
  actions_path,
  actions = 1
) {

  interview_hash_df <- actions_path |>
    haven::read_dta() |>
    dplyr::filter(action %in% actions) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      event_string = paste(
        date, time, action, originator, role,
        sep = "|"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(interview__id) |>
    dplyr::summarise(
      event_string = paste(event_string, collapse = "||")
    ) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      event_hash = secretbase::siphash13(x = event_string)
    ) |>
    dplyr::ungroup()

  return(interview_hash_df)

}

#' Identify interviews that have changed
#'
#' @description
#' Compare the hashed event logs of previously downloaded (old) interviews
#' with currently downloaded (new) interviews. New interviews are those that
#' either were not present in the old log or have a different event hash.
#'
#' @param old_hash_df Data frame of previously downloaded interviews,
#' in the form returned by `create_inteview_hash()`
#' @param new_hash_df Data frame of currently downloaded interviews,
#' in the form returned by `create_inteview_hash()`
#'
#' @return Data frame of interviews whose data has likely changed.
#'
#' @importFrom dplyr anti_join select rename inner_join filter bind_rows
get_updated_interviews <- function(
  old_hash_df,
  new_hash_df
) {

  new_interviews <- new_hash_df |>
    dplyr::anti_join(old_hash_df, by = "interview__id") |>
    dplyr::select(interview__id)

  updated_interviews <- new_hash_df |>
    dplyr::rename(old_event_hash = event_hash) |>
    dplyr::inner_join(old_hash_df, by = "interview__id") |>
    dplyr::filter(old_event_hash != event_hash) |>
    dplyr::select(interview__id)

  interviews_w_changes <- dplyr::bind_rows(
    new_interviews, updated_interviews
  )

  return(interviews_w_changes)

}
