#' Retrieve the message text in the target language
#'
#' @param messages List. Representation of `messages.yaml`
#' @param id Character. Validation ID.
#' @param type Character. Either `"desc"` for the HQ-facing description
#' or "`comment`" for the interviewer-facing rejection comment.
#' @param lang Character. Two-letter language code from
#' [ISO 639-1](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes).
#'
#' @return Character, atomic vector. Message for validation
#'
#' @importFrom cli cli_abort
get_msg <- function(
  messages,
  id,
  type,
  lang
) {

  message <- messages[[id]][[type]][[lang]]

  if (!is.null(message)) {
    return(message)
  } else {

    if (! id %in% names(messages)) {
      cli::cli_abort("{id} is not a valid issue ID")
    } else if (! type %in% names(messages[[id]])) {
      cli::cli_abort("{type} is not a valid message type for {id}")
    } else if (! lang %in% names(messages[[id]][[type]])) {
      cli::cli_abort("{lang} is not a valid language for {id} > {type}")
    }

  }

}
