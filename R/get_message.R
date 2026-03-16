#' Load all messages from YAML
#'
#' @param dir Character. Directory where 
#'
#' @return Named list, where:
#'
#' - names are the names, without extension,
#' of YAML files found in `dir`
#' - contents correspond to the contents of the YAML files
#'
#' @importFrom fs dir_ls path_file path_ext_remove
#' @importFrom cli cli_abort
#' @importFrom purrr set_names map
#' @importFrom yaml read_yaml
load_messages <- function(dir) {

  yaml_file_paths <- dir |>
    # get the path of all dta files in the target directory
    fs::dir_ls(regexp = "*\\.y[a]ml")

  if (length(yaml_file_paths) == 0) {
    cli::cli_abort(
      message = c(
        "x" = get_msg("get_messages", "no_yaml_in_dir")
      )
    )
  }

  messages <- yaml_file_paths |>
    # make the file name, without extension, the name of each path
    purrr::set_names(nm = ~ fs::path_ext_remove(fs::path_file(.x))) |>
    # replace the path with the data
    purrr::map(yaml::read_yaml)

  return(messages)

}

#' Make a function factory for extracting messages
#'
#' @return Function, where `messages` and `lang` are already defined.
#'
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
make_msg_extracter <- function(messages, lang) {

  # resolve references now when the factory function
  # rather than rely on lazy evaluation and be surprised
  force(messages)
  force(lang)

  function(...) {

    path <- list(...)

    if (length(path) == 0) {
      cli::cli_abort(
        message = c(
          "x" = "No message path supplied to resolver."
        )
      )
    }

    # Attempt to retrieve node
    node <- tryCatch(
      purrr::pluck(messages, !!!path),
      error = function(e) NULL
    )

    path_str <- paste(path, collapse = ".")

    if (is.null(node)) {

      cli::cli_abort(
        message = c(
          "x" = "No message found at path: '{path_str}'."
        )
      )

    }

    # Ensure language exists
    if (!lang %in% names(node)) {

      available_langs <- names(node)

      cli::cli_abort(
        message = c(
          "Language '{lang}' not available at path: '{path_str}'.",
          "Available languages: {available_langs}"
        )
      )
    }

    node[[lang]]

  }
}
