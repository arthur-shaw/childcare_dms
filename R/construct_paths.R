#' Construct paths for the project
#'
#' @return List. Collection of all project paths, available as named elements:
#'
#' ```yaml
#' proj: <character> # project root
#' r: <character> # R function directory
#' data: <character>
#'  meta: <character>
#'  demand:
#'    download: <character>
#'    combine: <character>
#'  community:
#'    download: <character>
#'    combine: <character>
#'  supply:
#'    download: <character>
#'    combine: <character>
#' validation:
#'  demand: <character>
#'  community: <character>
#'  supply: <character>
#' monitoring: <character>
#' ```
#'
#' @importFrom here here
#' @importFrom fs path
#' @importFrom purrr map
construct_paths <- function() {

  # ============================================================================
  # top-level
  # ============================================================================

  dirs <- list()
  dirs$proj <- here::here()
  dirs$r <- here::here("R")
  dir_data <- here::here("01_data")
  dir_validation <- here::here("02_validation")
  dirs$monitoring <- here::here("03_monitoring")

  # ============================================================================
  # data
  # ============================================================================

  # ----------------------------------------------------------------------------
  # add an entry for the metadata
  # ----------------------------------------------------------------------------

  dirs$data <- list(
    meta = fs::path(dir_data, "00_meta")
  )

  # ----------------------------------------------------------------------------
  # add sub-directory entries for all surveys
  # ----------------------------------------------------------------------------

  # use a named vector so that the list entries can be named
  # with the values being the directory name
  data_parents <- c(
    demand    = "01_demand",
    community = "02_community",
    supply    = "03_supply"
  )

  # apply a function to each a list of sub-directory paths
  # to each list entry named after a survey
  dirs$data[names(data_parents)] <- data_parents |>
    purrr::map(
      .f = \(parent_dir_name) {

        # construct the full path to the parent directory
        parent_dir <- fs::path(dir_data, parent_dir_name)

        # construct sub-directories under the parent
        child_dirs <- list(
          downloaded = fs::path(parent_dir, "01_downloaded"),
          combined = fs::path(parent_dir, "02_combined")
        )

        return(child_dirs)

      }
    )

  # ============================================================================
  # validation
  # ============================================================================

  dirs$validation <- list(
    demand = fs::path(dir_validation, "01_demand"),
    community = fs::path(dir_validation, "02_community"),
    supply = fs::path(dir_validation, "03_supply")
  )

  return(dirs)

}
