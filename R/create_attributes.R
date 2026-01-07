#' Create data attributes
#'
#' @param dfs_filtered List of data frames filtered to observations of interest
#'
#' @return Data frame of all created data attributes
#' for the observations of interest
#'
#' @importFrom susoreview count_obs any_vars count_vars
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
create_attributes <- function(
  dfs_filtered
) {

  # ============================================================================
  # [1] household roster
  # ============================================================================

  attrib_n_heads <- susoreview::count_obs(
    df = dfs_filtered$members,
    where = s01q03 == 1,
    attrib_name = "n_heads",
    attrib_vars = "s01q03"
  )

  attrib_n_young_children <- susoreview::count_obs(
    df = dfs_filtered$members,
    where = s04a_FILTER1 == 1,
    attrib_name = "n_young_children",
    attrib_vars = "s04a_FILTER1"
  )

  attrib_n_school_age_children <- susoreview::count_obs(
    df = dfs_filtered$members,
    where = s04a_FILTER1 == 1,
    attrib_name = "n_school_age_children",
    attrib_vars = "s08_FILTER1"
  )

  # ============================================================================
  # [10A] housing, energy, water and sanitation
  # ============================================================================

  # ----------------------------------------------------------------------------
  # access to electricity
  # ----------------------------------------------------------------------------

  attrib_access_to_electricity <- susoreview::create_attribute(
    df = dfs_filtered$households,
    condition = s10aq08 == 1,
    attrib_name = "access_to_electricity",
    attrib_vars = "s10aq09"
  )

  # ----------------------------------------------------------------------------
  # uses electricity from solar
  # ----------------------------------------------------------------------------

  expr_codes_solar_elec <-
    c(
      4, # solar home system
      5, # solar lantern
      6 # solar lighting system
    ) |>
    paste(collapse = "|")

  attrib_uses_solar_elec <- dfs_filtered$households |>
    susoreview::any_vars(
      var_pattern = glue::glue("s10bq01__({expr_codes_solar_elec})"),
      var_val = 1,
      attrib_name = "uses_solar_elec",
    )

  # ============================================================================
  # [10B] durable goods
  # ============================================================================

  # ----------------------------------------------------------------------------
  # number assets owned
  # ----------------------------------------------------------------------------

  attrib_n_assets <- dfs_filtered$households |>
    susoreview::count_vars(
      var_pattern = "s10bq01__",
      var_val == 1,
      attrib_name = "n_assets"
    )

  # ----------------------------------------------------------------------------
  # own assets that need significant and/or continuous supply of electricity
  # ----------------------------------------------------------------------------

  expr_codes_electric_assets <-
    # collect codes of assets that require electricity
    c(
      5, # Refrigerator
      6, # Freezer
      7, # Air conditioner
      10, # Washing machine
      18, # Microwave
      19 # Television
    ) |>
    # transform into part of a regex pattern (e.g., `asset__[101|102]`)
    paste(collapse = "|")

  attrib_own_assets_need_elec <- dfs_filtered$households |>
    susoreview::any_vars(
      var_pattern = glue::glue("s10bq01__({expr_codes_electric_assets})"),
      var_val = 1,
      attrib_name = "own_assets_need_elec",
    )

  # ----------------------------------------------------------------------------
  # owns solar panel
  # ----------------------------------------------------------------------------

  code_solar_panel <- "10"

  attrib_own_solar_panel <- dfs_filtered$households |>
    susoreview::any_vars(
      var_pattern = glue::glue("s10bq01__{code_solar_panel}"),
      var_val = 1,
      attrib_name = "own_assets_need_elec",
    )

  # ----------------------------------------------------------------------------
  # owns electricity-generating asset
  # ----------------------------------------------------------------------------

  expr_code_elec_gen_asset <-
    c(
      15, # generator
      16 # solar panel
    ) |>
    paste(collapse = "|")

  attrib_own_elec_gen_asset <- dfs_filtered$households |>
    susoreview::any_vars(
      var_pattern = glue::glue("s10bq01__{expr_code_elec_gen_asset}"),
      var_val = 1,
      attrib_name = "own_elec_gen_asset",
    )

  # =============================================================================
  # bind together all attributes
  # =============================================================================

  # ------------------------------------------------------------------------------
  # transform `attribs_*` objects from a list of df to single df
  # ------------------------------------------------------------------------------

  # note: not relevant for childcare

  # ------------------------------------------------------------------------------
  # put together all attributes data
  # ------------------------------------------------------------------------------

  # compose the regular expression to target two types of objects
  objects_rexpr <- c(
  "^attribs_", # objects previously lists of dfs
  "^attrib_" # objects simple that are simple df
  ) |>
    paste(collapse = "|")

  # put together in a single data frame all attributes
  attribs <- dplyr::bind_rows(mget(ls(pattern = objets_rexpr)))

  return(attribs)

}
