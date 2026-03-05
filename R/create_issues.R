#' Create non-outlier issues
#'
#' @description
#' Create the following issue types for cases of interest:
#'
#' - Impossible situations
#' - Internal inconsistencies
#'
#' @param df_attribs Data frame of attributes.
#' @param get_msg Function for retrieving the right message
#'
#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom susoreview create_issue
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
create_non_outlier_issues <- function(
  df_attribs,
  get_msg
) {

  # ============================================================================
  # impossible situations
  # ============================================================================

  # ----------------------------------------------------------------------------
  # no reference person
  # ----------------------------------------------------------------------------

  desc_no_head <- get_msg("issues", "no_head", "desc")

  issue_no_head <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_heads",
    where = n_heads == 0,
    type = 1,
    desc = desc_no_head,
    comment = glue::glue(
      get_msg("issues", "no_head", "comment")
    )
  )

  # ----------------------------------------------------------------------------
  # more than 1 reference person
  # ----------------------------------------------------------------------------

  desc_more_than_one_head <- get_msg("issues", "more_than_one_head", "desc")

  issue_more_than_one_head <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_heads",
    where = n_heads > 1,
    desc = desc_more_than_one_head,
    comment = glue::glue(get_msg("issues", "more_than_one_head", "comment"))
  )

  # ----------------------------------------------------------------------------
  # owns no assets
  # ----------------------------------------------------------------------------

  desc_owns_no_assets <- get_msg("issues", "owns_no_assets", "desc")

  issue_owns_no_assets <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_assets",
    where = n_assets == 0,
    desc = desc_owns_no_assets,
    comment = glue::glue(get_msg("issues", "owns_no_assets", "comment"))
  )

  # ----------------------------------------------------------------------------
  # no children
  # ----------------------------------------------------------------------------

  desc_no_children <- get_msg("issues", "no_children", "desc")

  issue_no_children <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = c("n_young_children", "n_school_age_children"),
    where = n_young_children == 0 & n_school_age_children == 0,
    desc = desc_no_children,
    comment = glue::glue(get_msg("issues", "no_children", "comment"))
  )

  # ============================================================================
  # internal inconsistencies
  # ============================================================================

  # ----------------------------------------------------------------------------
  # owns assets that require significant and continous electricity,
  # but has no access to electricity
  # ----------------------------------------------------------------------------

  desc_own_elect_assets_no_elec <- get_msg(
    "issues", "own_elect_assets_no_elec", "desc"
  )

  issue_own_elec_assets_no_elec <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = c("access_to_electricity", "own_assets_need_elec"),
    where = own_assets_need_elec == 1 & access_to_electricity == 0,
    type = 1,
    desc = desc_own_elect_assets_no_elec,
    comment = get_msg("issues", "own_elect_assets_no_elec", "comment")
  )

  # ----------------------------------------------------------------------------
  # gets electricity from solar panel but does not own a solar panel
  # ----------------------------------------------------------------------------

  desc_uses_solar_not_own_solar <- get_msg(
    "issues", "uses_solar_not_own_solar", "desc"
  )

  issue_use_solar_not_own_solar <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("uses_solar_elec", "own_assets_need_elec"),
    where = uses_solar_elec == 1 & own_assets_need_elec == 0,
    type = 1,
    desc = desc_uses_solar_not_own_solar,
    comment = glue::glue(
      get_msg("issues", "uses_solar_not_own_solar", "comment")
    )
  )

  # ----------------------------------------------------------------------------
  # owns asset that generates electricity but reports not having electricity
  # ----------------------------------------------------------------------------

  desc_owns_gen_asset_no_elec <- get_msg(
    "issues", "owns_gen_asset_no_elec", "desc"
  )

  issue_owns_gen_asset_no_elec <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("own_elec_gen_asset", "access_to_electricity"),
    where = own_elec_gen_asset == 1 & access_to_electricity == 0,
    type = 1,
    desc = desc_owns_gen_asset_no_elec,
    comment = glue::glue(get_msg("issues", "owns_gen_asset_no_elec", "desc"))
  )

  # ============================================================================
  # combine issues
  # ============================================================================

  obj_expr_issues <- "^issue[s]*_"

  # combine all issues
  issues <- dplyr::bind_rows(mget(ls(pattern = obj_expr_issues)))

  # ============================================================================
  # return issues issues
  # ============================================================================

  if (nrow(issues) == 0) {

    issues <- tibble::tibble(
      interview__id = NA_character_,
      interview__key = NA_character_,
      issue_type = NA_real_,
      issue_desc = NA_character_,
      issue_comment = NA_character_,
      issue_vars = NA_character_,
      issue_loc = NA_character_,
      .rows = 0
    )

  }

  return(issues)

}

#' Create outlier issues
#'
#' @description
#' Create outlier issues for cases of interest while drawing from all data
#'
#' @param dfs_full List of data frames that contain all survey observations.
#' @param dfs_filtered List of data frames that are filtered to observations
#' of interest.
#' @param get_msg Function for retrieving the right message
#'
#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom purrr map pmap
#' @importFrom dplyr semi_join
#' @importFrom tibble tribble
#' @importFrom rlang sym
#' @importFrom glue glue
#' @importFrom scales label_number
#' @importFrom haven zap_label
create_outlier_issues <- function(
  dfs_full,
  dfs_filtered,
  get_msg
) {

  # ============================================================================
  # create outlier for each data set of interest
  # ============================================================================

  # ----------------------------------------------------------------------------
  # extract comment strings
  # ----------------------------------------------------------------------------

  desc <- get_msg("outliers", "global", "desc")
  comment_intro <- get_msg("outliers", "global", "comment_intro")
  comment_var <- get_msg("outliers", "global", "comment_var")
  comment_body <- get_msg("outliers", "global", "comment_body")

  # ----------------------------------------------------------------------------
  # member-level
  # ----------------------------------------------------------------------------

  member_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "s03q48a", "s03q48b",
    "s04cq13", "NULL",
    "s04cq15a", "s04cq15b",
    "s05bq08a", "s05bq08b",
    "s05bq14a", "s05bq14b",
    "s08q16", "s08q17",
    "s08q34", "s08q35",
  ) |>
	dplyr::rowwise() |>
  dplyr::mutate(desc = get_msg("outliers", "member", var)) |>
	dplyr::ungroup()

  issues_member_lvl <- purrr::pmap(
    .l = member_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$members,
      df_full = dfs_full$members,
      var = !!rlang::sym(..1),
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the French thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      )
    )
  )

  # ----------------------------------------------------------------------------
  # child-caregiver-level
  # ----------------------------------------------------------------------------

  child_caregiver_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "s04bq26a", "s04bq26b",
    "s04bq27a", "s04bq27b",
    "s04bq35", "s04bq36",
    "s04bq56", "s04bq55",
    "s04bq68", "s04bq69",
  ) |>
  dplyr::rowwise() |>
	dplyr::mutate(desc2 = get_msg("outliers", "child-caregiver", var)) |>
	dplyr::ungroup()

  issues_child_caregiver_lvl <- purrr::pmap(
    .l = child_caregiver_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$caregivers_younger_kids,
      df_full = dfs_full$caregivers_younger_kids,
      var = !!rlang::sym(..1),
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the French thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      )
    )
  )

  # ----------------------------------------------------------------------------
  # household-level
  # ----------------------------------------------------------------------------

  hhold_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "s10aq06", "NULL",
  ) |>
	dplyr::rowwise() |>
  dplyr::mutate(desc = get_msg("outliers", "household", var)) |>
	dplyr::ungroup()

  issues_hhold_lvl <- purrr::pmap(
    .l = hhold_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$households,
      df_full = dfs_full$households,
      var = !!rlang::sym(..1),
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the French thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      )
    )
  )

  # ============================================================================
  # combine issues
  # ============================================================================

  outlier_issues <- dplyr::bind_rows(
    issues_member_lvl, issues_child_caregiver_lvl, issues_hhold_lvl
  )

  return(outlier_issues)

}

#' Create issues
#'
#' @description
#' Create issues for outliers and non-outliers alike
#'
#' @param df_attribs Data frame of attributesa.
#' @param dfs_full List of data frames that contain all survey observations.
#' @param dfs_filtered List of data frames that are filtered to observations
#' of interest.
#' @param get_msg Function for retrieving the right message

#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom dplyr bind_rows
create_issues <- function(
  df_attribs,
  dfs_full,
  dfs_filtered,
  get_msg
) {

  # ============================================================================
  # Create issues
  # ============================================================================

  issues_non_outlier <- create_non_outlier_issues(
    df_attribs = df_attribs
  )

  issues_outlier <- create_outlier_issues(
    dfs_full = dfs_full,
    dfs_filtered = dfs_filtered
  )

  # ============================================================================
  # Combine all issues
  # ============================================================================

  # expression to match intermediary issues objects
  # but, importantly, not the combined issues object
  obj_expr_issues <- "^issue[s]*_"

  # combine all issues
  issues <- dplyr::bind_rows(mget(ls(pattern = obj_expr_issues)))

  # remove intermediary objects to lighten load on memory
  rm(list = ls(pattern = obj_expr_issues))

  # ============================================================================
  # Add issues if questions unanswered
  # ============================================================================

  # extract number of questions unanswered
  # use `interview__diagnostics` file rather than request stats from API
  interview_stats <- dfs_filtered$interview__diagnostics |>
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

  # add error if interview completed, but questions left unanswered
  # returns issues data supplemented with unanswered question issues
  issues_plus_unanswered <- susoreview::add_issue_if_unanswered(
    df_cases_to_review = entretiens_a_valider,
    df_interview_stats = interview_stats,
    df_issues = issues,
    n_unanswered_ok = 0,
    issue_desc = get_msg("issues", "any_unanswered", "desc"),
    issue_comment = glue::glue(get_msg("issues", "any_unanswered", "comment"))
  )

  return(issues_plus_unanswered)

}
