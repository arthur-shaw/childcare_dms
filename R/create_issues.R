#' Create non-outlier issues
#'
#' @description
#' Create the following issue types for cases of interest:
#'
#' - Impossible situations
#' - Internal inconsistencies
#'
#' @param df_attribs
#'
#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom susoreview create_issue
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
create_non_outlier_issues <- function(
  df_attribs
) {

  # ============================================================================
  # impossible situations
  # ============================================================================

  # ----------------------------------------------------------------------------
  # no reference person
  # ----------------------------------------------------------------------------

  desc_no_head <- get_msg(
    messages = messages,
    id = "no_head",
    type = "desc",
    lang = msg_lang
  )

  issue_no_head <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_heads",
    where = n_heads == 0,
    type = 1,
    desc = desc_no_head,
    comment = glue::glue(
      get_msg(
        messages = messages,
        id = "no_head",
        type = "comment",
        lang = msg_lang
      )
    )
  )

  # ----------------------------------------------------------------------------
  # more than 1 reference person
  # ----------------------------------------------------------------------------

  desc_more_than_one_head <- get_msg(
    messages = messages,
    id = "more_than_one_head",
    type = "desc",
    lang = msg_lang
  )

  issue_more_than_one_head <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_heads",
    where = n_heads > 1,
    desc = desc_more_than_one_head,
    comment = glue::glue(
      get_msg(
        messages = messages,
        id = "more_than_one_head",
        type = "comment",
        lang = msg_lang
      )
    )
  )

  # ----------------------------------------------------------------------------
  # owns no assets
  # ----------------------------------------------------------------------------

  desc_owns_no_assets <- get_msg(
    messages = messages,
    id = "owns_no_assets",
    type = "desc",
    lang = msg_lang
  )

  issue_owns_no_assets <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_assets",
    where = n_assets == 0,
    desc = desc_owns_no_assets,
    comment = glue::glue(
      get_msg(
        messages = messages,
        id = "owns_no_assets",
        type = "comment",
        lang = msg_lang
      )
    )
  )

  # ----------------------------------------------------------------------------
  # no children
  # ----------------------------------------------------------------------------

  desc_no_children <- get_msg(
    messages = messages,
    id = "no_children",
    type = "desc",
    lang = msg_lang
  )

  issue_no_children <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = c("n_young_children", "n_school_age_children"),
    where = n_young_children == 0 & n_school_age_children == 0,
    desc = desc_no_children,
    comment = glue::glue(
      get_msg(
        messages = messages,
        id = "no_children",
        type = "comment",
        lang = msg_lang
      )
    )
  )

  # ============================================================================
  # internal inconsistencies
  # ============================================================================

  # ----------------------------------------------------------------------------
  # owns assets that require significant and continous electricity,
  # but has no access to electricity
  # ----------------------------------------------------------------------------

  desc_own_elect_assets_no_elec <- get_msg(
    messages = messages,
    id = "own_elect_assets_no_elec",
    type = "desc",
    lang = msg_lang
  )

  issue_own_elec_assets_no_elec <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = c("access_to_electricity", "own_assets_need_elec"),
    where = own_assets_need_elec == 1 & access_to_electricity == 0,
    type = 1,
    desc = desc_own_elect_assets_no_elec,
    comment = get_msg(
      messages = messages,
      id = "own_elect_assets_no_elec",
      type = "comment",
      lang = msg_lang
    )
  )

  # ----------------------------------------------------------------------------
  # gets electricity from solar panel but does not own a solar panel
  # ----------------------------------------------------------------------------

  desc_uses_solar_not_own_solar <- get_msg(
    messages = messages,
    id = "uses_solar_not_own_solar",
    type = "desc",
    lang = msg_lang
  )

  issue_use_solar_not_own_solar <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("uses_solar_elec", "own_assets_need_elec"),
    where = uses_solar_elec == 1 & own_assets_need_elec == 0,
    type = 1,
    desc = desc_uses_solar_not_own_solar,
    comment = glue::glue(
      get_msg(
        messages = messages,
        id = "uses_solar_not_own_solar",
        type = "comment",
        lang = msg_lang
      )
    )
  )

  # ----------------------------------------------------------------------------
  # owns asset that generates electricity but reports not having electricity
  # ----------------------------------------------------------------------------

  desc_owns_gen_asset_no_elec <- get_msg(
    messages = messages,
    id = "owns_gen_asset_no_elec",
    type = "desc",
    lang = msg_lang
  )

  issue_owns_gen_asset_no_elec <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("own_elec_gen_asset", "access_to_electricity"),
    where = own_elec_gen_asset == 1 & access_to_electricity == 0,
    type = 1,
    desc = desc_owns_gen_asset_no_elec,
    comment = glue::glue(
      get_msg(
        messages = messages,
        id = "owns_gen_asset_no_elec",
        type = "desc",
        lang = msg_lang
      )
    )
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
  dfs_filtered
) {

  # ============================================================================
  # create outlier for each data set of interest
  # ============================================================================

  # ----------------------------------------------------------------------------
  # extract comment strings
  # ----------------------------------------------------------------------------

  desc <- get_outlier_msgs(
    messages = messages,
    level = "global",
    id = "desc",
    lang = msg_lang
  )
  comment_intro <- get_outlier_msgs(
    messages = messages,
    level = "global",
    id = "comment_intro",
    lang = msg_lang
  )
  comment_var <- get_outlier_msgs(
    messages = messages,
    level = "global",
    id = "comment_var",
    lang = msg_lang
  )
  comment_body <- get_outlier_msgs(
    messages = messages,
    level = "global",
    id = "comment_body",
    lang = msg_lang
  )

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
  dplyr::mutate(
    desc = get_outlier_msgs(
      messages = messages,
      level = "member",
      id = var,
      lang = msg_lang
    )
  ) |>
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
	dplyr::mutate(
    desc2 = get_outlier_msgs(
      messages = messages,
      level = "child-caregiver",
      id = var,
      lang = msg_lang
    )
  ) |>
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
  dplyr::mutate(
    desc = get_outlier_msgs(
      messages = messages,
      level = "household",
      id = var,
      lang = msg_lang
    )
  ) |>
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
