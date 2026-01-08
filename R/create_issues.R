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
  df_attribs,
  cases_to_review
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
    type = "desc"
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
        type = "comment,"
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
  
  paste(
    "Le ménage possède des biens électriques",
    "mais n'a pas l'accès à l'électricité."
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

  # ------------------------------------------------------------------------------
  # owns asset that generates electricity but reports not having electricity
  # ------------------------------------------------------------------------------

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
  # member-level
  # ----------------------------------------------------------------------------

  member_lvl_specs <- tibble::tribble(
    ~ var, ~ by, ~ desc,
    "s03q48a", "s03q48b",
      "revenus ou bénéfices gagnés grâce à tous ses emplois",
    "s04cq13", "NULL",
      "nombre maximum de minutes à parcourir",
    "s04cq15a", "s04cq15b",
      "montant à payer un prestataire (par unité de temps)",
    "s05bq08a", "s05bq08b",
      "montant à gagner en travaillant ou en créant une entreprise (par unité de temps)",
    "s05bq14a", "s05bq14b",
      "montant à gagner en travaillant dans un autre domaine ou créant une entreprise (par unité de temps)",
    "s08q16", "s08q17",
      "montant devoir payer de votre poche (par unité de temps)",
    "s08q34", "s08q35",
      "montant devoir payer de votre poche (par unité de temps)",
  )

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
      desc = glue::glue("Valeur extrême pour {..3}"),
      comment = paste(
        # evaluate
        glue::glue("Valeur extrême identifée pour {..3}."),
        glue::glue("La valeur de {..1}"),
        # show the outlier amount
        # using the French thousands and decimal marks
        # evaluating the data in the context of the outlier function
        "({
          scales::label_number(
            big.mark = ' ',
            decimal.mark = ','
          )(haven::zap_label(.data[[var_chr]]))
          }
        )",
        "s'écarte de la norme.",
        "Veuillez vérier la justesse de la valeur.",
        "Si la valeur est erronnée, veuillez la corriger.",
        "Si la valeur est confirmée, veuillez laisser un commentaire explicatif."
      )
    )
  )

  # ----------------------------------------------------------------------------
  # child-caregiver-level
  # ----------------------------------------------------------------------------

  child_caregiver_lvl_specs <- tibble::tribble(
    ~ var, ~ by, ~ desc,
    "s04bq26a", "s04bq26b",
      "montant devoir payer de votre poche (par unité de temps)",
    "s04bq27a", "s04bq27b",
      "montant estimé payé en nature (par unité de temps)",
    "s04bq35", "s04bq36",
      "montant devoir payer de votre poche (par unité de temps)",
    "s04bq56", "s04bq55",
      "temps pour se rendre à une structure de garde d'enfant (par le mode de transport principal)",
    "s04bq68", "s04bq69",
      "montant devoir payer de votre poche (par unité de temps)",
  )

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
      desc = glue::glue("Valeur extrême pour {..3}"),
      comment = paste(
        # evaluate
        glue::glue("Valeur extrême identifée pour {..3}."),
        glue::glue("La valeur de {..1}"),
        # show the outlier amount
        # using the French thousands and decimal marks
        # evaluating the data in the context of the outlier function
        "({
          scales::label_number(
            big.mark = ' ',
            decimal.mark = ','
          )(haven::zap_label(.data[[var_chr]]))
          }
        )",
        "s'écarte de la norme.",
        "Veuillez vérier la justesse de la valeur.",
        "Si la valeur est erronnée, veuillez la corriger.",
        "Si la valeur est confirmée, veuillez laisser un commentaire explicatif."
      )
    )
  )

  # ----------------------------------------------------------------------------
  # household-level
  # ----------------------------------------------------------------------------

  hhold_lvl_specs <- tibble::tribble(
    ~ var, ~ by, ~ desc,
    "s10aq06", "NULL", "number of rooms"
  )

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
      desc = glue::glue("Valeur extrême pour {..3}"),
      comment = paste(
        # evaluate
        glue::glue("Valeur extrême identifée pour {..3}."),
        glue::glue("La valeur de {..1}"),
        # show the outlier amount
        # using the French thousands and decimal marks
        # evaluating the data in the context of the outlier function
        "({
          scales::label_number(
            big.mark = ' ',
            decimal.mark = ','
          )(haven::zap_label(.data[[var_chr]]))
          }
        )",
        "s'écarte de la norme.",
        "Veuillez vérier la justesse de la valeur.",
        "Si la valeur est erronnée, veuillez la corriger.",
        "Si la valeur est confirmée, veuillez laisser un commentaire explicatif."
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
