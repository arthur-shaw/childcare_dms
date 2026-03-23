# ==============================================================================
# ingest data
# ==============================================================================

combined_data_dir <- dirs$data$demand$combined

dfs_full <- ingest_dfs(
  dir = combined_data_dir,
  hhold_varname = main_file_name
)

# ==============================================================================
# identify updated interviews
# ==============================================================================

# create hashes for new data
hashes_new <- create_interview_hash(
  action_path = fs::path(combined_data_dir, interview__actions.dta),
  actions = 1
)

# load hashes for old data
hashes_old <- load_interview_tracker(dirs$data$demand$tracked)

# compare hashes to identify new/updated cases
updated_interviews <- get_updated_interviews(
  old_hash_df = hashes_old,
  new_hash_df = hashes_new
)

# stop program if there are no updated interviews
if (nrow(updated_interviews) == 0) {

  cli::cli_abort(
    message = c(
      "i" = "No new or updated interviews to process",
      "Consider downloading current data."
    )
  )

}

# ==============================================================================
# filter to udpated interviews
# ==============================================================================

dfs_filtered <- filter_dfs(
  dfs_list = dfs_full,
  interviews = updated_interviews
)

# ==============================================================================
# perform high-frequency checks
# ==============================================================================

attribs <- create_attributes(dfs_filtered = dfs_filtered)

issues <- create_issues(
  df_attribs = attribs,
  dfs_full = dfs_full,
  dfs_filtered = dfs_filtered,
  get_msg = get_msg
)

issues_w_unanswered <- add_issue_for_unanswered_q(
  dfs_filtered = dfs_filtered,
  interviews = updated_interviews,
  issues = issues
)

# ==============================================================================
# make decisions
# ==============================================================================

decisions <- create_decisions(
  dfs_filtered = dfs_filtered,
  interviews = updated_interviews,
  issues = issues_w_unanswered,
  issue_codes_to_reject = issue_codes_to_reject
)

# ===========================================================================
# write recommendations to disk
# ===========================================================================

# intermediate data
write_df_to_disk(
  df = updated_interviews,
  dir = dirs$validation$demand$recommandations
)
write_df_to_disk(
  df = attribs,
  dir = dirs$validation$demand$recommandations
)
write_df_to_disk(
  df = issues_w_unanswered,
  dir = dirs$validation$demand$recommandations
)

# recommendation files
write_df_list_to_disk(
  df_list = decisions,
  dir = dirs$validation$demand$recommandations
)

# ===========================================================================
# copy rejection recommendations to decisions
# ===========================================================================

fs::file_copy(
  path = fs::path(
    dirs$validation$demand$recommandations,
    "to_reject_api.xlsx"
  ),
  new_path = fs::path(
    dirs$validation$demand$decisions,
    "to_reject_api.xlsx"
  ),
  overwrite = TRUE
)
