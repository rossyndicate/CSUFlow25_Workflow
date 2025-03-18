#' @title Underlying function for flagging data.
#'
#' @description
#' This function adds a flag to the `flag` column of a given data frame based on
#' specified conditions for each row. The name of the flag is provided
#' by the user.
#'
#' @param df A data frame with a `flag` column.
#'
#' @param condition_arg A logical statement that is evaluated in the context of
#' the data frame.
#'
#' @param description_arg Flag added to the `flag` column.
#'
#' @returns
#' An identical data frame with a `flag` column that has been updated with the
#' flag description provided.
#'
#' @examples
# add_flag(df = all_data_flagged$`archery-Actual Conductivity`, condition_arg = mean >= 100, description_arg = "exceeds 100")
# add_flag(df = all_data_flagged$`archery-Actual Conductivity`, condition_arg = mean <= 10, description_arg = "below 10")

add_flag <- function(df, condition_arg, description_arg) {
  df <- df %>% mutate(flag = case_when(
    {{condition_arg}} ~ if_else(is.na(flag), paste(description_arg),
                                ifelse(!grepl(description_arg, flag), paste(flag, description_arg, sep = ";\n"), flag)),
    TRUE ~ flag))
  return(df)
}

add_filtering_columns <- function(df_arg) {
  
  # Helper function to safely add a new column if it doesn't already exist
  add_column_if_not_exists <- function(df, column_name, default_value = NA) {
    if (!column_name %in% colnames(df)) {
      df <- df %>% dplyr::mutate(!!sym(column_name) := default_value)
    }
    return(df)
  }
  
  # Add debugging after the setup is complete
  filter_columns_added <- df_arg %>% 
    add_column_if_not_exists(column_name = "filter") %>% 
    add_column_if_not_exists(column_name = "filter_step") %>% 
    add_column_if_not_exists(column_name = "is_filtered_out", default_value = FALSE)
  
  return(filter_columns_added)
  
}

filter_log <- function(data_arg, condition, filter_arg = "", filter_step_arg = ""){
  
  
  result <- temp %>%
    mutate(
      is_filtered_out = if_else(!{{condition}}, TRUE, is_filtered_out),
      filter = if_else(is_filtered_out, filter_arg, filter),
      filter_step = if_else(is_filtered_out, filter_step_arg, filter_step)
      )
  
  return(result)
  
}

filter_step = "Initial data filter"

test_inventory_filter_log <- inventory %>% 
  mutate(expected_days_in_operation  = round(((year(end_date)-year(begin_date))*365)*0.9)) %>% 
  filter_log(data_type == "Daily", filter_arg = "Daily type", filter_step_arg = filter_step) %>% 
  filter_log(year(begin_date) <= 2000, filter_arg = "Started measuring by 2000", filter_step) %>% 
  filter_log(year(end_date) >= 2024, filter_arg = "Continued measuring until 2024", filter_step) %>% 
  filter_log(n_obs >= expected_days_in_operation, filter_arg = "Consistent observations", filter_step)
  
