#' Calculate Daily Calorie Needs
#'
#' This function estimates the daily calorie needs based on activity level and personal data.
#'
#' @param weight A numeric value representing weight in kilograms.
#' @param height A numeric value representing height in centimeters.
#' @param age A numeric value representing age in years.
#' @param sex A character value, either "male" or "female".
#' @param activity_level A character value representing activity level: "sedentary", "light", "moderate", "active", "very_active".
#' @return The estimated daily calorie needs.
#' @export
#' @examples
#' daily_calories(70, 175, 30, "male", "moderate")
daily_calories <- function(weight, height, age, sex, activity_level) {
  if (sex == "male") {
    bmr <- 88.362 + (13.397 * weight) + (4.799 * height) - (5.677 * age)
  } else if (sex == "female") {
    bmr <- 447.593 + (9.247 * weight) + (3.098 * height) - (4.330 * age)
  } else {
    stop("Invalid sex. Please use 'male' or 'female'.")
  }

  activity_factors <- list(
    sedentary = 1.2,
    light = 1.375,
    moderate = 1.55,
    active = 1.725,
    very_active = 1.9
  )

  if (!activity_level %in% names(activity_factors)) {
    stop("Invalid activity level. Please use 'sedentary', 'light', 'moderate', 'active', or 'very_active'.")
  }

  daily_calories <- bmr * activity_factors[[activity_level]]
  return(daily_calories)
}
