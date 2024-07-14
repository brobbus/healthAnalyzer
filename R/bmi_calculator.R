#' Calculate Body Mass Index (BMI)
#'
#' This function calculates the Body Mass Index (BMI) based on weight and height.
#'
#' @param weight A numeric value representing weight in kilograms.
#' @param height A numeric value representing height in meters.
#' @return The BMI value.
#' @export
#' @examples
#' bmi_calculator(70, 1.75)
bmi_calculator <- function(weight, height) {
  bmi <- weight / (height ^ 2)
  return(bmi)
}
