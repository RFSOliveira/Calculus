#' Derivative function
#'
#' Calculates the derivative of a function at a given point.
#' @param expression A string contatining the function to differentiate.
#' @param Point Value at wich to differentiate.
#' @param variable a string containing the variable used in @param expression, Make sure the variable
#' string does not show anywhere else in @param expression (Ex: variable "x" in "exp(x)") To avoid this,
#' defaults to "X".
#' @export
#' @examples derivative("sin(X)*exp(X)",12)

derivative = function(expression = NULL, Point, variable = "X"){
  Point2 = Point + 10^-10
  expression1 = gsub(variable,"Point",expression)
  expression2 = gsub(variable,"Point2",expression)
  d = (eval(parse(text = expression2))-eval(parse(text = expression1)))*10^10
  return(d)
}


#' Definite Integral function
#'
#' Calculates the definite integral of a function between two given points.
#' @param expression A string contatining the function to integrate.
#' @param start, @param end limits of integration
#' @param variable a string containing the variable used in @param expression, Make sure the variable
#' string does not show anywhere else in @param expression (Ex: variable "x" in "exp(x)") To avoid this,
#' defaults to "X".
#' @export
#' @examples definiteIntegral("sin(X)*exp(X)",0,12)

definiteIntegral = function(expression = NULL, start, end, variable = "X"){
  samp1 = seq(start, end, length.out = 1000)
  samp2 = samp1[-1]
  samp1 = samp1[-length(samp1)]
  d = derivative(expression = expression, Point = samp1)
  #dimension = max(abs(2/d*sum(d)))
  dimension = 99999*2
  d = abs(ceiling(d/sum(d)*dimension))
  points = vector()
  for(i in 1:length(samp1)){
    points = c(points,seq(samp1[i],samp2[i],length.out = d[i]))
  }
  points = unique(points)
  m = matrix(ncol = 5, nrow = length(points)-1)
  m[,1] = points[-length(points)]
  m[,2] = points[-1]
  expression1 = gsub(variable,"m[,1]",expression)
  m[,3] = eval(parse(text = expression1))
  expression2 = gsub(variable,"m[,2]",expression)
  m[,4] = eval(parse(text = expression2))
  m[,5] = (m[,3]+m[,4])*(m[,2]-m[,1])/2
  return(sum(m[,5]))
}


