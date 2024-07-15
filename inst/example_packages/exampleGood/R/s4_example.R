#' An example S4 class
#'
#' @slot data Some data
#'
S4Example <- setClass("S4Example", representation(data = "list"))

#' Provide names of the data slot in an S4Example object
#'
#' @param x An x
#'
setMethod("names", signature("S4Example"), function(x) {
  names(x@data)
})
