# transmute_raster.expr <- function (
#   raster_object,
#   ...,
#   verbose = getOption("verbose")
# ) {
#
#   msg <- function (...) if(isTRUE(verbose)) {
#     message("[transmute_raster] ", ...)
#   }
#
#   arg_quos <- rlang::enquos(...)
#   stopifnot(length(arg_quos) == 1)
#   arg_quo <- arg_quos[[1]]
#
#   arg_expr <- rlang::quo_get_expr(arg_quo)
#
#   # Names of the layers that we are going to munge together
#   expr_vars <- all.vars(as.formula(str_c("~ ", deparse(arg_expr))))
#
#   # Helper function: creates a "truly empty" list with the specified names
#   empty_named_list <- function (nm) {
#     setNames(rep(list(bquote()), length(nm)), nm)
#   }
#
#   arg_fun <- rlang::new_function(
#     empty_named_list(expr_vars),
#     arg_expr)
#
#   layer <-
#     raster::overlay(
#       raster::subset(raster_object, expr_vars),
#       fun = arg_fun)
#
#   names(layer) <- names(arg_quo)
#
#   return(layer)
#
# }

#' transmute_raster
#'
#' @param raster_object multi-layer [Raster*](raster::Raster-class) object
#' @param f formula
#' @param ... ignored
#' @param verbose logical
#'
#' @export
transmute_raster <- function (
  raster_object,
  f,
  verbose = getOption("verbose")
) {

  f_call <- rlang::f_rhs(f)
  f_rhs_vars <- all.vars(f_call)
  f_lhs_vars <- all.vars(rlang::f_lhs(f))

  # Helper function: creates a "truly empty" list with the specified names
  empty_named_list <- function (nm) {
    setNames(rep(list(bquote()), length(nm)), nm)
  }

  f_fun <- rlang::new_function(
    empty_named_list(f_rhs_vars),
    f_call)

  layer <-
    raster::overlay(
      raster::subset(raster_object, f_rhs_vars),
      fun = f_fun)

  names(layer) <- f_lhs_vars

  return(layer)

}
