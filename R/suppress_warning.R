#' suppress_warning
#'
suppress_warning <- function (expr, pattern, ...) {
  eval.parent(
    substitute(
      withCallingHandlers(expr, warning = function (w) {
        warn_msg <- conditionMessage(w)
        match <- grepl(pattern, warn_msg)
        if (isTRUE(match)) invokeRestart("muffleWarning")
      })
    )
  )
}
