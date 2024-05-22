#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' load dependent packages
#' @name pckgs
#' 
#' @importFrom psych harmonic.mean
#' @importFrom purrr map
#' @import tidytable
#' @import vroom
#' @import here
#' @import DBI
#' @import data.table
#' @import afscdata
NULL