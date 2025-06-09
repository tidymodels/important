#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats sd predict
#' @importFrom hardhat extract_fit_parsnip extract_postprocessor

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom generics required_pkgs
#' @export
generics::required_pkgs

#' @importFrom generics augment
#' @export
generics::augment

utils::globalVariables(
  c(
    ".estimate",
    ".metric",
    "baseline",
    "direction",
    "importance",
    "permuted",
    "predictor",
    "ranking",
    "std_err"
  )
)
## usethis namespace: end
NULL

## From workflows
# nocov start
has_postprocessor <- function(x) has_postprocessor_tailor(x)
has_postprocessor_tailor <- function(x) "tailor" %in% names(x$post$actions)
# nocov end
