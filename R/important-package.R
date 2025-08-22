#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import recipes
#' @import rlang
#' @importFrom stats sd predict as.formula
#' @importFrom hardhat extract_fit_parsnip

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
    "std_err",
    "score",
    ".d_overall",
    "outcome",
    "score_objs"
  )
)
## usethis namespace: end
NULL
