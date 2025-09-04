#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import recipes
#' @import rlang
#' @importFrom stats sd predict as.formula
#' @importFrom hardhat extract_fit_parsnip
#' @importFrom S7 check_is_S7

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
    "score_objs",
    "removed",
    "terms"
  )
)

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

## usethis namespace: end
NULL
