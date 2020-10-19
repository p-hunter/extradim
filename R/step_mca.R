#' Multiple Correspondence Analysis
#'
#'`step_mca` creates a *specification* of a
#' recipe step that will compute a multiple
#' correspondence analysis of a set of factors.
#' This uses the `mca` function from the `MASS`
#' package to acheive this.
#'
#'
#' @param ... One or more selector functions to choose
#'  which variables will be used to compute the MCA.
#'
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new multiple corespondence columns created by the
#'  original variables will be used as predictors in a model
#'
#' @param num_f The number of MC variables to retain as new
#' predictors. Currently throws an error if non-positive values
#' are used.
#'
#' @param res The [MASS::mca()] object is stored here once
#'  this preprocessing step has been trained by [prep.recipe()]
#'
#' @param prefix A character string that will be the prefix for
#'  the new MC variables. Defaults to "MC".
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any).
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept mca
#' @concept projection_methods
#' @export
#' @details Multiple Correspondence Analysis is a
#'  dimension reduction method that works with
#'  categorical variables only, addressing the
#'  shortcomings of other dimention-reducing methods
#'  such as PCA.
#'
#' At this juncture, unlike the proper recipes found
#' within `tidymodels`, the variable names are not
#' padded with zeros.
#'
#' @references Venables, W. N. and Ripley, B. D. (2002). Modern
#'  Applied Statistics with S. Fourth edition. Springer.
#'
#'
#' @examples
#' # Using the Farms dataset
#' example_data <- MASS::farms
#'
#' # Original Data:
#' example_data
#'
#' example_rec <- recipes::recipe(Mois ~ ., data = example_data)
#'
#' example_mca <- step_mca(example_rec, recipes::all_predictors(), num_f = 5)
#'
#' # Prep the recipe
#'
#' mca_prepped <- recipes::prep(example_mca)
#'
#' # Transformed Data:
#' transformed_data <- recipes::juice(mca_prepped)
#'
#' head(transformed_data)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'










step_mca <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_f = 2,
  res = NULL,
  prefix = "MC",
  skip = FALSE,
  id = recipes::rand_id("mca")
) {

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_mca_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      num_f = num_f,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}


step_mca_new <-  function(
  terms,
  role,
  trained,
  num_f,
  options,
  res,
  prefix,
  skip,
  id
  ) {
  recipes::step(
      subclass = "mca",
      terms = terms,
      role = role,
      trained = trained,
      num_f = num_f,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
}



#' @export
prep.step_mca <- function(x, training, info = NULL, ...) {

  col_names <- recipes::terms_select(x$terms, info = info)

  recipes::check_type(training[, col_names], FALSE)

  max_length <- length(unique(unlist(training[,col_names]))) - 1

  x$num_f <- min(x$num_f, max_length)

  mca_call <- dplyr::expr(MASS::mca(x$num_f, abbrev = FALSE))

  mca_call$df <- dplyr::expr(training[, col_names])

  mca_obj <- eval(mca_call)

  step_mca_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_f = x$num_f,
    res = mca_obj,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )


}

#' @export
bake.step_mca <- function(object, new_data, ...) {

  mca_vars <- unique(gsub("\\..*", "", x=rownames(object$res$cs)))
  print(mca_vars)

  nfs <- stats::predict(eval(object$res), newdata = new_data[,mca_vars])

  colnames(nfs) <- paste0("MC", 1:ncol(nfs))

  new_data <- dplyr::bind_cols(new_data, tibble::as_tibble(nfs))

  tibble::as_tibble(new_data)

}

#' @export
num_f <- function(range = c(2, 5), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_f = "MCA Comps"),
    finalize = NULL
  )
}

#' @rdname required_pkgs.step
#' @export
tunable.step_mca <-  function(x, ...) {
  tibble::tibble(
    name = "num_f",
    call_info = list(list(pkg = "extradim", fun = "num_f", range = c(2L, 5L))),
    source = "recipe",
    component = "step_mca",
    component_id = x$id
  )
}








