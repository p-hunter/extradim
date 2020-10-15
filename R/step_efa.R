#' Factor Analysis (Exploratory)
#'
#'`step_efa` creates a *specification* of a
#'  recipe step that will compute a factor analysis
#'  of a set of variables.
#'
#'
#' @param ... One or more selector functions to choose
#'  which variables will be used to compute the efa.
#'
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new multiple corespondence columns created by the
#'  original variables will be used as predictors in a model
#'
#' @param num_comp The number of MC variables to retain as new
#' predictors. Currently throws an error if non-positive values
#' are used.
#'
#' @param res The [psych::fa()] object is stored here once
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
#' @concept fa
#' @concept projection_methods
#' @export
#' @details Factor analysis is a dimension reduction
#' method that uses estimation to rotate variables
#' rather than the empirical PCA method.
#' It seeks to find commonality between variables and
#' lump these into one factor (not to be confused with
#' the data type!). This recipe step assumes linearity
#' whereas step_efa_poly does not.
#'
#' At this juncture, unlike the proper recipes found
#' within `tidymodels`, the variable names are not
#' padded with zeros.
#'
#' @references Goldberg, L.R. (1999) A broad-bandwidth,
#'  public domain, personality inventory measuring
#'  the lower-level facets of several five-factor models.
#'  In Mervielde, I. and Deary, I. and De Fruyt, F.
#'  and Ostendorf, F. (eds) Personality psychology in Europe. 7.
#'  Tilburg University Press. Tilburg, The Netherlands.
#'
#'
#' @examples
#' # Using the Farms dataset
#' example_data <- psych::bfi
#'
#' # Original Data:
#' head(example_data)
#'
#' example_rec <- recipes::recipe(age ~ ., data = example_data)
#'
#' example_efa <- step_efa(example_rec, recipes::all_predictors(), num_comp = 3)
#'
#' # Prep the recipe
#'
#' efa_prepped <- recipes::prep(example_efa)
#'
#' # Transformed Data:
#'
#' transformed_data <- recipes::juice(efa_prepped)
#'
#' head(transformed_data)
#'
#'
#'






step_efa <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  res = NULL,
  prefix = "MC",
  skip = FALSE,
  id = recipes::rand_id("efa")
) {

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_efa_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}


step_efa_new <-  function(
  terms,
  role,
  trained,
  num_comp,
  options,
  res,
  prefix,
  skip,
  id
) {
  recipes::step(
    subclass = "efa",
    terms = terms,
    role = role,
    trained = trained,
    num_comp = num_comp,
    res = res,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_efa <- function(x, training, info = NULL, ...) {

  col_names <- recipes::terms_select(x$terms, info = info)

  recipes::check_type(training[, col_names])

  max_length <- length(col_names)

  x$num_comp <- min(x$num_comp, max_length)

  efa_call <- dplyr::expr(psych::fa( nfactors = x$num_comp))

  efa_call$r <- dplyr::expr(training[, col_names])

  efa_obj <- eval(efa_call)

  step_efa_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    res = efa_obj,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )


}



#' @export
bake.step_efa <- function(object, new_data, ...) {

  efa_vars <- colnames(object$res$r)

print(efa_vars)

print(new_data[,efa_vars])
  comps <- stats::predict(eval(object$res), data = new_data[,efa_vars])

  colnames(comps) <- paste0("Factor", 1:object$res$factors)

  new_data <- dplyr::bind_cols(new_data, tibble::as_tibble(comps))

  tibble::as_tibble(new_data)

}

#' @rdname tunable.step
#' @export
tunable.step_efa <-  function(x, ...) {
  tibble::tibble(
    name = "num_comp",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1L, 5L))),
    source = "recipe",
    component = "step_efa",
    component_id = x$id
  )
}


#' @rdname required_pkgs.step
#' @export
required_pkgs.step_efa <- function(x, ...) {
  c("psych", "GPArotation")
}




