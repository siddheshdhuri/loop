
logical_js <- function(value){
  if(value) return('true')
  return('false')
}



#' @title Creates an autocomplete text input field
#'
#' @description autocomplete_input creates an autocomplete text input field,
#' showing all possible options from a given list under the input while typing.
#' Alternative to very slow select(ize) inputs for (very) large option lists.
#'
#' @param id id of the element
#' @param label label to show for the input, NULL for no label
#' @param options list (or vector) of possible options
#' @param value initial value
#' @param width optional, the width of the input, see
#' \code{\link{validateCssUnit}}
#' @param placeholder optional character specifying the placeholder text
#' @param max_options optional numeric specifying the maximum number of
#' options to show (for performance reasons)
#' @param hide_values optional boolean indicating whether to show values
#' under labels or not
#' @param create optional boolean to enable entering values not in the list
#' @param contains optional boolean to enable searching in the middle of options
#'
#' @return autocomplete_input: shiny input element
#'
#' @export
autocomplete_input <- function(
  id, label, options, value = "", width = NULL, placeholder = NULL,
  max_options = 0, hide_values = FALSE, create = FALSE, contains = FALSE
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is needed to convert list of options into json!")
  }
  value <- shiny::restoreInput(id = id, default = value)
  js_opts <- jsonlite::toJSON(as.list(options), auto_unbox = TRUE)
  width <- shiny::validateCssUnit(width)
  if (length(value) == 0L) value <- ""
  shiny::div(
    class = "form-group shiny-input-container autocomplete",
    style = if (!is.null(width)) paste0("width: ", width, ";"),
    if (!is.null(label)) shiny::tags$label(label, `for` = id),
    shiny::tags$input(
      id = id, type = "text", class = "form-control", result = value,
      value = value, placeholder = placeholder, "data-options" = js_opts,
      "data-max" = max_options, "data-contains" = logical_js(contains),
      "data-hide" = logical_js(hide_values), "data-create" = logical_js(create),
      autocomplete = "off"
    ),
    htmltools::htmlDependency(
      "autocomplete", "0.0.1", c(href = "custom"),
      script = "js/autocomplete-binding.js", stylesheet = "css/autocomplete.css"
    )
  )
}

#' @description update_autocomplete_input changes the value or the options of an
#' autocomplete input element on the client side.
#'
#' @param session the shiny session object
#'
#' @return update_autocomplete_input: message to the client
#' @export
#' @rdname autocomplete_input
update_autocomplete_input <- function(
  session, id, label = NULL, options = NULL, max_options = NULL, value = NULL,
  placeholder = NULL, hide_values = NULL, create = NULL, contains = NULL
) {
  message <- not_null(list(
    label = label, options = options, value = value, max = max_options,
    placeholder = placeholder, hide = hide_values, create = create,
    contains = contains
  ))
  session$sendInputMessage(id, message)
}