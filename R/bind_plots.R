#' @title Bind Multiple ggplot Objects
#'
#' @description This is an aesthetically efficient implementation of the
#'   \link[gridExtra]{grid.arrange}.
#'
#' @param ... (\code{ggplot}) ggplot objects to combine.
#' @param byrow (\code{logical}) if \code{FALSE} (the default) the plots are bind by
#'   columns, otherwise the plots are bind by rows.
#'
#' @return (\code{gtable}) A plottable object with \code{plot()}.
#'
#' @author \url{https://github.com/harell}
#'
#' @examples
#' \dontrun{
#' library("DALEX")
#' titanic_glm <- glm(survived ~ gender + age + fare,
#'                    data = titanic_imputed, family = "binomial")
#'
#' explain_glm <- explain(titanic_glm,
#'                        data = titanic_imputed,
#'                        y = titanic_imputed$survived,
#'                        verbose = FALSE)
#'
#' pdp_numerical <- partial_dependence(explain_glm, N = 50, variable_type = "numerical")
#' pdp_categorical <- partial_dependence(explain_glm, N = 50, variable_type = "categorical")
#'
#' # Bind plots by rows
#' bind_plots(plot(pdp_numerical), plot(pdp_categorical), byrow = TRUE)
#'
#' # Bind plots by columns
#' bind_plots(plot(pdp_numerical), plot(pdp_categorical), byrow = FALSE)
#' }
#'
#' @export
#' @rdname bind_plots
bind_plots <- function(..., byrow = FALSE){
    # Helper Functions --------------------------------------------------------
    extract_facets <- function(p) ggplot2::ggplot_build(p)[['layout']][['layout']]
    extract_facets_nrow <- function(p) length(unique(extract_facets(p)[['ROW']]))
    extract_facets_ncol <- function(p) length(unique(extract_facets(p)[['COL']]))
    extract_facets_dim <- function(p) c(extract_facets_nrow(p), extract_facets_ncol(p))
    pseudo_facet_grid <- function(p) p + ggplot2::facet_grid(~"")
    has_facets <- function(p) identical(prod(extract_facets_dim(p)), 1)
    add_pseudo_facet_grid <- function(p) if(has_facets(p)) pseudo_facet_grid(p) else p

    # Defensive Programming ---------------------------------------------------
    stopifnot(is.logical(byrow))
    sapply(list(...), function(x) stopifnot(ggplot2::is.ggplot(x)))

    # Programming Logic -------------------------------------------------------
    gg_objects <- list(...)
    gg_objects_len <- length(gg_objects)
    gg_objects_dim <- lapply(gg_objects, extract_facets_dim)
    gg_objects_nrow <- sapply(gg_objects_dim, function(x) x[1])
    gg_objects_ncol <- sapply(gg_objects_dim, function(x) x[2])

    gg_objects <- lapply(gg_objects, add_pseudo_facet_grid)

    n <- sum(sapply(gg_objects_dim, prod))
    nrow <- if(byrow) sum(gg_objects_nrow) else max(gg_objects_nrow)
    ncol <- if(!byrow) sum(gg_objects_ncol) else max(gg_objects_ncol)

    layout_matrix <- matrix(NA_real_, nrow, ncol, byrow)
    x <- y <- 1
    for(i in seq_len(gg_objects_len)){
        w <- gg_objects_ncol[i]; h <- gg_objects_nrow[i]
        layout_matrix[y:(y+h-1), x:(x+w-1)] <- i
        if(byrow) y <- y + h else x <- x + w
    }

    gridExtra::grid.arrange(grobs = gg_objects, layout_matrix = layout_matrix)
}
