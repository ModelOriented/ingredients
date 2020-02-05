#' @title Bind Multiple ggplot Objects
#'
#' @description This is an aesthetically efficient implementation of the
#'   \link[gridExtra]{grid.arrange}.
#'
#' @param ... (`ggplot`) ggplot objects to combine.
#' @param byrow (`logical`) if FALSE (the default) the plots are bind by
#'   columns, otherwise the plots are bind by rows.
#'
#' @return (`gtable`) A plottable object with \code{plot()}.
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars_ggplot_0_0 <- ggplot2::ggplot(mtcars)
#' mtcars_ggplot_1_1 <- mtcars_ggplot_0_0 + ggplot2::geom_point(aes(x = mpg, y = wt))
#' mtcars_ggplot_1_2 <- mtcars_ggplot_1_1 + ggplot2::facet_grid(.~vs)
#' # Bind plots by rows: the result is a plot with 2 columns and 2 rows
#' bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_1_2, byrow = TRUE)
#' # Bind plots by columns: the result is a plot with 3 columns and 1 row
#' bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_1_2, byrow = FALSE)
#' }
#'
bind_plots <- function(..., byrow = FALSE){
    # Helper Functions --------------------------------------------------------
    extract_facets <- function(p) p %>% ggplot2::ggplot_build() %>% magrittr::extract2('layout') %>% magrittr::extract2('layout')
    extract_facets_nrow <- function(p) p %>% extract_facets() %>% magrittr::extract2('ROW') %>% unique() %>%length()
    extract_facets_ncol <- function(p) p %>% extract_facets() %>% magrittr::extract2('COL') %>% unique() %>%length()
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



