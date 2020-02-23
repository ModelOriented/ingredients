context("unit test for bind_plots")

# Setup -------------------------------------------------------------------
testthat::setup({
    assign("test_env", testthat::test_env(), envir = parent.frame())

    data <- mtcars
    data[, "vs"] <- factor(data[, "vs"], levels = 0:1, labels = c("V-shaped", "straight"))
    mtcars_ggplot_0_0 <- ggplot2::ggplot(data)
    mtcars_ggplot_1_1 <- mtcars_ggplot_0_0 + ggplot2::geom_point(aes(x = mpg, y = wt))
    mtcars_ggplot_1_2 <- mtcars_ggplot_1_1 + ggplot2::facet_grid(.~vs)
    mtcars_ggplot_1_3 <- mtcars_ggplot_1_1 + ggplot2::facet_grid(.~cyl)
    mtcars_ggplot_2_3 <- mtcars_ggplot_1_1 + ggplot2::facet_grid(vs~cyl)

    test_env$mtcars_ggplot_0_0 <- mtcars_ggplot_0_0
    test_env$mtcars_ggplot_1_1 <- mtcars_ggplot_1_1
    test_env$mtcars_ggplot_1_2 <- mtcars_ggplot_1_2
    test_env$mtcars_ggplot_1_3 <- mtcars_ggplot_1_3
    test_env$mtcars_ggplot_2_3 <- mtcars_ggplot_2_3
})

# bind_plots by columns ---------------------------------------------------
test_that("bind_plots works with two NULL ggplot objects", {
    attach(test_env)
    mtcars_ggplot_0_0 <- test_env$mtcars_ggplot_0_0
    expect_class(gtable <- bind_plots(mtcars_ggplot_0_0, mtcars_ggplot_0_0), "gtable")
    expect_length(gtable$heights, 1)
    expect_length(gtable$widths, 2)
})

test_that("bind_plots works with two 1x1-facet ggplot objects", {
    attach(test_env)
    mtcars_ggplot_1_1 <- test_env$mtcars_ggplot_1_1
    expect_class(gtable <- bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_1_1), "gtable")
    expect_length(gtable$heights, 1)
    expect_length(gtable$widths, 2)
})

test_that("bind_plots works with two 1x2-facet ggplot objects", {
    attach(test_env)
    mtcars_ggplot_1_2 <- test_env$mtcars_ggplot_1_2
    expect_class(gtable <- bind_plots(mtcars_ggplot_1_2, mtcars_ggplot_1_2), "gtable")
    expect_length(gtable$heights, 1)
    expect_length(gtable$widths, 4)
})

test_that("bind_plots works with 1x1 and 1x2 facet ggplot objects", {
    attach(test_env)
    mtcars_ggplot_1_1 <- test_env$mtcars_ggplot_1_1
    mtcars_ggplot_1_2 <- test_env$mtcars_ggplot_1_2

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_1_2), "gtable")
    expect_length(gtable$heights, 1)
    expect_length(gtable$widths, 3)

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_2, mtcars_ggplot_1_1), "gtable")
    expect_length(gtable$heights, 1)
    expect_length(gtable$widths, 3)
})

test_that("bind_plots works with 1x1 and 2x3 facet ggplot objects", {
    attach(test_env)
    mtcars_ggplot_1_1 <- test_env$mtcars_ggplot_1_1
    mtcars_ggplot_2_3 <- test_env$mtcars_ggplot_2_3

    expect_class(gtable <- bind_plots(mtcars_ggplot_2_3, mtcars_ggplot_1_1), "gtable")
    expect_length(gtable$heights, 2)
    expect_length(gtable$widths, 4)

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_2_3), "gtable")
    expect_length(gtable$heights, 2)
    expect_length(gtable$widths, 4)
})

test_that("bind_plots works with 1x2-and 2x3 facet ggplot objects", {
    attach(test_env)
    mtcars_ggplot_1_2 <- test_env$mtcars_ggplot_1_2
    mtcars_ggplot_2_3 <- test_env$mtcars_ggplot_2_3

    expect_class(gtable <- bind_plots(mtcars_ggplot_2_3, mtcars_ggplot_1_2), "gtable")
    expect_length(gtable$heights, 2)
    expect_length(gtable$widths, 5)

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_2, mtcars_ggplot_2_3), "gtable")
    expect_length(gtable$heights, 2)
    expect_length(gtable$widths, 5)
})

# bind_plots by rows ------------------------------------------------------
test_that("bind_plots works binds by rows", {
    attach(test_env)
    mtcars_ggplot_1_2 <- test_env$mtcars_ggplot_1_2
    mtcars_ggplot_2_3 <- test_env$mtcars_ggplot_2_3

    expect_class(gtable <- bind_plots(mtcars_ggplot_2_3, mtcars_ggplot_1_2, byrow = TRUE), "gtable")
    expect_length(gtable$heights, 3)
    expect_length(gtable$widths, 3)

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_2, mtcars_ggplot_2_3, byrow = TRUE), "gtable")
    expect_length(gtable$heights, 3)
    expect_length(gtable$widths, 3)
})

# bind_plots with more than two ggplot objects ----------------------------
test_that("bind_plots works with more than two ggplot objects", {
    attach(test_env)
    mtcars_ggplot_1_1 <- test_env$mtcars_ggplot_1_1
    mtcars_ggplot_1_2 <- test_env$mtcars_ggplot_1_2
    mtcars_ggplot_2_3 <- test_env$mtcars_ggplot_2_3

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_1_2, mtcars_ggplot_2_3, byrow = FALSE), "gtable")
    expect_length(gtable$heights, 2)
    expect_length(gtable$widths, 6)

    expect_class(gtable <- bind_plots(mtcars_ggplot_1_1, mtcars_ggplot_1_2, mtcars_ggplot_2_3, byrow = TRUE), "gtable")
    expect_length(gtable$heights, 4)
    expect_length(gtable$widths, 3)
})


