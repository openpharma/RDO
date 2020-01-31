# library(testthat)
# RDO::RDO$debug("get_dependencies")
# RDO::RDO$undebug("get_dependencies")
options(digits.secs = 6)

# Single RDO without dependencies #############################################
context("Single RDO without dependencies")

# _RDO initialization ---------------------------------------------------------

data_1_loading <- RDO::RDO$new(name = "data_1_loading")

test_that("RDO initialization", {

  expect_equal(
    data_1_loading$get_name(),
    "data_1_loading")

  expect_equal(
    data_1_loading$cache,
    NULL)

  expect_equal(
    data_1_loading$r_code,
    NULL)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    class(data_1_loading$get_status()),
    "list")

  expect_equal(
    data_1_loading$get_status()$is_validated,
    FALSE)

  expect_equal(
    class(data_1_loading$get_status()$created),
    c("POSIXct", "POSIXt"))

})

# _setting r_code -------------------------------------------------------------

data_1_loading$r_code <- expression({
  data_1_loading <- mtcars
})

test_that("setting r_code", {capture_output({

  expect_equal(
    data_1_loading$r_code,
    expression(data_1_loading = {
      data_1_loading <- mtcars
      }))

  expect_equal(
    data_1_loading$get_r_code(),
    expression(data_1_loading = {
      data_1_loading <- mtcars
    }))

})})

# _evaluation of r_code ======================================================
test_that("evaluation of r_code", {capture_output({

  expect_equal(
    eval(data_1_loading$r_code, envir = new.env()),
    mtcars)

})})

# _caching the data -----------------------------------------------------------
test_that("running r_code and caching the data", {

  data_1_loading$cache <- NULL

  expect_equal(
    data_1_loading$cache,
    NULL)

  expect_equal(
    data_1_loading$run_r_code(cache = FALSE,
                              verbose = FALSE),
    mtcars)

  expect_equal(
    data_1_loading$cache,
    NULL)

  expect_equal(
    data_1_loading$run_r_code(verbose = FALSE)$cache,
    mtcars)

  expect_equal(
    data_1_loading$cache,
    mtcars)

})



# _implicit (in)validation ----------------------------------------------------
test_that("implicit (in)validation", {

  data_1_loading$cache <- mtcars

  data_1_loading$r_code <- expression({
    data_1_loading <- iris
  })

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    data_1_loading$run_r_code(verbose = FALSE)$cache,
    iris)

  expect_equal(
    data_1_loading$cache,
    iris)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  data_1_loading$r_code <- expression({
    data_1_loading <- mtcars
  })

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    data_1_loading$cache,
    iris)

  expect_equal(
    data_1_loading$run_r_code(verbose = FALSE)$cache,
    mtcars)

  expect_equal(
    data_1_loading$cache,
    mtcars)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

})


# _explicit (in)validation ----------------------------------------------------
test_that("explicit (in)validation", {

  data_1_loading$r_code <- expression({
    data_1_loading <- mtcars
  })

  data_1_loading$run_r_code(verbose = FALSE)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  expect_equal(
    data_1_loading$invalidate(verbose = FALSE)$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    eval(data_1_loading$get_r_code(), envir = new.env()),
    data_1_loading$cache)

  expect_equal(
    data_1_loading$validate(verbose = FALSE)$is_validated(verbose = FALSE),
    TRUE)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  data_1_loading$cache <- iris

  expect_equal(
    data_1_loading$validate(verbose = FALSE)$is_validated(verbose = FALSE),
    FALSE)

  data_1_loading$cache <- mtcars

  expect_equal(
    data_1_loading$validate(verbose = FALSE)$is_validated(verbose = FALSE),
    TRUE)

})





# Multiple RDOs with dependencies #############################################
context("Multiple RDOs with dependencies")

data_1_top_half <-
  RDO::RDO$new(name = "data_1_top_half",
               dependencies = list(data_1_loading))

data_1_bottom_half <-
  RDO::RDO$new(name = "data_1_bottom_half",
               dependencies = list(data_1_loading))

data_1_binding_rows <-
  RDO::RDO$new(name = "data_1_binding_rows",
               dependencies = list(data_1_top_half,
                                   data_1_bottom_half))


data_2_loading <-
  RDO::RDO$new(name = "data_2_loading")

data_2_select_rows <-
  RDO::RDO$new(name = "data_2_select_rows",
               dependencies = list(data_2_loading))

data_2_select_cols <-
  RDO::RDO$new(name = "data_2_select_cols",
               dependencies = list(data_2_select_rows))



data_3_binding_datasets <-
  RDO::RDO$new(name = "data_3_binding_datasets",
               dependencies = list(data_1_binding_rows,
                                   data_2_select_cols))



# _checking dependencies ------------------------------------------------------
test_that("checking dependencies", {

  expect_equal(
    names(data_1_top_half$get_dependencies()),
    c("data_1_loading"))

  expect_equal(
    names(data_1_bottom_half$get_dependencies()),
    c("data_1_loading"))

  expect_equal(
    names(data_1_binding_rows$get_dependencies()),
    c("data_1_top_half", "data_1_bottom_half"))

  expect_equal(
    data_1_loading$has_dependencies(),
    FALSE)

  expect_equal(
    data_1_top_half$has_dependencies(),
    TRUE)

  expect_equal(
    data_1_bottom_half$has_dependencies(),
    TRUE)

  expect_equal(
    data_1_binding_rows$has_dependencies(),
    TRUE)

  expect_equal(
    names(data_1_binding_rows$get_dependencies(deep = TRUE)),
    c("data_1_loading", "data_1_top_half", "data_1_bottom_half"))

  expect_equal(
    names(data_3_binding_datasets$get_dependencies(deep = TRUE)),
    c("data_1_loading",
      "data_1_top_half", "data_1_bottom_half",
      "data_2_loading", "data_2_select_rows", "data_1_binding_rows",
      "data_2_select_cols"))

})

# _setting r_code -------------------------------------------------------------

data_1_top_half$r_code <- expression({
  data_1_top_half <- head(data_1_loading, NROW(mtcars)/2)
})

data_1_bottom_half$r_code <- expression({
  data_1_bottom_half <- tail(data_1_loading, NROW(mtcars)/2)
})

data_1_binding_rows$r_code <- expression({
  data_1_binding_rows <- rbind(data_1_top_half, data_1_bottom_half)
})

data_2_loading$r_code <- expression({
  data_2_loading <- iris
})

data_2_select_rows$r_code <- expression({
  set.seed(1234)
  sampled_rows <- c(rep(TRUE, 32), rep(FALSE, NROW(iris) - 32))
  data_2_select_rows <- data_2_loading[sample(sampled_rows, NROW(iris)), ]
})

data_2_select_cols$r_code <- expression({
  data_2_select_cols <- data_2_select_rows[, c("Species", "Petal.Width")]
})

data_3_binding_datasets$r_code <- expression({
  data_3_binding_datasets <- cbind(data_2_select_cols, data_1_binding_rows)
})

test_that("getting r_code", {

  expect_equal(
    names(data_3_binding_datasets$get_r_code()),
    c("data_3_binding_datasets"))

  expect_equal(
    names(data_3_binding_datasets$get_r_code(deep = TRUE)),
    c("data_1_loading",
      "data_1_top_half", "data_1_bottom_half",
      "data_2_loading",
      "data_2_select_rows",
      "data_1_binding_rows",
      "data_2_select_cols",
      "data_3_binding_datasets"))

})


# _printing r_code -----------------------------------------------------------
test_that("printing r_code", {

  expect_output(
    data_1_binding_rows$print_r_code(),
    "^data_1_binding_rows.*$")

  expect_output(
    data_1_binding_rows$print_r_code(deep = TRUE),
    "^data_1_loading.*data_1_top_half.*data_1_bottom_half.*$")

  expect_output(
    data_1_binding_rows$r_code,
    "^data_1_loading.*data_1_binding_rows .*$")

})


# _running r_code deep-------------------------------------------------------
test_that("running r_code deep", {

  expect_equal(
    data_1_binding_rows$is_validated(verbose = FALSE),
    FALSE)

  expect_error(
    (data_3_binding_datasets$run_r_code(verbose = FALSE)))

  expect_equal(
    data_3_binding_datasets$run_r_code(deep = TRUE, verbose = FALSE)$cache,
    {
      set.seed(1234)
      sampled_rows <- c(rep(TRUE, 32), rep(FALSE, NROW(iris) - 32))
      cbind(
        iris[sample(sampled_rows, NROW(iris)), c("Species", "Petal.Width")],
        mtcars)
    })

  expect_output(
    data_3_binding_datasets$run_r_code(deep = TRUE, verbose = TRUE),
    "^.*data_1_binding_rows.*data_2_select_cols' is validated.*$")

})




# _invalidating tree element --------------------------------------------------
test_that("invalidating tree element", {

  expect_equal(
    data_3_binding_datasets$is_validated(verbose = FALSE),
    TRUE)

  expect_equal(
    data_3_binding_datasets$is_validated(deep = TRUE, verbose = FALSE),
    TRUE)

  expect_equal(
    (data_2_select_rows$invalidate(verbose = FALSE)$is_validated(
      verbose = FALSE)),
    FALSE)

  expect_equal(
    data_3_binding_datasets$is_validated(deep = TRUE, verbose = FALSE),
    FALSE)

  expect_output(
    data_3_binding_datasets$is_validated(deep = TRUE),
    "^.*NOT VALIDATED.*$")

  expect_equal(
    data_2_select_rows$validate(verbose = FALSE)$is_validated(
      verbose = FALSE),
    TRUE)

  expect_equal(
    data_3_binding_datasets$is_validated(deep = TRUE, verbose = FALSE),
    TRUE)

  data_2_select_rows$cache <- NULL

  expect_equal(
    data_2_select_rows$validate(verbose = FALSE)$is_validated(
      verbose = FALSE),
    FALSE)

  expect_output(
    data_3_binding_datasets$run_r_code(deep = TRUE, verbose = TRUE),
    "^.*have changed!.*$")

  expect_equal(
    data_2_select_rows$is_validated(verbose = FALSE),
    TRUE)

})


# _invalidating deeply --------------------------------------------------------
test_that("invalidating deeply", {

  data_3_binding_datasets$invalidate(deep = TRUE, verbose = FALSE)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  data_3_binding_datasets$run_r_code(deep = TRUE, verbose = FALSE)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  expect_equal(
    data_3_binding_datasets$is_validated(deep = TRUE, verbose = FALSE),
    TRUE)

})



# _adding test RDO -----------------------------------------------------------
data_3_tests <-
  RDO::RDO$new(name = "data_3_tests",
               dependencies = list(data_3_binding_datasets))

data_3_tests$r_code <- expression({

  stopifnot("mpg" %in% names(data_3_binding_datasets))
  data_3_tests <- data_3_binding_datasets

})

data_3_tests$run_r_code(verbose = FALSE)

test_that("adding test RDO", {

  data_1_loading$r_code <- expression({
    data_1_loading <- iris
  })

  expect_error(
    data_3_tests$run_r_code(deep = TRUE, verbose = FALSE))

  data_1_loading$r_code <- expression({
    data_1_loading <- mtcars
  })

  data_3_tests$run_r_code(deep = TRUE, verbose = FALSE)

})


# _prunning cache -------------------------------------------------------------
test_that("prunning cache", {

  expect_equal(
    data_3_tests$get_cache_size(deep = TRUE, verbose = FALSE) %>%
      unlist() %>%
      sum(),
    48256)

  expect_equal(
    (data_3_tests$prune_cache(
      deep = TRUE, verbose = FALSE)$get_cache_size(deep = TRUE,
                                                   verbose = FALSE)) %>%
      unlist() %>%
      sum(),
    6144)

  data_3_tests$run_r_code(deep = TRUE, verbose = FALSE)

  expect_equal(
    data_3_tests$get_cache_size(deep = TRUE, verbose = FALSE) %>%
      unlist() %>%
      sum(),
    48256)

})


# _prunning tree with locked RDOs ---------------------------------------------
test_that("prunning tree with locked RDOs", {

  expect_equal(
    data_3_tests$get_cache_size(deep = TRUE, verbose = FALSE) %>%
      unlist() %>%
      sum(),
    48256)

  expect_equal(
    data_1_loading$lock(verbose = FALSE)$is_locked(verbose = FALSE),
    TRUE)

  expect_equal(
    data_2_loading$lock(verbose = FALSE)$is_locked(verbose = FALSE),
    TRUE)

  expect_equal(
    (data_3_tests$prune_cache(
      deep = TRUE, verbose = FALSE)$get_cache_size(deep = TRUE,
                                                   verbose = FALSE)) %>%
      unlist() %>%
      sum(),
    20608)

  expect_equal(
    data_3_tests$lock(verbose = FALSE)$is_locked(verbose = FALSE),
    TRUE)

  expect_error(
    data_3_tests$cache <- NULL)

  expect_equal(
    data_3_tests$unlock(verbose = FALSE)$is_locked(verbose = FALSE),
    FALSE)

})

# _validating deep r_code -----------------------------------------------------
test_that("validating deep r_code", {

  expect_equal(
    data_3_tests$is_validated(deep = TRUE, verbose = FALSE),
    FALSE)

  expect_equal(
    data_3_tests$invalidate(verbose = FALSE)$is_validated(deep = FALSE,
                                                          verbose = FALSE),
    FALSE)

  expect_error(
    data_3_tests$validate(deep = FALSE, verbose = FALSE))

  expect_equal(
    data_3_tests$validate(deep = TRUE,
                          verbose = FALSE)$is_validated(verbose = FALSE),
    TRUE)

})


# _deep locking --------------------------------------------------------------
test_that("deep locking", {

  expect_equal(
    data_3_tests$is_locked(deep = TRUE, verbose = FALSE),
    FALSE)

  expect_equal(
    data_3_tests$unlock(deep = TRUE,
                        verbose = FALSE)$is_locked(deep = TRUE,
                                                   verbose = FALSE),
    FALSE)

  expect_equal(
    data_3_tests$lock(deep = TRUE,
                      verbose = FALSE)$is_locked(deep = TRUE,
                                                 verbose = FALSE),
    TRUE)

})

# _removing intermediary RDOs -----------------------------------------------

  rm(data_1_top_half, data_1_bottom_half,
     data_1_binding_rows,
     data_2_select_cols, data_2_select_rows,
     data_3_binding_datasets)

test_that("removing intermediary RDOs", {

  expect_false(
    data_3_tests$unlock(
      deep = TRUE,
      verbose = FALSE)$prune_cache(
        deep = TRUE,
        verbose = FALSE)$invalidate(
          deep = TRUE,
          verbose = FALSE)$is_validated(
            deep = TRUE,
            verbose = FALSE))

  expect_true(
    data_3_tests$run_r_code(
      deep = TRUE,
      verbose = FALSE)$is_validated(
        deep = TRUE,
        verbose = FALSE))

  expect_equal(
    data_3_tests$get_dependencies(deep = TRUE)$data_1_binding_rows$cache,
    mtcars)



})


# _shared dependencies -------------------------------------------------------
test_that("shared dependencies", {

  expect_true(
    data_1_loading$is_validated(verbose = FALSE))

  expect_false(
    data_3_tests$get_dependencies(
      deep = TRUE)$data_1_loading$invalidate(
        verbose = FALSE)$is_validated(verbose = FALSE))

  expect_false(
    data_1_loading$is_validated(verbose = FALSE))

  expect_true(
    data_3_tests$get_dependencies(
      deep = TRUE)$data_1_top_half$get_dependencies(
        deep = TRUE)$data_1_loading$validate(
          verbose = FALSE)$is_validated(
            verbose = FALSE))

  expect_true(
    data_1_loading$is_validated(verbose = FALSE))

})



test_that("deep cloning", {

  data_3_cloned <- data_3_tests$clone(deep = TRUE)

  data_1_loading_cloned <-
    data_3_cloned$get_dependencies(deep = TRUE)$data_1_loading

  data_1_loading_cloned$r_code <-
    expression(data_1_loading = {
      data_1_loading <- iris
      })

  data_3_cloned$get_dependencies(
    deep = TRUE)$data_1_top_half$add_dependencies(data_1_loading_cloned)

  data_3_cloned$get_dependencies(
    deep = TRUE)$data_1_bottom_half$add_dependencies(data_1_loading_cloned)

  expect_true(
    data_3_cloned$invalidate(
      deep = TRUE, verbose = FALSE
      )$get_dependencies()$data_3_binding_datasets$run_r_code(
        deep = TRUE, verbose = FALSE)$is_validated(deep = TRUE, verbose = FALSE))

  expect_equal(
    data_3_cloned$get_dependencies(
      deep = TRUE)$data_1_top_half$get_dependencies(
        deep = TRUE)$data_1_loading$cache,
    iris)

  expect_equal(
    data_3_cloned$get_dependencies(
      deep = TRUE)$data_1_bottom_half$get_dependencies(
        deep = TRUE)$data_1_loading$cache,
    iris)

  expect_equal(
    data_3_tests$get_dependencies(
      deep = TRUE)$data_1_bottom_half$get_dependencies(
        deep = TRUE)$data_1_loading$cache,
    mtcars)

  expect_equal(
    data_3_tests$get_dependencies(
      deep = TRUE)$data_1_top_half$get_dependencies(
        deep = TRUE)$data_1_loading$cache,
    mtcars)

  expect_true(
    all(c("Species", "mpg") %in%  names(
      data_3_tests$invalidate(
        deep = TRUE, verbose = FALSE
        )$get_dependencies()$data_3_binding_datasets$run_r_code(
          deep = TRUE, verbose = FALSE)$cache)))

  expect_false(
    all(c("Species", "mpg") %in%  names(
      data_3_cloned$invalidate(
        deep = TRUE, verbose = FALSE
        )$get_dependencies()$data_3_binding_datasets$run_r_code(
          deep = TRUE, verbose = FALSE)$cache)))


  data_3_cloned <- data_3_tests$clone(deep = TRUE)

  data_3_cloned$prune_dependencies(verbose = FALSE)

  data_1_loading_cloned <-
    data_3_cloned$get_dependencies(deep = TRUE)$data_1_loading

  data_1_loading_cloned$r_code <-
    expression(data_1_loading = {
      data_1_loading <- iris
      })

  expect_true(
    all(c("Species", "mpg") %in%  names(
      data_3_tests$invalidate(
        deep = TRUE, verbose = FALSE
        )$get_dependencies()$data_3_binding_datasets$run_r_code(
          deep = TRUE, verbose = FALSE)$cache)))

  expect_false(
    all(c("Species", "mpg") %in%  names(
      data_3_cloned$invalidate(
        deep = TRUE, verbose = FALSE
        )$get_dependencies()$data_3_binding_datasets$run_r_code(
          deep = TRUE, verbose = FALSE)$cache)))


})



# _converting RDOs -------------------------------------------------------------
capture.output(eval(expr = data_3_tests$r_code))

test_that("replacing RDOs", {

  expect_true(
    is.data.frame(data_3_tests))

})
