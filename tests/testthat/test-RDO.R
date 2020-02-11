# library(testthat)
# RDO::RDO$debug("get_dependencies")
# RDO::RDO$undebug("get_dependencies")
# Sys.setenv("RDO_VERBOSE" = FALSE)

# RDOs definitions ############################################################
data_mtcars <- RDO::RDO$new(name = "data_mtcars")

data_mtcars$code <- expression({
  data_mtcars <- mtcars
})


mtcars_half_top <-
  RDO::RDO$new(name = "mtcars_half_top",
               dependencies = list(data_mtcars))

mtcars_half_top$code <- expression({
  mtcars_half_top <- head(data_mtcars, 16)
})


mtcars_half_bottom <-
  RDO::RDO$new(name = "mtcars_half_bottom",
               dependencies = list(data_mtcars))

mtcars_half_bottom$code <- expression({
  mtcars_half_bottom <- tail(data_mtcars, 16)
})


mtcars_whole <-
  RDO::RDO$new(name = "mtcars_whole",
               dependencies = list(mtcars_half_top,
                                   mtcars_half_bottom))

mtcars_whole$code <- expression({
  mtcars_whole <- rbind(mtcars_half_top,
                        mtcars_half_bottom)
})


data_iris <-
  RDO::RDO$new(name = "data_iris")

data_iris$code <- expression({
  data_iris <- iris
})


iris_selected_rows <-
  RDO::RDO$new(name = "iris_selected_rows",
               dependencies = list(data_iris))

iris_selected_rows$code <- expression({
  set.seed(1234)
  sampled_rows <- c(rep(TRUE, 32), rep(FALSE, NROW(data_iris) - 32))
  iris_selected_rows <- data_iris[sample(sampled_rows, NROW(data_iris)), ]
})


iris_selected_columns <-
  RDO::RDO$new(name = "iris_selected_columns",
               dependencies = list(iris_selected_rows))

iris_selected_columns$code <- expression({
  iris_selected_columns <- iris_selected_rows[, c("Species", "Petal.Width")]
})


iris_mtcars <-
  RDO::RDO$new(name = "iris_mtcars",
               dependencies = list(mtcars_whole,
                                   iris_selected_columns))

iris_mtcars$code <- expression({
  iris_mtcars <- cbind(iris_selected_columns, mtcars_whole)
})


iris_mtcars_test <-
  RDO::RDO$new(name = "iris_mtcars_test",
               dependencies = list(iris_mtcars))

iris_mtcars_test$code <- expression({

  stopifnot("mpg" %in% names(iris_mtcars))
  iris_mtcars <- iris_mtcars

})

# RDO without dependencies ####################################################
context("RDO without dependencies")

# _RDO initialization ---------------------------------------------------------
test_that("RDO after initialization", {

  data_mtcars$code <-  NULL

  expect_equal(
    data_mtcars$get_name(),
    "data_mtcars")

  expect_equal(
    data_mtcars$cache,
    NULL)

  expect_equal(
    as.numeric(data_mtcars$get_cache_size()$data_mtcars),
    0)

  # expect_equal(
  #   is.character(data_mtcars$code),
  #   TRUE)

  expect_equal(
    data_mtcars$get_code(),
    NULL)

  expect_equal(
    data_mtcars$is_validated(),
    FALSE)

  expect_equal(
    class(data_mtcars$get_status()),
    "list")

  expect_equal(
    data_mtcars$get_status()$is_validated,
    FALSE)

  expect_equal(
    data_mtcars$get_status()$is_locked,
    FALSE)

  expect_equal(
    class(data_mtcars$get_status()$created),
    c("POSIXct", "POSIXt"))

  expect_equal(
    class(data_mtcars$get_status()$changed),
    c("POSIXct", "POSIXt"))

  expect_equal(
    data_mtcars$has_dependencies(),
    FALSE)

  expect_equal(
    data_mtcars$get_dependencies(),
    list())

})

# _setting code -------------------------------------------------------------
data_mtcars$code <- expression({
  data_mtcars <- mtcars
})

test_that("setting code", {capture_output({

  expect_equal(
    data_mtcars$get_code(),
    expression(data_mtcars = {
      data_mtcars <- mtcars
    }))

})})

# _running of code -------------------------------------------------------
test_that("running code", {capture_output({

  expect_equal(
    eval(data_mtcars$get_code(), envir = new.env()),
    mtcars)

  expect_equal(
    eval(data_mtcars$get_code(deep = TRUE), envir = new.env()),
    mtcars)

  expect_equal(
    data_mtcars$run(cache = FALSE),
    mtcars)

})})

# _caching the data -----------------------------------------------------------
test_that("running code and caching the data", {

  expect_equal(
    data_mtcars$cache,
    NULL)

  expect_equal(
    data_mtcars$run(cache = FALSE),
    mtcars)

  expect_equal(
    data_mtcars$cache,
    NULL)

  expect_equal(
    data_mtcars$run()$cache,
    mtcars)

})

# _implicit (in)validation ----------------------------------------------------
test_that("implicit (in)validation", {

  expect_equal(
    data_mtcars$is_validated(),
    TRUE)

  data_mtcars$code <- expression({
    data_mtcars <- iris
  })

  expect_equal(
    data_mtcars$is_validated(),
    FALSE)

  expect_equal(
    data_mtcars$run()$is_validated(),
    TRUE)

  data_mtcars$cache <- mtcars

  expect_equal(
    data_mtcars$is_validated(),
    FALSE)

  expect_equal(
    data_mtcars$run()$is_validated(),
    TRUE)

  expect_equal(
    data_mtcars$cache,
    iris)

  data_mtcars$code <- expression({
    data_mtcars <- mtcars
  })

  expect_equal(
    data_mtcars$is_validated(),
    FALSE)

  expect_equal(
    data_mtcars$run()$cache,
    mtcars)

  expect_equal(
    data_mtcars$is_validated(),
    TRUE)

})


# _explicit (in)validation ----------------------------------------------------
test_that("explicit (in)validation", {

  expect_equal(
    data_mtcars$run()$is_validated(),
    TRUE)

  expect_equal(
    data_mtcars$invalidate()$is_validated(),
    FALSE)

  expect_equal(
    data_mtcars$validate()$is_validated(),
    TRUE)

  expect_equal(
    data_mtcars$invalidate()$run()$is_validated(),
    TRUE)

})

# RDO with dependencies #######################################################
context("RDO with dependencies")

# _printing RDO ---------------------------------------------------------------
test_that("printing RDO", {
  expect_output(
    print(mtcars_whole),
    "Name.*mtcars_whole")

  expect_output(
    print(mtcars_whole),
    "Dependencies.*mtcars_half_top.*mtcars_half_bottom")

  expect_output(
    print(mtcars_whole),
    "Status.*created")

  expect_output(
    print(mtcars_whole),
    "Status.*changed.")

  expect_output(
    print(mtcars_whole),
    "Status.*validated")

  expect_output(
    print(mtcars_whole),
    "Status.*is locked")

  expect_output(
    print(mtcars_whole),
    "Status.*is validated")

  expect_output(
    print(mtcars_whole),
    "Status.*touched.*never.*?$")

  mtcars_whole$cache

  expect_output(
    print(mtcars_whole),
    ".*?touched.*UTC")

})

# _checking dependencies ------------------------------------------------------
test_that("checking dependencies", {

  expect_equal(
    mtcars_half_top$has_dependencies(),
    TRUE)

  expect_equal(
    mtcars_half_bottom$has_dependencies(),
    TRUE)

  expect_equal(
    mtcars_whole$has_dependencies(),
    TRUE)

  expect_equal(
    names(mtcars_half_top$get_dependencies()),
    c("data_mtcars"))

  expect_equal(
    names(mtcars_half_bottom$get_dependencies()),
    c("data_mtcars"))

  expect_equal(
    names(mtcars_whole$get_dependencies()),
    c("mtcars_half_top", "mtcars_half_bottom"))

  expect_equal(
    names(mtcars_whole$get_dependencies(deep = TRUE)),
    c("data_mtcars", "mtcars_half_top", "mtcars_half_bottom"))

  expect_equal(
    names(iris_mtcars$get_dependencies(deep = TRUE)),
    c("data_mtcars",
      "mtcars_half_top", "mtcars_half_bottom",
      "data_iris", "iris_selected_rows", "mtcars_whole",
      "iris_selected_columns"))

})

# _getting code -------------------------------------------------------------
test_that("getting code", {

  expect_equal(
    names(iris_mtcars$get_code()),
    c("iris_mtcars"))

  expect_equal(
    names(iris_mtcars$get_code(deep = TRUE)),
    c("data_mtcars",
      "mtcars_half_top", "mtcars_half_bottom",
      "data_iris",
      "iris_selected_rows",
      "mtcars_whole",
      "iris_selected_columns",
      "iris_mtcars"))

})

# _printing code -----------------------------------------------------------
test_that("printing code", {

  expect_output(
    mtcars_whole$print_code(),
    "^mtcars_whole.*$")

  expect_output(
    mtcars_whole$print_code(deep = TRUE),
    "^data_mtcars.*mtcars_half_top.*mtcars_half_bottom.*$")

  # expect_output(
  #   cat(mtcars_whole$code),
  #   "^data_mtcars.*mtcars_whole .*$")

})

# _checking uniqueness of RDO name ------------------------------------------
test_that("checking uniqueness of RDO name", {

  expect_error(RDO::RDO$new(name = "mtcars_half_top",
                            dependencies = list(mtcars_half_top)))

})

# _running code with missing dependencies -------------------------------------
test_that("running code with missing dependencies", {

  expect_true(mtcars_half_top$run()$is_validated())
  expect_true(mtcars_half_bottom$run()$is_validated())

  mtcars_whole_wrong_dependencies <-
    RDO::RDO$new(name = "mtcars_whole_wrong_dependencies",
                 dependencies = list(mtcars_half_top))

  mtcars_whole_wrong_dependencies$code <- expression({
    mtcars_whole <- rbind(mtcars_half_top,
                          mtcars_half_bottom)
  })

  expect_error(mtcars_whole_wrong_dependencies$run())

  mtcars_whole_wrong_dependencies$add_dependencies(mtcars_half_bottom)
  mtcars_whole_wrong_dependencies$add_dependencies(mtcars_half_top)

  expect_true(mtcars_whole_wrong_dependencies$run()$is_validated())

})

# _running code deep ----------------------------------------------------------
test_that("running code deep", {

  expect_equal(
    mtcars_whole$invalidate(deep = TRUE)$is_validated(),
    FALSE)

  expect_error(
    (mtcars_whole$run()))

  expect_equal(
    iris_mtcars$run(deep = TRUE)$cache,
    {
      set.seed(1234)
      sampled_rows <- c(rep(TRUE, 32), rep(FALSE, NROW(iris) - 32))
      cbind(
        iris[sample(sampled_rows, NROW(iris)), c("Species", "Petal.Width")],
        mtcars)
    })

  expect_output(
    iris_mtcars$run(deep = TRUE, verbose = TRUE),
    "^.*mtcars_whole.*iris_selected_columns' is validated.*$")

  expect_false(mtcars_half_top$invalidate()$is_validated())

  expect_false(iris_mtcars$is_validated(deep = TRUE))

  expect_true(iris_mtcars$run(deep = TRUE)$is_validated(deep = TRUE))

})

# _invalidating tree element --------------------------------------------------
test_that("invalidating tree element", {

  expect_equal(
    mtcars_whole$is_validated(),
    TRUE)

  expect_equal(
    mtcars_whole$is_validated(deep = TRUE),
    TRUE)

  expect_equal(
    iris_selected_rows$invalidate()$is_validated(),
    FALSE)

  expect_equal(
    iris_mtcars$is_validated(deep = TRUE),
    FALSE)

  expect_output(
    iris_mtcars$is_validated(deep = TRUE, verbose = TRUE),
    "^.*NOT VALIDATED.*$")

  expect_equal(
    iris_selected_rows$validate()$is_validated(),
    TRUE)

  expect_equal(
    mtcars_whole$is_validated(deep = TRUE),
    TRUE)

  iris_selected_rows$cache <- NULL

  expect_equal(
    iris_selected_rows$validate()$is_validated(),
    FALSE)

  expect_output(
    iris_mtcars$run(deep = TRUE, verbose = TRUE),
    "^.*have changed!.*$")

  expect_equal(
    iris_selected_rows$is_validated(),
    TRUE)

})


# _invalidating deeply --------------------------------------------------------
test_that("invalidating deeply", {

  expect_equal(
    data_mtcars$invalidate(deep = TRUE)$is_validated(),
    FALSE)

  expect_equal(
    data_mtcars$run(deep = TRUE)$is_validated(),
    TRUE)

  expect_equal(
    mtcars_whole$is_validated(deep = TRUE),
    TRUE)

})

# _testing RDO ----------------------------------------------------------------
test_that("testing RDO", {

  expect_true(
    iris_mtcars_test$run(deep = TRUE)$is_validated())

  data_mtcars$code <- expression({
    data_mtcars <- iris
  })

  expect_error(
    iris_mtcars_test$run(deep = TRUE))

  data_mtcars$code <- expression({
    data_mtcars <- mtcars
  })

  expect_true(
    iris_mtcars_test$run(deep = TRUE)$is_validated())

})

# _prunning cache -------------------------------------------------------------
test_that("prunning cache", {

  expect_equal(
    sum(unlist(iris_mtcars_test$get_cache_size(deep = TRUE))),
    48256)

  expect_equal(
    sum(unlist(
      iris_mtcars_test$prune_cache(deep = TRUE)$get_cache_size(deep = TRUE)
    )),
    6144)

  expect_equal(
    sum(unlist(
      iris_mtcars_test$run(deep = TRUE)$get_cache_size(deep = TRUE)
    )),
    48256)

})

# _prunning tree with locked RDOs ---------------------------------------------
test_that("prunning tree with locked RDOs", {

  expect_equal(
    sum(unlist(iris_mtcars_test$get_cache_size(deep = TRUE))),
    48256)

  expect_equal(
    data_mtcars$lock()$is_locked(),
    TRUE)

  expect_equal(
    data_iris$lock()$is_locked(),
    TRUE)

  expect_equal(
    sum(unlist(
      iris_mtcars_test$prune_cache(deep = TRUE)$get_cache_size(deep = TRUE)
    )),
    20608)

  expect_equal(
    iris_mtcars_test$lock()$is_locked(),
    TRUE)

  expect_error(
    iris_mtcars_test$cache <- NULL)

  expect_equal(
    iris_mtcars_test$unlock()$is_locked(),
    FALSE)

})

# _validating deep code -----------------------------------------------------
test_that("validating deep code", {

  expect_equal(
    iris_mtcars_test$is_validated(deep = TRUE),
    FALSE)

  expect_equal(
    iris_mtcars_test$invalidate()$is_validated(deep = FALSE),
    FALSE)

  expect_error(
    iris_mtcars_test$validate(deep = FALSE))

  expect_equal(
    iris_mtcars_test$validate(deep = TRUE)$is_validated(),
    TRUE)

})

# _deep locking --------------------------------------------------------------
test_that("deep locking", {

  expect_equal(
    iris_mtcars_test$is_locked(deep = TRUE),
    FALSE)

  expect_equal(
    iris_mtcars_test$unlock(deep = TRUE)$is_locked(deep = TRUE),
    FALSE)

  expect_equal(
    iris_mtcars_test$lock(deep = TRUE)$is_locked(deep = TRUE),
    TRUE)

})

# _removing intermediary RDOs -----------------------------------------------
rm(mtcars_half_top, mtcars_half_bottom, mtcars_whole,
   iris_selected_columns, iris_selected_rows)

test_that("removing intermediary RDOs", {

  expect_false(
    iris_mtcars_test$unlock(deep = TRUE)$prune_cache(deep = TRUE)$invalidate(
      deep = TRUE)$is_validated(deep = TRUE))

  expect_true(
    iris_mtcars_test$run(deep = TRUE)$is_validated(deep = TRUE))

  expect_equal(
    iris_mtcars_test$get_dependencies(deep = TRUE)$mtcars_whole$cache,
    mtcars)

})

# _shared dependencies -------------------------------------------------------
test_that("shared dependencies", {

  expect_true(data_mtcars$is_validated())

  expect_false(
    iris_mtcars_test$get_dependencies(
      deep = TRUE)$data_mtcars$invalidate()$is_validated())

  expect_false(data_mtcars$is_validated())

  expect_true(
    iris_mtcars_test$get_dependencies(
      deep = TRUE)$mtcars_half_top$get_dependencies(
        deep = TRUE)$data_mtcars$validate()$is_validated())

  expect_true(data_mtcars$is_validated())

})

# _deep cloning  with manual prunning -----------------------------------------
test_that("deep cloning with manual prunning", {

  iris_mtcars_test_clone <- iris_mtcars_test$clone(deep = TRUE)

  data_mtcars_cloned <-
    iris_mtcars_test_clone$get_dependencies(deep = TRUE)$data_mtcars

  data_mtcars_cloned$code <-
    expression(data_mtcars = {
      data_mtcars <- iris
    })

  iris_mtcars_test_clone$get_dependencies(
    deep = TRUE)$mtcars_half_top$add_dependencies(data_mtcars_cloned)

  iris_mtcars_test_clone$get_dependencies(
    deep = TRUE)$mtcars_half_bottom$add_dependencies(data_mtcars_cloned)

  expect_true(
    iris_mtcars_test_clone$invalidate(
      deep = TRUE)$get_dependencies(deep = TRUE)$mtcars_whole$run(
        deep = TRUE)$is_validated(deep = TRUE))

  expect_equal(
    iris_mtcars_test_clone$get_dependencies(
      deep = TRUE)$mtcars_half_top$get_dependencies(
        deep = TRUE)$data_mtcars$cache,
    iris)

  expect_equal(
    iris_mtcars_test_clone$get_dependencies(
      deep = TRUE)$mtcars_half_bottom$get_dependencies(
        deep = TRUE)$data_mtcars$cache,
    iris)

  expect_equal(
    iris_mtcars_test$get_dependencies(
      deep = TRUE)$mtcars_half_bottom$get_dependencies(
        deep = TRUE)$data_mtcars$cache,
    mtcars)

  expect_equal(
    iris_mtcars_test$get_dependencies(
      deep = TRUE)$mtcars_half_top$get_dependencies(
        deep = TRUE)$data_mtcars$cache,
    mtcars)

  expect_true(
    all(c("Species", "mpg") %in% names(
      iris_mtcars_test$invalidate(deep = TRUE)$get_dependencies(
        deep = TRUE)$iris_mtcars$run(deep = TRUE)$cache
    )))

  expect_false(
    all(c("Species", "mpg") %in% names(
      iris_mtcars_test_clone$invalidate(deep = TRUE)$get_dependencies(
        deep = TRUE)$iris_mtcars$run(deep = TRUE)$cache
    )))

})

# _deep cloning  with automatic prunning --------------------------------------
test_that("deep cloning with automatic prunning", {

  iris_mtcars_test_clone <-
    iris_mtcars_test$clone(deep = TRUE)$prune_dependencies()

  data_mtcars_cloned <-
    iris_mtcars_test_clone$get_dependencies(deep = TRUE)$data_mtcars

  data_mtcars_cloned$code <-
    expression(data_mtcars = {
      data_mtcars <- iris
    })

  expect_true(
    all(c("Species", "mpg") %in% names(
      iris_mtcars_test$invalidate(deep = TRUE)$get_dependencies(
        deep = TRUE)$iris_mtcars$run(deep = TRUE)$cache
    )))

  expect_false(
    all(c("Species", "mpg") %in% names(
      iris_mtcars_test_clone$invalidate(deep = TRUE)$get_dependencies(
        deep = TRUE)$iris_mtcars$run(deep = TRUE)$cache
    )))

})

# _prunning dependencies in the root ------------------------------------------
test_that("prunning dependencies in the root", {

  iris_mtcars_test_clone <- iris_mtcars_test$clone(deep = TRUE)

  iris_mtcars_test_clone$add_dependencies(
    dependencies = iris_mtcars_test$get_dependencies(TRUE)$mtcars_whole)

  expect_equal(
    names(iris_mtcars_test_clone$get_dependencies()),
    c("iris_mtcars" , "mtcars_whole"))

  register <- iris_mtcars_test_clone$get_dependency_register()

  expect_equal(
    register[register$dependency == "mtcars_whole",]$parent,
    c("iris_mtcars", "iris_mtcars_test"))

    # TODO: id ####
  iris_mtcars_test_clone$prune_dependencies()

})

# _converting RDOs ------------------------------------------------------------
capture.output(eval(expr = iris_mtcars_test$get_code(deep = TRUE)))

test_that("replacing RDOs", {

  expect_true(is.data.frame(iris_mtcars))

})
