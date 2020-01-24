# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
# usethis::use_test()
#' @export

RDO <-
  R6::R6Class(
    classname = "RDO",

    # PUBLIC ##################################################################
    public = list(

      # TODO: print default rdo

      initialize = function(name,
                            dependencies = list()
                            ) {

        # TODO: list of dendencies
        # private$code_cached <- code
        private$name <- name

        private$set_status(status = "created")

        dependency_names <-
          purrr::map_chr(dependencies, function(dependency) {

            dependency$get_name()

          })

        names(dependencies) <- dependency_names

        private$dependencies <- dependencies


      }, # end of initialize



      get_status = function() {

        private$status

      }, # end of get_status



      get_name = function() {

        private$name

      }, # end of get_name



      has_dependencies = function() {

        if (NROW(private$dependencies) > 0) {

          TRUE

        } else {

          FALSE

        } # end of if
      }, # end of has_dependencies

      get_dependencies = function(deep = FALSE) {

        has_dependencies <- self$has_dependencies()
        dependencies <- private$dependencies

        if (!deep) return(dependencies)

        if (!has_dependencies) return(NULL)

        nested_dependencies <-
          purrr::map(dependencies, function(rdo) {

            rdo$get_dependencies(deep = TRUE) %>%
              purrr::compact()

          }) # end of map

        dependencies <-
          c(nested_dependencies,
            dependencies) %>% unlist()

        dependency_names <-
         dependencies %>%
         purrr::map_chr(~.x$get_name())

        dependencies <-
          dependencies %>%
          setNames(dependency_names)

        duplicated_dependecies <-
          names(dependencies) %>%
          duplicated()

        dependencies[!duplicated_dependecies]

      }, # end of get_dependencies



      get_r_code = function(deep = FALSE
                            ) {

        has_dependencies <- self$has_dependencies()

        if (!has_dependencies | !deep) {

          r_code <- private$r_code_expression

          if (!is.null(r_code)) names(r_code) <- self$get_name()

          return(r_code)

        } # end if

        rdos <- self$get_dependencies(deep = TRUE)

        r_code <-
          purrr::map(rdos, function(rdo) {

          rdo$get_r_code(deep = FALSE)

          }) %>%
          unname() %>%
          do.call(what = c, args = .)

        r_code_self <- self$get_r_code(deep = FALSE)
        names(r_code_self) <- self$get_name()

        r_code <- c(r_code, r_code_self)

        return(r_code)

      }, # end of get_r_code


      print_r_code = function(deep = FALSE) {

        r_code_text <-
          self$get_r_code(deep = deep) %>%
          as.list() %>%
          purrr::map(~ .x %>% as.character()) %>%
          purrr::map_chr(function(code_line) {

            if (code_line[1] == "{") {

              code_line <- code_line[-1]

            }

            code_line <- paste(code_line, collapse = "\n")

            return(code_line)

          })

        r_code_text %>%
          # paste(collapse = "\n") %>%
          cat(sep = "\n")
        # r_code_text %>% cat(fill = TRUE)

        return(invisible(NULL))

      }, # end of print_r_code



      run_r_code = function(deep = FALSE,
                            cache = TRUE,
                            # force = FALSE,
                            verbose = TRUE
                            ) {

        rdo_name <- self$get_name()

        if (verbose) cat("Evaluating RDO:", rdo_name, "... ")



        has_dependencies <- self$has_dependencies()

        if (has_dependencies) {

          if (verbose) cat("has dependencies ...\n")

        } else {

          temp_data <- eval(expr = self$get_r_code(deep = FALSE))

          if (verbose) cat("done!\n")

        } # end of if



        if (has_dependencies & deep) {

          # TODO: self$invalidate(deep = TRUE)

          dependencies <- self$get_dependencies(deep = TRUE)

          purrr::walk(dependencies, function(rdo) {

            rdo_name <- rdo$get_name()

            is_validated <- rdo$is_validated(verbose = verbose)

            dependecies <- rdo$get_dependencies()

            dependencies_changed <-
              purrr::map(dependecies, function(rdo) {

                rdo$get_status()$changed

              }) %>% unlist()

            self_validated <- self$get_status()$validated

            dependencies_changed <-
              any(dependencies_changed >= self_validated)

            if (verbose & dependencies_changed)
              cat("Dependencies of RDO:", rdo_name, "have changed!\n")

            if (!is_validated | dependencies_changed) {

              rdo$run_r_code(deep = FALSE,
                             cache = cache,
                             verbose = verbose)

            } # end of if
          }) # end of walk
        } # end of if

        if (has_dependencies) {

          dependecies <- self$get_dependencies()

          temp_data <- eval(
            expr = self$get_r_code(deep = FALSE),
            envir = purrr::map(dependecies, function(rdo) {

              rdo_name <- rdo$get_name()

              is_validated <- rdo$is_validated(verbose = verbose)

              if (!is_validated)
                stop("Dependency object '", rdo_name, "' is not validated. ")

              rdo$data

              }) %>% setNames(names(dependecies)))

        } # end of if



        if (cache) {

          self$data <- temp_data

          private$set_status(status = "validated")

          # private$status$is_validated <- TRUE

        } # end of if

        if (verbose) cat("...evaluation of", rdo_name, "completed.\n")


        invisible(temp_data)

      }, # end of if






      is_validated = function(deep = FALSE,
                              verbose = TRUE
                              ) {

        self_name <- self$get_name()


        has_dependencies <- self$has_dependencies()

        are_validated <- c()

        if (deep & has_dependencies) {

          dependecies <- self$get_dependencies(deep = deep)

          are_validated <-
            purrr::map_lgl(dependecies, function(rdo) {

              rdo_name <- rdo$get_name()

              is_validated <- rdo$is_validated(deep = FALSE,
                                               verbose = verbose)

              is_validated

            }) %>% all()

        } # end of if

        is_validated <- private$status$is_validated

        if (verbose) {

          cat("RDO: '", self_name, "' is ", sep = "")

          if (is_validated) cat("validated.\n") else cat("NOT VALIDATED!\n")

        } # end of if

        is_validated <- all(are_validated, is_validated)
        return(is_validated)

      }, # end of is_validated



      invalidate = function(deep = FALSE,
                            verbose = TRUE
                            ) {

        has_dependencies <- self$has_dependencies()

        if (deep & has_dependencies) {

          dependecies <- self$get_dependencies(deep = deep)

          purrr::walk(dependecies, function(rdo) {

            rdo_name <- rdo$get_name()

            rdo$invalidate(deep = FALSE, verbose = TRUE)

          })

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("Invalidating RDO: '", self_name, "' ... ", sep = "")

        private$set_status(status = "invalidated")

        # private$status$is_validated <- FALSE

        if (verbose) cat("done.\n")

        invisible(
          setNames(c(private$status$is_validated), self_name))


        # TODO: deep == TRUE

      }, # end of if



      validate = function(deep = FALSE,
                          verbose = TRUE
                          ) {


        # TODO: deep == TRUE
        # TODO: step_by_step == FALSE
        dependencies_validated <- c()

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = TRUE)

          dependencies_validated <-
            purrr::map_lgl(dependencies, function(rdo) {

              rdo$validate(deep = FALSE, verbose = verbose)

            })

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("Validating RDO: '", self_name, "' ... ", sep = "")

        self_deep_r_code <- self$get_r_code(deep = TRUE)

        eval_data <- eval(expr = self_deep_r_code, envir = new.env())

        is_validated <- identical(self$data,
                                  eval_data)

        if (is_validated)  private$set_status(status = "validated")
        if (!is_validated) private$set_status(status = "invalidated")
        # private$status$is_validated <- is_validated

        if (verbose) {

          if (is_validated) cat("done.\n") else
            cat("NOT validated!\n")

        } # end if

        # invisible(
        rdos_validated <- c(dependencies_validated,
                            setNames(c(is_validated), self_name))

        rdos_validated

        # rdos_validated[self_name] <- all(rdos_validated)
        # setNames(all(rdos_validated), self_name)
          # )

      } # end of validate

    ), # end of public

    # ACTIVE BINDINGS #########################################################
    active = list(


      r_code = function(value) {

        if (missing(value)) {

          self$print_r_code(deep = TRUE)

          self$get_r_code(deep = TRUE)

        } else {

          private$r_code_expression <- value

          # private$status$is_validated <- FALSE

          private$set_status(status = "invalidated")

        } # end of if
      }, # end of r_code



      data = function(value) {

        if (missing(value)) {

          return(private$data_cache)

        } else {

          private$data_cache <- value

          # private$status$is_validated <- FALSE

          private$set_status(status = "invalidated")

        } # end of if
      } # end of data

    ), # end of active

    # PRIVATE #################################################################
    private = list(

      name = NULL,

      dependencies  = list(),

      # _status ---------------------------------------------------------------
      status = list(
        created = NULL,
        changed = NULL,
        validated = NULL,
        invalidated = NULL,
        is_validated = FALSE
      ),

      # TODO: validated


      data_cache = NULL,

      r_code_expression = NULL,

      set_status = function(status = "changed"
                            ) {

        timestamp <- Sys.time()
        attr(timestamp, "tzone") <- "UTC"

        if (status == "created") {

          private$status$created <- timestamp

        }

        if (status == "validated") {

          private$status$is_validated <- TRUE
          private$status$validated <- timestamp

        }


        if (status == "invalidated") {

          private$status$is_validated <- FALSE
          private$status$invalidated <- timestamp

        }

        # if (status == "changed") {}
        private$status$changed <- timestamp

      } # end of set_status

    ) # end of private

) # end of RDO R6Class

