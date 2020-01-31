
#' @export

RDO <-
  R6::R6Class(
    classname = "RDO",

    # PUBLIC ##################################################################
    public = list(

      initialize = function(name,
                            dependencies = list()
                            ) {

        private$name <- name

        private$set_status(status = "created")

        self$add_dependencies(dependencies = dependencies)

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



      add_dependencies = function(dependencies = list()) {

        if (!is.list(dependencies)) dependencies <- list(dependencies)

        purrr::walk(dependencies, function(rdo) {

          rdo_name <- rdo$get_name()
          private$dependencies[[rdo_name]] <- rdo

        })

        invisible(self)

      }, # end of add_dependencies



      get_dependencies = function(deep = FALSE) {

        has_dependencies <- self$has_dependencies()
        dependencies <- private$dependencies

        if (!deep) return(dependencies)

        if (!has_dependencies) return(NULL)

        nested_dependencies <-
          purrr::map(dependencies, function(rdo) {

            rdo$get_dependencies(deep = TRUE) %>% purrr::compact(.x = .)

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


      get_dependency_register = function() {

        has_dependencies <- self$has_dependencies()

        if (!has_dependencies) {

          dependency_register <-
            tibble::tibble(dependency = NA_character_,
                           parent = self$get_name())

          return(dependency_register)

        } # end of if

        dependencies <- self$get_dependencies()

        purrr::map_df(dependencies, function(rdo) {

          dplyr::bind_rows(
            tibble::tibble(dependency = rdo$get_name(),
                           parent = self$get_name()),
            rdo$get_dependency_register())

        }) # end of map

      }, # end of get_dependency_register


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

            paste(code_line, "\n")

          }) # end of map_chr

        r_code_text <- paste(r_code_text, collapse = '\n')

        r_code_text %>% cat()

        return(invisible(r_code_text))

      }, # end of print_r_code



      run_r_code = function(deep = FALSE,
                            cache = TRUE,
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

              rdo$cache

              }) %>% setNames(names(dependecies)))

        } # end of if


        if (cache) {

          self$cache <- temp_data

          private$set_status(status = "validated")

        } # end of if

        if (verbose) cat("...evaluation of", rdo_name, "completed.\n")

        if (cache == FALSE) return(temp_data)

        invisible(self)

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

            rdo$invalidate(deep = FALSE, verbose = verbose)

          })

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("Invalidating RDO: '", self_name, "' ... ", sep = "")

        private$set_status(status = "invalidated")

        if (verbose) cat("done.\n")

        invisible(self)

      }, # end of if



      validate = function(deep = FALSE,
                          verbose = TRUE
                          ) {

        has_dependencies <- self$has_dependencies()

        if (deep | !has_dependencies) {

          eval_envir <- new.env()

        }

        if (!deep & has_dependencies) {

          dependecies <- self$get_dependencies()

          eval_envir <-
            purrr::map(dependecies, function(rdo) {

              rdo_name <- rdo$get_name()

              is_validated <- rdo$is_validated(verbose = verbose)

              if (!is_validated)
                stop("Dependency object '", rdo_name, "' is not validated. ")

              rdo$cache

              }) %>% setNames(names(dependecies))

        }

        self_name <- self$get_name()

        if (verbose) cat("Validating RDO: '", self_name, "' ... ", sep = "")

        self_deep_r_code <- self$get_r_code(deep = deep)

        eval_data <- eval(expr = self_deep_r_code, envir = eval_envir)

        is_validated <- identical(self$cache,
                                  eval_data)

        if (is_validated)  private$set_status(status = "validated")
        if (!is_validated) private$set_status(status = "invalidated")

        if (verbose) {

          if (is_validated) cat("done.\n") else
            cat("NOT validated!\n")

        } # end if

        invisible(self)

      }, # end of validate

      lock = function(deep = FALSE,
                      verbose = TRUE
                      ) {

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          purrr::walk(dependencies, function(rdo) {

            rdo$lock(deep = FALSE, verbose = verbose)

          })

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("Locking RDO: '", self_name, "' ... ", sep = "")

        private$status$is_locked <- TRUE

        if (verbose) cat("done.\n")

        invisible(self)

      },


      unlock = function(deep = FALSE,
                        verbose = TRUE
                        ) {

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          purrr::walk(dependencies, function(rdo) {

            rdo$unlock(deep = FALSE, verbose = verbose)

          })

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("Unlocking RDO: '", self_name, "' ... ", sep = "")

        private$status$is_locked <- FALSE

        if (verbose) cat("done.\n")

        invisible(self)

      },


      is_locked = function(deep = FALSE,
                           verbose = TRUE
                           ) {

        has_dependencies <- self$has_dependencies()

        are_locked <- c()

        if (has_dependencies & deep) {

          dependencies <- self$get_dependencies(deep = deep)

          are_locked <-
            purrr::map_lgl(dependencies, function(rdo) {

              rdo$is_locked(deep = FALSE, verbose = verbose)

            }) %>% all()

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("RDO: '", self_name, "' is ", sep = "")

        is_locked <- private$status$is_locked

        if (verbose) {

          if (is_locked) cat("LOCKED!\n") else
            cat("unlocked.\n")

        }

        is_locked <- all(are_locked, is_locked)

        is_locked

      },



      get_cache_size = function(deep = FALSE,
                                verbose = TRUE
                                ) {

        if (verbose) cat("Cache size: ")

        cache_size <- list()

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          cache_size <-
            purrr::map(dependencies, function(rdo) {

            object.size(rdo$cache)

          })

        } # end of if

        self_cache_size <- setNames(object.size(self$cache), self$get_name())

        cache_size <- c(cache_size, self_cache_size)

        cache_size <-
          purrr::map(cache_size, ~ `class<-`(.x, value = "object_size"))

        cache_size_total <-
          cache_size %>%
          unlist() %>%
          sum %>%
          `class<-`(., value = "object_size")

        if (verbose)
          cat(format(x = cache_size_total, units = "Mb", digits = 4L), "\n")

        invisible(cache_size)

      },



      prune_cache = function(deep = FALSE,
                             verbose = TRUE
                             ) {

        if (!self$has_dependencies())
          stop("This RDO doesn't have any dependencies!")

        dependencies <- self$get_dependencies(deep = deep)

        purrr::walk(dependencies, function(rdo) {

          rdo_name <- rdo$get_name()

          is_locked <- rdo$is_locked(verbose = FALSE)

          if (!is_locked) {

            rdo$cache <- NULL

          }

          if (verbose) cat("Cache in RDO: '", rdo_name, "' was ",
                           "cleared.\n", sep = "")

        }) # end of walk

        invisible(self)

      }, # end of prune

      prune_dependencies = function(verbose = TRUE) {

        dependency_register <- self$get_dependency_register() %>% na.omit()

        duplicated_clones <-
          dependency_register$dependency[
            dependency_register$dependency %>% duplicated()] %>%
          unique()

        duplicated_clones <-
          dependency_register[
            dependency_register$dependency %in% duplicated_clones, ]

        if (NROW(duplicated_clones) > 0) {

          dependencies <- self$get_dependencies(deep = TRUE)

          duplicated_clones %>%
            purrr::pwalk(function(dependency, parent) {

              if (verbose) {

                cat("Prunning dependency:",
                    dependencies[[dependency]]$get_name(),
                    "in parent:",
                    dependencies[[parent]]$get_name(),
                    "... ")

              }

              dependencies[[parent]]$add_dependencies(dependencies[[dependency]])

              if (verbose) cat("done.\n")

            }) # end of pwalk

        } # end of if

        invisible(self)

      } # end of prune_dependencies

    ), # end of public



    # ACTIVE BINDINGS #########################################################
    active = list(


      r_code = function(value) {

        if (missing(value)) {

          self$print_r_code(deep = TRUE)

        } else {

          if (private$status$is_locked)
            stop("This RDO is locked! Cannot overwrite the r_code.")

          private$r_code_expression <- value

          private$set_status(status = "invalidated")

        } # end of if
      }, # end of r_code



      cache = function(value) {

        if (missing(value)) {

          return(private$data_cache)

        } else {

          if (private$status$is_locked)
            stop("This RDO is locked! Cannot overwrite the RDO's cache.")

          private$data_cache <- value

          private$set_status(status = "invalidated")

        } # end of if
      } # end of cache




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
        is_validated = FALSE,
        is_locked = FALSE
      ),

      data_cache = NULL,

      r_code_expression = NULL,

      set_status = function(status = "changed") {

        timestamp <- Sys.time()
        attr(timestamp, "tzone") <- "UTC"

        private$status$changed <- timestamp


        if (status == "created") {

          private$status$created <- timestamp

        }

        if (status == "validated") {

          private$status$is_validated <- TRUE
          private$status$validated <- timestamp

        }

        if (status == "invalidated") {

          private$status$is_validated <- FALSE

        }


      }, # end of set_status


      deep_clone = function(name, value) {

        if (name == "dependencies") {

          rdo_dependencies_cloned <-
            purrr::map(value, function(rdo) {

              value <- rdo$clone(deep = TRUE)

            }) %>% setNames(names(value))

          rdo_dependencies_cloned

        } else {

          value

        }
      } # end of deep_clone
    ) # end of private
) # end of RDO R6Class

