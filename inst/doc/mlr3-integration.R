## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(datatable.print.topn = 3L)

## ----setup, message = FALSE---------------------------------------------------
library(gmeans)
library(mlr3cluster)
library(mlr3misc)
library(mlr3viz)
library(paradox)

## -----------------------------------------------------------------------------
LearnerClustGMeans <- R6::R6Class("LearnerClustGMeans",
  inherit = LearnerClust,
  public = list(
    initialize = function() {
      param_set <- ps(
        k_init = p_int(2L, default = 2L, tags = "train"),
        k_max = p_int(2L, default = 10L, tags = "train"),
        level = p_dbl(0, 1, default = 0.05, tags = "train"),
        iter.max = p_int(1L, default = 10L, tags = "train"),
        algorithm = p_fct(
          levels = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
          default = "Hartigan-Wong",
          tags = "train"
        ),
        trace = p_lgl(default = FALSE, tags = "train")
      )

      super$initialize(
        id = "clust.gmeans",
        feature_types = c("logical", "integer", "numeric"),
        predict_types = "partition",
        param_set = param_set,
        properties = c("partitional", "exclusive", "complete"),
        packages = "gmeans",
        man = "mlr3cluster::mlr_learners_clust.gmeans",
        label = "G-means"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      m <- invoke(gmeans::gmeans, x = task$data(), .args = pv)
      if (self$save_assignments) {
        self$assignments <- m$cluster
      }
      m
    },
    .predict = function(task) {
      partition <- invoke(predict, self$model,
        newdata = task$data(), type = "class_ids"
      )
      PredictionClust$new(task = task, partition = partition)
    }
  )
)

mlr_learners$add("clust.gmeans", LearnerClustGMeans)

## -----------------------------------------------------------------------------
task <- tsk("usarrests")
learner <- lrn("clust.gmeans")
learner$train(task)
prediction <- learner$predict(task = task)
prediction

## ----message = FALSE, warning = FALSE, dpi = 300------------------------------
autoplot(prediction, task)

## -----------------------------------------------------------------------------
measures <- msrs(c("clust.wss", "clust.silhouette"))
prediction$score(measures, task)

## ----dpi = 300----------------------------------------------------------------
autoplot(prediction, task, type = "pca")
autoplot(prediction, task, type = "sil")

## -----------------------------------------------------------------------------
learners <- list(
  lrn("clust.featureless"),
  lrn("clust.kmeans"),
  lrn("clust.gmeans")
)
measures <- list(msr("clust.wss"), msr("clust.silhouette"))
bmr <- benchmark(benchmark_grid(tsk("ruspini"), learners, rsmp("insample")))
bmr$aggregate(measures)[, c(4, 7, 8)]

