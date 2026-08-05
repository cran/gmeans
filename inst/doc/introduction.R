## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(datatable.print.topn = 3L)

## ----setup, message = FALSE---------------------------------------------------
library(data.table)
library(ggplot2)
library(gmeans)

## ----dpi = 300----------------------------------------------------------------
set.seed(27)

centers <- data.table(
  cluster = factor(1:3),
  num_points = c(100, 150, 50),
  x1 = c(5, 0, -3),
  x2 = c(-1, 1, -2)
)
points <- centers[, .(
  x1 = rnorm(num_points, mean = x1),
  x2 = rnorm(num_points, mean = x2)
), by = cluster]

ggplot(points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3)

## -----------------------------------------------------------------------------
points <- points[, cluster := NULL]
kclust <- kmeans(points, centers = 3)
kclust

## -----------------------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y

tidy <- function(x, col.names = colnames(x$centers)) {
  col.names <- col.names %||% paste0("x", seq_len(ncol(x$centers)))
  dt <- as.data.table(x$centers)
  setnames(dt, col.names)
  dt[, let(
    size = x$size,
    withinss = x$withinss,
    cluster = factor(seq_len(.N))
  )][]
}

augment <- function(x, data) {
  if (inherits(data, "matrix") && is.null(colnames(data))) {
    colnames(data) <- paste0("X", seq_len(ncol(data)))
  }
  dt <- as.data.table(data)
  dt[, .cluster := as.factor(x$cluster)][]
}

glance <- function(x) {
  as.data.table(x[c("totss", "tot.withinss", "betweenss", "iter")])
}

## -----------------------------------------------------------------------------
augment(kclust, points)

## -----------------------------------------------------------------------------
tidy(kclust)

## -----------------------------------------------------------------------------
glance(kclust)

## ----dpi = 300----------------------------------------------------------------
kclusts <- data.table(k = 1:9)
kclusts[, kclust := lapply(k, \(x) kmeans(points, x))]
kclusts[, let(
  tidied = lapply(kclust, tidy),
  glanced = lapply(kclust, glance),
  augmented = lapply(kclust, augment, points)
)]

clusters <- kclusts[, .(k, rbindlist(tidied))]
assignments <- kclusts[, .(k, rbindlist(augmented))]
clusterings <- kclusts[, .(k, rbindlist(glanced))]

p1 <- ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~k) +
  labs(title = "k-means Clustering Results with Different Values of k")
p1

## -----------------------------------------------------------------------------
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x") +
  labs(title = "k-means Clustering with Centers")
p2

## ----dpi = 300----------------------------------------------------------------
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Total Within-Cluster Sum of Squares vs. Number of Clusters (k)",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  )

## -----------------------------------------------------------------------------
set.seed(123)

gmeans(points)

## -----------------------------------------------------------------------------
set.seed(1234)

x <- as.matrix(iris[, -5])
gclust <- gmeans(x)

## ----dpi = 300----------------------------------------------------------------
augment(gclust, x) |>
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = .cluster))

## -----------------------------------------------------------------------------
tidy(gclust)

## -----------------------------------------------------------------------------
glance(gclust)

