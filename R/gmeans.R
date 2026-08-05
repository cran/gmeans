#' G-means Clustering
#'
#' @description
#' Perform G-means clustering on a data matrix.
#'
#' @details
#' The G-means clustering algorithm is an extension of the traditional k-means
#' algorithm that automatically determines the number of clusters by iteratively
#' testing the Gaussianity of data within clusters. The process begins with a specified
#' initial number of clusters (`k_init`) and iteratively increases the number of
#' clusters until it reaches the specified maximum (`k_max`) or the data within
#' clusters is determined to be Gaussian at the specified significance level (`level`).
#'
#' The algorithm is outlined as follows:
#'
#' 1. Let \eqn{C} be the initial set of centers (usually
#'    \eqn{C \leftarrow \{\bar{x}\}}).
#' 2. Perform k-means clustering on the dataset \eqn{X} using the current set of
#'    centers \eqn{C}, i.e., \eqn{C \leftarrow \text{kmeans}(C, X)}.
#' 3. For each center \eqn{c_j}, identify the set of data points
#'    \eqn{\{x_i \mid \text{class}(x_i) = j\}} that are assigned to \eqn{c_j}.
#' 4. Use the Anderson-Darling test to check if the set of data points
#'    \eqn{\{x_i \mid \text{class}(x_i) = j\}} follows a Gaussian distribution
#'    at the confidence level \eqn{\alpha}.
#' 5. If the data points appear Gaussian, keep \eqn{c_j}.
#'    Otherwise, replace \eqn{c_j} with two new centers.
#' 6. Repeat from step 2 until no more centers are added.
#'
#' @param x (`matrix()`)\cr
#'   Numeric matrix of data, or an object that can be coerced to such a matrix
#'   (such as a numeric vector or a data frame with all numeric columns).
#' @param k_init (`integer(1)`)\cr
#'   Initial amount of centers. Default is `2L`.
#' @param k_max (`integer(1)`)\cr
#'   Maximum amount of centers. Default is `10L`.
#' @param level (`numeric(1)`)\cr
#'   Significance level for the Anderson-Darling test.
#'   Default is `0.05`. See [ad.test()] for more information.
#' @param ... (`any`)\cr
#'   Additional arguments passed to [stats::kmeans()].
#' @references
#' `r format_bib("hamerly2003learning")`
#' @returns An object of class `c("gmeans", "kmeans")`. See [stats::kmeans()] for details.
#' @export
#' @examples
#' set.seed(123)
#' x <- rbind(
#'   matrix(rnorm(100, sd = 0.3), ncol = 2),
#'   matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
#' )
#' colnames(x) <- c("x", "y")
#' cl <- gmeans(x)
gmeans <- function(x, k_init = 2L, k_max = 10L, level = 0.05, ...) {
  if (inherits(x, "data.frame")) {
    x <- as.matrix(x)
  }
  stopifnot(
    is.matrix(x),
    is_count(k_init),
    is_count(k_max),
    is_number(level),
    level > 0,
    level < 1
  )

  init_centers <- kmeans_plusplus(x, k_init)
  if (length(init_centers) == 1L) {
    init_centers <- 1L
  }
  km <- stats::kmeans(x, init_centers, ...)

  repeat {
    new_centers <- suggest_centers(x, km, k_max, level, ...)
    # no more centers added
    if (nrow(km$centers) == nrow(new_centers)) {
      break
    }
    km <- stats::kmeans(x, new_centers, ...)
  }
  class(km) <- c("gmeans", class(km))
  km
}

suggest_centers <- function(data, km, k_max, level, ...) {
  centers <- NULL
  k <- nrow(km$centers)
  for (i in seq_len(k)) {
    new_centers <- NULL
    if (k < k_max) {
      cluster <- which(km$cluster == i)
      new_centers <- split_and_search(data, cluster, level, ...)
    }
    if (is.null(new_centers)) {
      centers <- rbind(centers, km$centers[i, ])
    } else {
      centers <- rbind(centers, new_centers)
      k <- k + 1L
    }
  }
  centers
}

split_and_search <- function(data, cluster, level, ...) {
  if (length(cluster) < 8L) {
    return()
  }
  points <- data[cluster, , drop = FALSE]
  # all rows identical, so there are fewer distinct points than centers to find
  if (!any(points != rep(points[1L, ], each = nrow(points)))) {
    return()
  }
  km <- stats::kmeans(points, 2L, ...)
  new_centers <- km$centers
  if (nrow(new_centers) > 1L && !is_null_hypothesis(points, new_centers, level)) {
    new_centers
  } else {
    NULL
  }
}

#' kmeans++ initialization
#'
#' @description
#' Algorithm for choosing the initial centers. k-means++ algorithm guarantees an
#' approximation ratio \eqn{O(\log k)}. Clustering results of k-means are dependent
#' on the choice of initial centers. This method is used to find out optimal initial
#' centers.
#'
#' @details
#' The kmeans++ can be divided into the following steps:
#' 1. The first center is chosen randomly from the input data with a uniform
#'    distribution.
#' 2. For each point \eqn{x_i}, compute its distance \eqn{D(x_i)} to the
#'    nearest center already chosen.
#' 3. Calculate the probability \eqn{p_i} for each point \eqn{x_i} to be
#'    selected as the next center: \deqn{
#'      p_{i} = \frac{D(x_{i})^2}{\sum_{j=0}^{n} D(x_{j})^2}
#'    }
#'    Points farther from existing centers have a higher probability of being
#'    chosen.
#' 4. Select the next center based on the probability distribution calculated
#'    in step 3.
#' 5. Repeat steps 2-4 until the required number of centers, \eqn{k}, is
#'    initialized.
#'
#' @references
#' `r format_bib("arthur2007kmeanspp")`
#' @noRd
kmeans_plusplus <- function(x, k) {
  n <- nrow(x)
  centroids <- matrix(NA_real_, nrow = k, ncol = ncol(x))
  centroids[1L, ] <- x[sample.int(n, 1L), ]

  if (k > 1L) {
    tx <- t(x)
    # distance to the nearest chosen center, updated as centers are added
    dists <- colSums((tx - centroids[1L, ])^2)
    for (i in seq_len(k)[-1L]) {
      prob <- dists / sum(dists)
      centroids[i, ] <- x[sample.int(n, 1, prob = prob), ]
      if (i < k) {
        dists <- pmin(dists, colSums((tx - centroids[i, ])^2))
      }
    }
  }
  centroids
}

#' Null Hypothesis Test
#'
#' @description
#' Simplifies the test for Gaussian fit by projecting the data to one dimension using
#' the following formula, as described in `r cite_bib("hamerly2003learning")`:
#' \deqn{
#'   x_{i}^{*}=\frac{\left \langle x_{i}, v \right \rangle}{\left \| v \right \|^{2}}
#' }
#' @noRd
is_null_hypothesis <- function(data, centers, level = 0.05) {
  v <- centers[1L, ] - centers[2L, ]
  points <- as.vector(data %*% v / sum(v^2))
  ad.test(points)$p.value > level
}

#' Predict Method for G-means Clustering
#'
#' @description
#' Predicted values based on the G-means clustering model.
#'
#' @details
#' The `predict` method for G-means clustering assigns new data points to the nearest
#' cluster center identified by the G-means algorithm. The method uses the specified
#' distance metric to calculate the distance between each new data point and all
#' cluster centers, and then assigns each point to the cluster with the closest center.
#'
#' The `method` argument specifies the distance metric to use. The following options:
#' - `"euclidean"`: The Euclidean distance is the default metric used in the k-means
#'   and is defined as \deqn{
#'     d(x, y) = \sqrt{\sum_{i=1}^{n} (x_i - y_i)^2}
#'   }
#' - `"manhattan"`: The Manhattan distance is defined as \deqn{
#'     d(x, y) = \sum_{i=1}^{n} |x_i - y_i|
#'   }
#' - `"minkowski"`: The Minkowski distance is defined as \deqn{
#'     d(x, y) = \left( \sum_{i=1}^{n} |x_i - y_i|^p \right)^{1/p},
#'   }
#'   where \eqn{p} is a parameter that defines the distance type (e.g., \eqn{p=2}
#'   for Euclidean, \eqn{p=1} for Manhattan).
#'
#' @param object (`gmeans()`)\cr
#'   An object of class `"gmeans"`.
#' @param newdata (`matrix()`)\cr
#'   New data to predict on.
#' @param method (`character(1)`)\cr
#'   Distance metric to use.
#'   Either `"euclidean"`, `"manhattan"`, or `"minkowski"`. Default is `"euclidean"`.
#' @param p (`numeric(1)`)\cr
#'   Power of the Minkowski distance. Default is `2`.
#' @param ... (`any`)\cr
#'   Additional arguments.
#' @returns An `integer()` vector with one cluster index per row of `newdata`.
#' @seealso [clue::cl_predict()] to predict on a plain [stats::kmeans()] object.
#' @source Adapted from \CRANpkg{clue}
#' @export
#' @examples
#' set.seed(123)
#' x <- as.matrix(iris[, -5])
#' cl <- gmeans(x)
#'
#' newdata <- x[1:10, ]
#' predict(cl, newdata)
predict.gmeans <- function(
  object,
  newdata,
  method = c("euclidean", "manhattan", "minkowski"),
  p = 2,
  ...
) {
  d <- rxdist(object, newdata, method, p)
  max.col(-d, ties.method = "first")
}

#' Compute Within-Cluster Sum of Squares
#'
#' @details
#' WSS is defined as \deqn{
#'   \sum_{i=1}^{n} \left\|x_{i} - \mu_{j(i)}\right\|^2,
#' }
#' where \eqn{x_{i}} is a data point and \eqn{\mu_{j(i)}} is the centroid of the cluster
#' to which \eqn{x_{i}} is assigned. When new data is provided, the function predicts
#' the nearest cluster for each new observation and computes the WSS for these points
#' based on their predicted clusters.
#'
#' @param object (`any`)\cr
#'   Class inheriting from `"kmeans"`.
#' @param newdata (`matrix()`)\cr
#'   New data to predict on.
#' @returns A `numeric()` vector with one within-cluster sum of squares per cluster,
#'   in the order of the rows of `object$centers`. Clusters with no assigned points
#'   contribute `0`.
#' @export
#' @examples
#' km <- kmeans(mtcars, 5)
#' compute_wss(km)
#' # or with new data
#' compute_wss(km, mtcars)
compute_wss <- function(object, newdata = NULL) {
  if (!inherits(object, "kmeans")) {
    stop("object must be of class 'kmeans'", call. = FALSE)
  }
  if (is.null(newdata)) {
    wss <- object$withinss
  } else {
    d <- rxdist(object, newdata)
    pred <- factor(apply(d, 1L, which.min), levels = seq_len(nrow(object$centers)))
    dist <- apply(d, 1L, min)
    wss <- as.numeric(tapply(dist, pred, sum, default = 0))
  }
  wss
}

rxdist <- function(
  object,
  newdata,
  method = c("euclidean", "manhattan", "minkowski"),
  p = 2
) {
  stopifnot(is_number(p))
  if (inherits(newdata, "data.frame")) {
    newdata <- as.matrix(newdata)
  }
  method <- match.arg(method)
  centers <- object$centers
  if (!all(colnames(centers) %in% colnames(newdata))) {
    stop("`newdata` must have the same columns as the centers", call. = FALSE)
  }

  data_nms <- colnames(newdata)
  center_nms <- colnames(centers)
  if (!is.null(data_nms) && !is.null(center_nms) && !identical(data_nms, center_nms)) {
    newdata <- newdata[, center_nms, drop = FALSE]
  }
  distance <- switch(
    method,
    euclidean = \(data, x) rowSums(sweep(data, 2L, x)^2),
    manhattan = \(data, x) rowSums(abs(sweep(data, 2L, x))),
    minkowski = \(data, x) (rowSums(abs(sweep(data, 2L, x))^p))^(1 / p)
  )
  matrix(
    apply(centers, 1L, \(x) distance(newdata, x)),
    nrow = nrow(newdata),
    ncol = nrow(centers)
  )
}

#' Anderson-Darling Normality Test
#'
#' @description
#' Perform the Anderson-Darling normality test.
#'
#' @details
#' The Anderson-Darling test is an EDF omnibus test for the composite hypothesis of
#' normality. The test statistic is \deqn{
#'   A^2 = -n -\frac{1}{n} \sum_{i=1}^{n} (2i - 1) [\ln(z_{i}) + \ln(1 - z_{n + 1 - i})]
#' }
#' where \eqn{z_{i} = \Phi(\frac{x_{i} - \bar{x}}{s})}. Here,
#' \eqn{\Phi} is the cumulative distribution function of the standard normal
#' distribution, and \eqn{\bar{x}} and \eqn{s} are mean and standard deviation of
#' the data values. The p-value is computed from the modified statistic
#' \eqn{A^2_*=A^2 (1.0 + 0.75/n + 2.25/n^{2})} according to Table 4.9 in
#' Stephens (1986).
#'
#' @param x (`numeric()`)\cr
#'   Vector of data values. Missing values are allowed, but the
#'   number of non-missing values must be greater than 7.
#'
#' @returns
#' A list inheriting from classes `"htest"` containing the following components:
#'   * statistic: the value of the statistic.
#'   * p.value: the p-value of the test.
#'   * method: the character string `"Anderson-Darling normality test"`.
#'   * data.name: a character string giving the name(s) of the data.
#' @seealso [stats::shapiro.test()] for performing the Shapiro-Wilk test for normality.
#'   [nortest::cvm.test()], [nortest::lillie.test()], [nortest::pearson.test()],
#'   [nortest::sf.test()] for performing further tests for normality.
#'   [stats::qqnorm()] for producing a normal quantile-quantile plot.
#' @source Adapted from [nortest::ad.test()]
#' @references
#' `r format_bib("d2017goodness", "thode2002testing")`
#' @export
#' @examples
#' set.seed(123)
#' ad.test(rnorm(100, mean = 5, sd = 3))
#' ad.test(runif(100, min = 2, max = 4))
ad.test <- function(x) {
  stopifnot(is.numeric(x), length(x) > 7L)
  dname <- deparse1(substitute(x))
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 8L) {
    stop("sample size must be greater than 7", call. = FALSE)
  }
  if (!all(is.finite(x))) {
    stop("`x` must contain only finite values", call. = FALSE)
  }
  sdx <- stats::sd(x)
  if (sdx == 0) {
    stop("`x` must have a non-zero standard deviation", call. = FALSE)
  }

  scaled <- (x - mean(x)) / sdx
  logp1 <- stats::pnorm(scaled, log.p = TRUE)
  logp2 <- stats::pnorm(-scaled, log.p = TRUE)
  h <- (2 * 1:n - 1) * (logp1 + rev(logp2))

  A <- -n - mean(h)
  AA <- (1 + 0.75 / n + 2.25 / n^2) * A
  if (AA < 0.2) {
    pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  } else if (AA < 0.34) {
    pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  } else if (AA < 0.6) {
    pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  } else if (AA < 10) {
    pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  } else {
    pval <- 3.7e-24
  }
  structure(
    list(
      statistic = c(A = A),
      p.value = pval,
      method = "Anderson-Darling normality test",
      data.name = dname
    ),
    class = "htest"
  )
}
