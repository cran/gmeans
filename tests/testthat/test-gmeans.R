test_that("gmeans works", {
  withr::local_seed(1234L)
  # x needs to be a matrix
  expect_error(gmeans(NULL))
  expect_error(gmeans(letters))
  expect_error(gmeans(numeric()))
  expect_error(gmeans(rnorm(5L)))
  # k_init and k_max need to be integers
  x <- matrix(rnorm(100L, sd = 0.3), ncol = 2L)
  colnames(x) <- c("x", "y")
  expect_error(gmeans(x, k_init = NA_integer_))
  expect_error(gmeans(x, k_init = 1:10))
  expect_error(gmeans(x, k_init = 1.5))
  expect_error(gmeans(x, k_max = NA_integer_))
  expect_error(gmeans(x, k_max = 1:10))
  expect_error(gmeans(x, k_max = 1.5))
  # level needs to be a number between 0 and 1
  expect_error(gmeans(x, level = NULL))
  expect_error(gmeans(x, level = NA_real_))
  expect_error(gmeans(x, level = 0))
  expect_error(gmeans(x, level = 1))
  expect_error(gmeans(x, level = c(0.5, 0.7)))
})

test_that("gmeans works with a single column", {
  withr::local_seed(1234L)
  x <- matrix(c(rnorm(60L, sd = 0.3), rnorm(60L, mean = 8, sd = 0.3)), ncol = 1L)
  colnames(x) <- "v"
  cl <- withr::with_seed(1L, gmeans(x))
  expect_s3_class(cl, "gmeans")
  expect_identical(ncol(cl$centers), 1L)
  expect_gt(nrow(cl$centers), 1L)
  expect_identical(colnames(cl$centers), "v")
  expect_identical(cl$centers, withr::with_seed(1L, gmeans(as.data.frame(x)))$centers)
})

test_that("gmeans works with a single initial center", {
  withr::local_seed(1234L)
  x <- rbind(
    matrix(rnorm(100L, sd = 0.3), ncol = 2L),
    matrix(rnorm(100L, mean = 3, sd = 0.3), ncol = 2L)
  )
  expect_gt(nrow(gmeans(x, k_init = 1L)$centers), 1L)
  expect_gt(nrow(gmeans(x[, 1L, drop = FALSE], k_init = 1L)$centers), 1L)
})

test_that("gmeans respects k_max", {
  withr::local_seed(2L)
  x <- do.call(rbind, lapply(seq(0, 60, by = 4), \(m) cbind(rnorm(40L, m), rnorm(40L, m))))
  expect_lte(nrow(gmeans(x, k_max = 3L)$centers), 3L)
  expect_lte(nrow(gmeans(x, k_max = 5L)$centers), 5L)
})

test_that("gmeans works with duplicated points", {
  withr::local_seed(11L)
  # a cluster of identical points has fewer distinct rows than the 2 centers
  # stats::kmeans() is asked for
  expect_null(split_and_search(matrix(rep(1, 20L), ncol = 2L), seq_len(10L), 0.05))
  x <- cbind(a = rep(1:3, 20L), b = rep(1:2, 30L))
  expect_s3_class(gmeans(x), "gmeans")
})

test_that("kmeans_plusplus works", {
  withr::local_seed(1234L)
  x <- matrix(rnorm(100L, sd = 0.3), ncol = 2L)
  for (i in 1:5) {
    res <- kmeans_plusplus(x, i)
    expect_identical(dim(res), c(i, 2L))
    expect_true(is.matrix(res))
  }

  # a constant column contributes no per-dimension difference, so seeding only
  # works when distances are summed over dimensions rather than minimised
  x <- cbind(a = as.numeric(1:20), b = 0)
  res <- kmeans_plusplus(x, 2L)
  expect_identical(dim(res), c(2L, 2L))
  expect_false(anyNA(res))
})

test_that("predict works", {
  withr::local_seed(1234L)
  x <- matrix(rnorm(100L, sd = 0.3), ncol = 2L)
  colnames(x) <- c("x", "y")
  cl <- gmeans(x)
  # should return an integer vector
  expect_vector(predict(cl, x), ptype = integer())
  # should raise an error with no new data provided
  expect_error(predict(cl))
  # newdata should work with a single row
  newdata <- matrix(rnorm(2L), ncol = 2L, dimnames = list(NULL, c("x", "y")))
  expect_length(predict(cl, newdata), 1L)
  expect_identical(predict(cl, newdata), predict(cl, rbind(newdata, newdata))[1L])
  expect_length(compute_wss(cl, newdata), nrow(cl$centers))
  # error when required cols are missing
  expect_error(predict(cl, x[, "x", drop = FALSE]))
  # allow more than required cols
  newdata <- cbind(x, z = 1:50)
  expect_no_error(predict(cl, newdata))
})

test_that("predict breaks ties deterministically", {
  km <- structure(
    list(
      centers = matrix(
        c(0, 0, 2, 2),
        ncol = 2L,
        byrow = TRUE,
        dimnames = list(NULL, c("x", "y"))
      )
    ),
    class = c("gmeans", "kmeans")
  )
  newdata <- matrix(c(1, 1, 1, 1), ncol = 2L, dimnames = list(NULL, c("x", "y")))
  expect_identical(predict(km, newdata), c(1L, 1L))
  expect_identical(
    replicate(10L, predict(km, newdata)),
    matrix(1L, nrow = 2L, ncol = 10L)
  )
})

test_that("ad.test works", {
  withr::local_seed(1234L)
  x <- rnorm(100L, mean = 5, sd = 3)
  res <- ad.test(x)
  expect_s3_class(res, "htest")
  expect_named(res, c("statistic", "p.value", "method", "data.name"))
  expect_vector(res$statistic, ptype = numeric(), size = 1L)
  expect_vector(res$p.value, ptype = numeric(), size = 1L)
  expect_gte(res$p.value, 0)
  expect_lte(res$p.value, 1)
  expect_identical(res$method, "Anderson-Darling normality test")
  expect_identical(res$data.name, "x")
  expect_snapshot(res)
  # input validation
  expect_error(ad.test(NULL))
  expect_error(ad.test(letters))
  expect_error(ad.test(numeric()))
  expect_error(ad.test(rnorm(7L)))
  expect_error(ad.test(rep(1, 20L)), "standard deviation")
  expect_error(ad.test(c(rnorm(10L), Inf)), "finite")
})

test_that("compute_wss works", {
  km <- kmeans(mtcars, 5)
  expect_equal(compute_wss(km), compute_wss(km, mtcars)) # nolint
})

test_that("compute_wss keeps one entry per cluster", {
  withr::local_seed(1234L)
  km <- kmeans(mtcars, 5L)
  # newdata reaching only a subset of the clusters
  wss <- compute_wss(km, mtcars[1:3, ])
  expect_length(wss, nrow(km$centers))
  expect_true(any(wss == 0))
  hit <- apply(rxdist(km, mtcars[1:3, ]), 1L, which.min)
  expect_identical(which(wss > 0), sort(unique(hit)))
})
