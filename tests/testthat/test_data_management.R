
d <- data.frame(x = c(rep(1:3, each = 10), rep(NA, 10)),
                y = rep(1:2, 20),
                z = rep(c('male', 'female', 'missing', NA), each = 10),
                age = 1:40)



# recode ------------------------------------------------------------------

test_that("mvdecode", {
  expect_equal({
    sum(is.na(mvdecode(d, 2:3, 4)$x))
  }, 30)
})

test_that("mvdecode", {
  expect_equal({
    sum(mvrecode(d, 99)$x == 99)
  }, 10)
})


test_that("recode", {
  expect_equal({
    sum(recode(d, y, 2/"female")$y == "female")
  }, 20)

  expect_equal({
    sum(recode(d, z, "male"/1, NA/0)$z == "1", na.rm = TRUE)
  }, 10)

  expect_equal({
    sum(recode(d, z, "male"/1, NA/0)$z == "0", na.rm = TRUE)
  }, 10)

  expect_equal({
    sum(recode(d, z, "missing"/NA, "male"/1, "female"/1)$z == 1, na.rm = TRUE)
  }, 20)

  expect_equal({
    sum(is.na(recode(d, z, "missing"/NA, "male"/1, "female"/1)$z), na.rm = TRUE)
  }, 20)

  expect_equal({
    sum(is.na(recode(d, x, 1:3/NA)$x), na.rm = TRUE)
  }, 40)

  expect_equal({
    sum((recode(d, list(x, z), NA/0) == 0), na.rm = TRUE)
  }, 20)

  expect_equal({
    sum((recode(d, y, 1/2, 2/1)$y == 1), na.rm = TRUE)
  }, 20)

})


# label -------------------------------------------------------------------

test_that("label", {
  expect_equal({
    attr(label(d, "dataset"), "label")
  }, c(d = "dataset"))

  expect_equal({
    attr(label(d, z="sex")$z, "label")
  }, c("sex"))
})


# egen --------------------------------------------------------------------

test_that("egen", {
  expect_equal({
    sum(egen(d, age, c(11, 21, 31))$age_cat == "1-10")
  }, 10)
})



# n_ N_ n_all -------------------------------------------------------------

test_that("n_", {
  expect_equal({
    sum(with(d, n_(x)) == 1)
  }, 4)

  expect_equal({
    sum(with(d, N_(x)) == 10)
  }, 40)

  expect_equal({
    with(d, n_all())
  }, 1:40)

})



# append ------------------------------------------------------------------

test_that("append", {
  expect_equal({
    dim(append(iris, infert))
  }, c(398, 13))
})



# generate ----------------------------------------------------------------

test_that("generate", {
  expect_equal({
    generate(cars, time, speed / dist)$time[1:10]
  }, (cars$speed / cars$dist)[1:10])
})




# replace -----------------------------------------------------------------

test_that("replace", {
  expect_equal({
    sum(replace(d, x, 4, is.na(x))$x == 4)
  }, 10)
})



# rowmiss -----------------------------------------------------------------

test_that("rowmiss", {
  expect_equal({
    sum(with(d, rowmiss()))
  }, 20)
})

