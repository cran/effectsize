test_that("basic examples", {
  if (getRversion() < "4.1.3") {
    skip_on_os("linux")
  }

  skip_if_not_installed("boot")

  # t.test
  x <- t.test(mpg ~ vs, data = mtcars)
  expect_warning(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = mtcars))

  # cor.test
  # no need to specify the data argument
  x <- cor.test(~ qsec + drat, data = mtcars)
  expect_warning(effectsize(x), "'htest' method is not")

  # wilcox.test
  x <- wilcox.test(mpg ~ vs, data = mtcars, exact = FALSE)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = mtcars))

  # friedman.test
  wb <- aggregate(warpbreaks$breaks, by = list(
    w = warpbreaks$wool, t = warpbreaks$tension
  ), FUN = mean)
  x <- friedman.test(x ~ w | t, data = wb)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = wb))

  # kruskal.test
  airquality2 <- airquality
  airquality2$Month <- as.factor(airquality2$Month)
  airquality2$Ozone <- ifelse(is.na(airquality2$Ozone), 10, airquality2$Ozone)
  x <- kruskal.test(Ozone ~ Month, data = airquality2)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = airquality2))
})

test_that("edge cases", {
  skip_if_not_installed("boot")

  # Example 1
  tt1 <- t.test(mpg ~ I(am + cyl == 4), data = mtcars)
  dd1 <- cohens_d(mpg ~ I(am + cyl == 4), data = mtcars, pooled_sd = FALSE)

  expect_warning(effectsize(tt1), "Unable to retrieve data")
  expect_no_warning(effectsize(tt1, data = mtcars))
  expect_identical(effectsize(tt1, data = mtcars)[[1]], dd1[[1]])

  # Example 2
  dat <- mtcars
  tt2 <- t.test(dat$mpg[dat$am == 1], dat$mpg[dat$am == 0])
  dd2 <- cohens_d(dat$mpg[dat$am == 1], dat$mpg[dat$am == 0], pooled_sd = FALSE)

  rm("dat")
  expect_warning(effectsize(tt2), "Unable to retrieve data")
  expect_no_warning(effectsize(tt2, data = mtcars))

  expect_identical(effectsize(tt2, data = mtcars)[[1]], dd2[[1]])

  # Example 3
  col_y <- "mpg"
  tt3 <- t.test(mtcars[[col_y]])
  dd3 <- cohens_d(mtcars[[col_y]])

  rm("col_y")
  expect_warning(effectsize(tt3), "Unable to retrieve data")
  expect_no_warning(effectsize(tt3, data = mtcars))
  expect_identical(effectsize(tt3, data = mtcars)[[1]], dd3[[1]])

  # Example 4
  tt4 <- t.test(mpg ~ as.factor(am), data = mtcars)

  expect_warning(effectsize(tt4), "Unable to retrieve data")
  expect_no_warning(effectsize(tt4, data = mtcars))

  # wilcox.test
  x <- wilcox.test(mpg ~ as.factor(vs), data = mtcars, exact = FALSE)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = mtcars))

  # friedman.test does not allow formula modifiers, skipping

  # kruskal.test
  airquality2 <- airquality
  airquality2$Month <- as.factor(airquality2$Month)
  airquality2$Ozone <- ifelse(is.na(airquality2$Ozone), 10, airquality2$Ozone)
  x <- kruskal.test(Ozone ~ as.factor(Month), data = airquality2)

  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = airquality2))

  # Paired t-test
  x <- t.test(mpg ~ 1, data = mtcars)
  expect_no_warning(effectsize(x, data = mtcars))

  x <- t.test(Pair(mpg, hp) ~ 1, data = mtcars)
  expect_no_warning(effectsize(x, data = mtcars))
})

test_that("subset and na.action", {
  if (getRversion() < "4.1.3") {
    skip_on_os("linux")
  }

  # t.test
  some_data <- mtcars
  some_data$mpg[1] <- NA

  tt <- t.test(mpg ~ am,
    data = some_data,
    alternative = "less",
    mu = 1,
    var.equal = TRUE,
    subset = cyl == 4,
    na.action = na.omit
  )

  d1 <- effectsize(tt,
    data = some_data,
    alternative = "less",
    mu = 1,
    var.equal = TRUE,
    subset = cyl == 4,
    na.action = na.omit
  )

  d2 <- cohens_d(mpg ~ am,
    data = some_data,
    alternative = "less",
    mu = 1,
    pooled_sd = TRUE,
    subset = cyl == 4,
    na.action = na.omit
  )

  expect_equal(d1, d2, ignore_attr = TRUE)

  # Paired t.test with formula
  sleep2 <- reshape(sleep,
    direction = "wide",
    idvar = "ID", timevar = "group"
  )
  sleep2$ID <- as.numeric(sleep2$ID)
  sleep2$extra.2[1] <- NA

  tt_paired <- t.test(
    Pair(extra.1, extra.2) ~ 1,
    data = sleep2,
    alternative = "less",
    var.equal = TRUE,
    subset = ID > 3,
    na.action = na.omit
  )

  d1_paired <- effectsize(
    tt_paired,
    data = sleep2,
    alternative = "less",
    var.equal = TRUE,
    subset = ID > 3,
    na.action = na.omit
  )

  d2_paired <- cohens_d(
    tt_paired,
    data = sleep2,
    alternative = "less",
    paired = TRUE,
    var.equal = TRUE,
    subset = ID > 3,
    na.action = na.omit
  )

  expect_identical(d1_paired, d2_paired)

  # wilcox.test
  x <- wilcox.test(
    mpg ~ vs,
    data = some_data,
    alternative = "less",
    mu = 1,
    var.equal = TRUE,
    subset = cyl == 4,
    na.action = na.omit,
    exact = FALSE
  )

  d1 <- effectsize(
    x,
    data = some_data,
    alternative = "less",
    mu = 1,
    var.equal = TRUE,
    subset = cyl == 4,
    na.action = na.omit
  )

  d2 <- rank_biserial(
    mpg ~ vs,
    data = some_data,
    alternative = "less",
    mu = 1,
    pooled_sd = TRUE,
    subset = cyl == 4,
    na.action = na.omit
  )

  expect_equal(d1, d2, ignore_attr = TRUE)

  # friedman.test
  wb <- aggregate(warpbreaks$breaks, by = list(
    w = warpbreaks$wool, t = warpbreaks$tension
  ), FUN = mean)
  new_row <- data.frame(w = "B", t = "H", x = 99, stringsAsFactors = FALSE)
  wb <- rbind(wb, wb[6, ], new_row)
  wb$x[7] <- NA

  x <- friedman.test(
    x ~ w | t,
    data = wb,
    subset = x < 99,
    na.action = na.omit
  )

  d1 <- effectsize(
    x,
    data = wb,
    subset = x < 99,
    na.action = na.omit
  )

  d2 <- kendalls_w(
    x ~ w | t,
    data = wb,
    subset = x < 99,
    na.action = na.omit
  )

  expect_equal(d1, d2, ignore_attr = FALSE)

  # kruskal.test
  airquality2 <- airquality
  airquality2$Month <- as.factor(airquality2$Month)
  airquality2$Ozone <- ifelse(is.na(airquality2$Ozone), 10, airquality2$Ozone)

  x <- kruskal.test(
    Ozone ~ Month,
    data = airquality2,
    subset = Month != 5,
    na.action = na.omit
  )

  set.seed(42)
  d1 <- effectsize(
    x,
    data = airquality2,
    alternative = "less",
    subset = Month != 5,
    na.action = na.omit
  )

  set.seed(42)
  d2 <- rank_epsilon_squared(
    Ozone ~ Month,
    data = airquality2,
    alternative = "less",
    subset = Month != 5,
    na.action = na.omit
  )

  expect_equal(d1, d2, ignore_attr = TRUE)

  # subset and na.omit arguments do not apply to square bracket subsetting
  # using the S3 method instead of the formula interface because no other
  # dataframe is provided on which to do the subsetting. So no test is
  # necessary here.

  # paired t-test with formula
  # Removing this test because paired t-test with formula isn't supported anymore
  #
  # before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, NA, 999)
  # after <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, NA, 999)
  # my_data <- data.frame(
  #   group = rep(c("before", "after"), each = 10),
  #   weight = c(before, after),
  #   stringsAsFactors = FALSE
  # )
  #
  # res <- t.test(weight ~ group,
  #   data = my_data, paired = TRUE,
  #   alternative = "less", na.omit = TRUE
  # )
  #
  # d1 <- effectsize(
  #   res,
  #   data = my_data,
  #   subset = weight < 999,
  #   na.action = na.omit
  # )
  #
  # d2 <- cohens_d(weight ~ group,
  #   data = my_data,
  #   paired = TRUE,
  #   alternative = "less",
  #   subset = weight < 999,
  #   na.action = na.omit
  # )
  #
  # expect_equal(d1, d2, ignore_attr = TRUE)
})
