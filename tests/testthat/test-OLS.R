outcome1 = "mpg"
predictors1 = c("cyl", "hp", "wt")

m1.1 = OLS(mtcars, outcome1, predictors1, through_origin=FALSE)
m1.2 = lm(mpg~cyl+hp+wt, data=mtcars)
t1.1 = m1.1$coefficients
t1.2 = m1.2$coefficients
names(t1.1) = NULL
names(t1.2) = NULL

m2.1 = OLS(mtcars, outcome1, predictors1, through_origin=TRUE)
m2.2 = lm(mpg~-1+cyl+hp+wt, data=mtcars)
t2.1 = m2.1$coefficients
t2.2 = m2.2$coefficients
names(t2.1) = NULL
names(t2.2) = NULL

set.seed(1)
x1 = rnorm(100)
x2 = rt(100, 20)
x3 = rchisq(100, 50)
err = rnorm(100, 0, 3)
y = x1 + x2 + x3 + err
data = data.frame(y, x1, x2, x3)

m3.1 = OLS(data, 1, 2:4, through_origin=FALSE)
m3.2 = lm(y~x1+x2+x3, data=data)
t3.1 = m3.1$coefficients
t3.2 = m3.2$coefficients
names(t3.1) = NULL
names(t3.2) = NULL

m4.1 = OLS(data, 1, 2:4, through_origin=TRUE)
m4.2 = lm(y~-1+x1+x2+x3, data=data)
t4.1 = m4.1$coefficients
t4.2 = m4.2$coefficients
names(t4.1) = NULL
names(t4.2) = NULL

test_that("OLS works", {
  expect_equal(t1.1, t1.2)
})
test_that("OLS works", {
  expect_equal(t2.1, t2.2)
})
test_that("OLS works", {
  expect_equal(t3.1, t3.2)
})
test_that("OLS works", {
  expect_equal(t4.1, t4.2)
})
