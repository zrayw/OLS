outcome1 = "mpg"
predictors1 = c("cyl", "hp", "wt")

m1.1 = OLS(mtcars, outcome1, predictors1, through_origin=FALSE)
m1.2 = lm(mpg~cyl+hp+wt, data=mtcars)
t1.1 = AOV(m1.1)[c("cyl", "hp", "wt", "Residuals"), c("Df", "Sum of Squares")]
t1.2 = as.matrix(anova(m1.2)[, c("Df", "Sum Sq")])
colnames(t1.1) = NULL
colnames(t1.2) = NULL

m2.1 = OLS(mtcars, outcome1, predictors1, through_origin=TRUE)
m2.2 = lm(mpg~-1+cyl+hp+wt, data=mtcars)
t2.1 = AOV(m2.1)[c("cyl", "hp", "wt", "Residuals"), c("Df", "Sum of Squares")]
t2.2 = as.matrix(anova(m2.2)[, c("Df", "Sum Sq")])
colnames(t2.1) = NULL
colnames(t2.2) = NULL

set.seed(1)
x1 = rnorm(100)
x2 = rt(100, 20)
x3 = rchisq(100, 50)
err = rnorm(100, 0, 3)
y = x1 + x2 + x3 + err
data = data.frame(y, x1, x2, x3)

m3.1 = OLS(data, 1, 2:4, through_origin=FALSE)
m3.2 = lm(y~x1+x2+x3, data=data)
t3.1 = AOV(m3.1)[c("x1", "x2", "x3", "Residuals"), c("Df", "Sum of Squares")]
t3.2 = as.matrix(anova(m3.2)[, c("Df", "Sum Sq")])
colnames(t3.1) = NULL
colnames(t3.2) = NULL

m4.1 = OLS(data, 1, 2:4, through_origin=TRUE)
m4.2 = lm(y~-1+x1+x2+x3, data=data)
t4.1 = AOV(m4.1)[c("x1", "x2", "x3", "Residuals"), c("Df", "Sum of Squares")]
t4.2 = as.matrix(anova(m4.2)[, c("Df", "Sum Sq")])
colnames(t4.1) = NULL
colnames(t4.2) = NULL

test_that("AOV works", {
  expect_equal(t1.1, t1.2)
})
test_that("AOV works", {
  expect_equal(t2.1, t2.2)
})
test_that("AOV works", {
  expect_equal(t3.1, t3.2)
})
test_that("AOV works", {
  expect_equal(t4.1, t4.2)
})
