outcome1 = "mpg"
predictors1 = c("cyl", "hp", "wt")

m1.1 = OLS(mtcars, outcome1, predictors1, through_origin=FALSE)
m1.2 = lm(mpg~cyl+hp+wt, data=mtcars)
t1.1 = CI(m1.1, c("intercept", "cyl", "hp", "wt"), 0.95)[,c("2.5 %", "97.5 %")]
t1.2 = confint(m1.2)
rownames(t1.1) = NULL
rownames(t1.2) = NULL

m2.1 = OLS(mtcars, outcome1, predictors1, through_origin=TRUE)
m2.2 = lm(mpg~-1+cyl+hp+wt, data=mtcars)
t2.1 = CI(m2.1, c("cyl", "hp", "wt"), 0.95)[,c("2.5 %", "97.5 %")]
t2.2 = confint(m2.2)
rownames(t2.1) = NULL
rownames(t2.2) = NULL

set.seed(1)
x1 = rnorm(100)
x2 = rt(100, 20)
x3 = rchisq(100, 50)
err = rnorm(100, 0, 3)
y = x1 + x2 + x3 + err
data = data.frame(y, x1, x2, x3)

m3.1 = OLS(data, 1, 2:4, through_origin=FALSE)
m3.2 = lm(y~x1+x2+x3, data=data)
t3.1 = CI(m3.1, c("hp", "wt"), 0.95)[,c("2.5 %", "97.5 %")]
t3.2 = confint(m3.2, parm=c("hp", "wt"))
rownames(t3.1) = NULL
rownames(t3.2) = NULL

m4.1 = OLS(data, 1, 2:4, through_origin=FALSE)
m4.2 = lm(y~x1+x2+x3, data=data)
t4.1 = CI(m3.1, c("hp", "wt"), 0.90)[,c("5 %", "95 %")]
t4.2 = confint(m3.2, parm=c("hp", "wt"), level=0.9)
rownames(t4.1) = NULL
rownames(t4.2) = NULL

test_that("CI works", {
  expect_equal(t1.1, t1.2)
})
test_that("CI works", {
  expect_equal(t2.1, t2.2)
})
test_that("CI works", {
  expect_equal(t3.1, t3.2)
})
test_that("CI works", {
  expect_equal(t4.1, t4.2)
})
