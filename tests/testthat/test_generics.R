context('generics')

test_that('kernel addition and multiplication works', {
  
  a <- gpflow$kernels$Constant(1L)
  b <- gpflow$kernels$RBF(1L)
  
  k1 <- a + b
  k2 <- a * b
  
  expect_s3_class(k1, "GPflow.kernels.Add")
  expect_s3_class(k2, "GPflow.kernels.Prod")
  
})

test_that('str works for kernels', {
  
  k <- gpflow$kernels$RBF(1L, variance = 2.4, lengthscales = 1.3)
  
  str_k <- capture.output(str(k))
  print_k <- capture.output(print(k))
  
  expected_text <- c("unnamed.variance transform:+ve prior:None",
                     "[ 2.4]",
                     "unnamed.lengthscales transform:+ve prior:None",
                     "[ 1.3]")

  expect_identical(str_k, expected_text)
  expect_identical(print_k, expected_text)
  
})