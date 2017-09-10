context('generics')

test_that('kernel addition and multiplication works', {
  
  a <- gpflow$kernels$Constant(1L)
  b <- gpflow$kernels$RBF(1L)
  
  k1 <- a + b
  k2 <- a * b
  
  name <- which_gpflow()
  expect_s3_class(k1, paste0(name, ".kernels.Add"))
  expect_s3_class(k2, paste0(name, ".kernels.Prod"))
  
})

test_that('str works for kernels', {
  
  k <- gpflow$kernels$RBF(1L, variance = 2.4, lengthscales = 1.3)
  
  str_k <- capture.output(str(k))
  print_k <- capture.output(print(k))
  
  expected_text <- c("unnamed.lengthscales transform:+ve prior:None",
                     "[ 1.3]",
                     "unnamed.variance transform:+ve prior:None",
                     "[ 2.4]")

  expect_identical(str_k, expected_text)
  expect_identical(print_k, expected_text)
  
})