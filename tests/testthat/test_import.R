context('import')

test_that('module loads', {
  
  expect_true(gpflow_available())
  expect_s3_class(gpflow, "python.builtin.module")

})
