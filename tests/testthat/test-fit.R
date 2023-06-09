context("fitting curves")

test_that("curve fitting accepts only numeric input", {
  
  ymax = "a"
  xopt = 4
  xmin = 0
  xmax = 8
  
  expect_error( EndosymbiontModel:::fit_tpc("briere", ymax, xopt, xmin, xmax) )
  
  ymax = 0.5
  xopt = "a"
  
  expect_error( EndosymbiontModel:::fit_tpc("briere", ymax, xopt, xmin, xmax) )
  
  xopt = 4
  xmin = "a"
  
  expect_error( EndosymbiontModel:::fit_tpc("briere", ymax, xopt, xmin, xmax) )
  
  xmin = 0
  xmax = "a"
  
  expect_error( EndosymbiontModel:::fit_tpc("briere", ymax, xopt, xmin, xmax) )
  
})

test_that("xopt not between xmin and xmax", {
  
  ymax = 0.5
  xopt = -1
  xmin = 0
  xmax = 8
  
  expect_warning( EndosymbiontModel:::fit_tpc("briere", ymax, xopt, xmin, xmax) )
  
  expect_warning( EndosymbiontModel:::fit_tpc("gaussian", ymax, xopt, xmin, xmax) )
  
  expect_warning( EndosymbiontModel:::fit_tpc("quadratic", ymax, xopt, xmin, xmax) )
  
  expect_warning( EndosymbiontModel:::fit_tpc("rezende", ymax, xopt, xmin, xmax) )
  
  expect_warning( EndosymbiontModel:::fit_tpc("weibull", ymax, xopt, xmin, xmax) )
  
})