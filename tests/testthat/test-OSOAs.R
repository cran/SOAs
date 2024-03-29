test_that("OSOAs", {
  temp <- OSOAs(DoE.base::L64.4.6, optimize=FALSE)
  expect_s3_class(temp, "SOA")
  expect_equal(attr(temp, "type"), "OSOA")
  expect_equal(attr(temp, "strength"), "3")
  expect_equal(dim(temp), c(256, 6))
  expect_equal(length(unique(c(temp))), 64)

  temp <- OSOAs(DoE.base::L27.3.4, el=2, optimize=FALSE)
  expect_snapshot_output(temp)  ## el=2
  expect_equal(attr(temp, "type"), "OSOA")
  expect_equal(attr(temp, "strength"), "3-")
  expect_equal(dim(temp), c(81, 4))
  expect_equal(length(unique(c(temp))), 9)

  temp <- OSOAs(DoE.base::oa.design(ID=L27.3.13), el=2, optimize=FALSE)
  expect_equal(attr(temp, "type"), "OSOA")
  expect_equal(attr(temp, "strength"), "2+")
  expect_equal(dim(temp), c(81, 13))
  expect_equal(length(unique(c(temp))), 9)

  temp <- OSOAs(DoE.base::oa.design(ID=L27.3.13, randomize=FALSE), optimize=FALSE)
  expect_snapshot_output(temp)  ## el=3
  expect_equal(attr(temp, "type"), "OSOA")
  expect_equal(attr(temp, "strength"), "2*")
  expect_equal(dim(temp), c(81, 12))
  expect_equal(length(unique(c(temp))), 27)
})
