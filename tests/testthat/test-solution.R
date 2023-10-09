
test_that("election_summary should return correct object", {
  summary <-election_summary()
  expect_equal(typeof(summary),"list")

  #Should be at least 1 party results
  expect_gt(length(summary),0)

  #Should be at least 1 vote percentage
  expect_gt(summary[[1]]$andel,0)

  #Should be at least 1 party name
  expect_gt(length(summary[[1]]$partibeteckning),0)
})

test_that("election_result has correct params", {
  #no county code
  expect_error(election_result())
  #empty county code
  expect_error(election_result(county_code=""))
  #wrong county code
  expect_error(election_result(county_code="100"))

  results <-election_result(county_code="10")

  expect_equal(typeof(results),"list")

  #Should be at least 1 party results
  expect_gt(length(results),0)

  #Should be at least 1 vote percentage
  expect_gt(results[[1]]$andel,0)

  #Should be at least 1 vote percentage
  expect_gt(length(results[[1]]$partibeteckning),0)

})
