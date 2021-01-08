test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
})
