test_that("missing or invalid arguments are rejected", {
  fx <- nr_fixture()

  expect_error(
    net_revenue_test(
      "not data", fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    class = "error"
  )
  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window, reps = 0
    ),
    class = "error"
  )
  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window, reps = 2.5
    ),
    class = "error"
  )
  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = 0
    ),
    class = "error"
  )
  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window, seed = "a"
    ),
    class = "error"
  )
})

test_that("duplicate IDs fail explicitly", {
  fx <- nr_fixture()

  dup_customers <- fx$customers
  dup_customers$customer_id <- c("t1", "t1", "c1", "c2", "t4", "c3")
  expect_error(
    net_revenue_test(
      dup_customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "customer_id"
  )

  dup_orders <- fx$orders
  dup_orders$order_id <- c("o1", "o1", "o3", "o4", "o5")
  expect_error(
    net_revenue_test(
      fx$customers, dup_orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "order_id"
  )

  dup_refunds <- fx$refunds
  dup_refunds$refund_id <- c("r1", "r1", "r3")
  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, dup_refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "refund_id"
  )
})

test_that("dangling refunds fail explicitly", {
  fx <- nr_fixture()
  fx$refunds$order_id[1] <- "does-not-exist"

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "does not exist"
  )
})

test_that("dangling refunds fail even when recorded after the cutoff", {
  fx <- nr_fixture()
  # Referential integrity is checked on the whole ledger, not just as-of
  # cutoff, so a dangling reference cannot hide behind a late timestamp.
  fx$refunds$order_id[1] <- "does-not-exist"
  fx$refunds$refund_time[1] <- as.Date("2024-02-15")

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff,
      window = fx$window,
      reps = 10
    ),
    "does not exist"
  )
})

test_that("refunds after the cutoff do not affect net revenue", {
  fx <- nr_fixture()
  # A well-formed refund recorded past the cutoff is ignored as-of cutoff.
  fx$refunds$refund_time[1] <- as.Date("2024-02-15")

  res <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = fx$cutoff,
    window = fx$window,
    reps = 10,
    seed = 1
  )

  t1 <- res$customers[res$customers$customer_id == "t1", ]
  # Only r2 (500) is as-of cutoff; r1 (1000) is not.
  expect_equal(t1$refund_cents, 500)
})

test_that("cumulative refunds above the paid amount fail explicitly", {
  fx <- nr_fixture()
  # o1 is 5000; three refunds of 2000 total 6000.
  fx$refunds <- data.frame(
    refund_id = c("r1", "r2", "r3"),
    order_id = "o1",
    refund_time = as.Date(c("2024-01-03", "2024-01-03", "2024-01-03")),
    refund_amount = c(2000, 2000, 2000)
  )

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "exceeding"
  )
})

test_that("refunds equal to the paid amount are allowed", {
  fx <- nr_two_customer_fixture(
    order_amount = 1000,
    refund_amount = c(1000)
  )
  fx$refunds$refund_time <- as.Date("2024-01-03")

  res <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = as.Date("2024-02-01"),
    window = 10,
    reps = 10,
    seed = 1
  )

  expect_equal(res$customers$net_revenue_cents[1], 0)
})

test_that("fractional or negative cents fail explicitly", {
  fx <- nr_two_customer_fixture()

  expect_error(
    net_revenue_test(
      fx$customers,
      data.frame(
        order_id = "o1", customer_id = "a",
        order_time = as.Date("2024-01-02"), order_amount = 10.5
      ),
      NULL,
      cutoff = as.Date("2024-02-01"), window = 10
    ),
    "whole numbers of cents"
  )

  expect_error(
    net_revenue_test(
      fx$customers,
      data.frame(
        order_id = "o1", customer_id = "a",
        order_time = as.Date("2024-01-02"), order_amount = -100
      ),
      NULL,
      cutoff = as.Date("2024-02-01"), window = 10
    ),
    "negative"
  )

  expect_error(
    net_revenue_test(
      fx$customers,
      data.frame(
        order_id = "o1", customer_id = "a",
        order_time = as.Date("2024-01-02"), order_amount = NA_real_
      ),
      NULL,
      cutoff = as.Date("2024-02-01"), window = 10
    ),
    "missing"
  )
})

test_that("time types must agree", {
  fx <- nr_two_customer_fixture()
  fx$orders$order_time <- as.POSIXct("2024-01-02", tz = "UTC")

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, NULL,
      cutoff = as.Date("2024-02-01"), window = 10
    ),
    "same time type"
  )
})

test_that("window must be a whole number of days for Date exposure", {
  fx <- nr_two_customer_fixture()

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, NULL,
      cutoff = as.Date("2024-02-01"), window = as.difftime(36, units = "hours")
    ),
    "whole number of days"
  )
})

test_that("exactly two groups are required", {
  fx <- nr_fixture()
  fx$customers$group <- c("a", "b", "c", "a", "b", "c")

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "two groups"
  )
})

test_that("arm labels must identify the two observed groups", {
  fx <- nr_fixture()

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window, treatment = "nope"
    ),
    "treatment"
  )
})

test_that("no mature customers is an error", {
  fx <- nr_fixture()

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = as.Date("2024-01-05"), window = 14
    ),
    "mature"
  )
})

test_that("a missing arm among mature customers is an error", {
  fx <- nr_fixture()
  # Make both mature groups treatment-only by moving all controls late.
  is_control <- fx$customers$group == "control"
  fx$customers$exposure[is_control] <- as.Date("2024-01-30")

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "No mature customers"
  )
})

test_that("orders for unknown customers are ignored with a message", {
  fx <- nr_fixture()
  extra <- data.frame(
    order_id = "ox", customer_id = "ghost",
    order_time = as.Date("2024-01-02"), order_amount = 500
  )
  fx$orders <- rbind(fx$orders, extra)

  expect_message(
    res <- net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window, reps = 10, seed = 1
    ),
    "not in `customers`"
  )

  ox <- res$orders[res$orders$order_id == "ox", ]
  expect_false(ox$counted)
  expect_identical(ox$attribution, "unknown_customer")
})

test_that("a missing column is reported by name", {
  fx <- nr_fixture()
  fx$orders$order_amount <- NULL

  expect_error(
    net_revenue_test(
      fx$customers, fx$orders, fx$refunds,
      cutoff = fx$cutoff, window = fx$window
    ),
    "order_amount"
  )
})
