test_that("net revenue is aggregated to one row per mature customer", {
  fx <- nr_fixture()
  res <- nr_run(fx)

  expect_s3_class(res, "net_revenue_test")
  expect_named(
    res,
    c(
      "estimate", "null_distribution", "p_value", "customers",
      "immature_customers", "orders", "groups", "reps", "cutoff", "window"
    )
  )

  # Exactly one row per mature customer, and no duplicates.
  mature_ids <- c("t1", "t2", "c1", "c2", "c3")
  expect_setequal(res$customers$customer_id, mature_ids)
  expect_equal(anyDuplicated(res$customers$customer_id), 0L)

  # Immature customers are reported separately and excluded.
  expect_identical(res$immature_customers$customer_id, "t4")
  expect_false(any(mature_ids %in% res$immature_customers$customer_id))
})

test_that("zero-order customers are retained with zero net revenue", {
  res <- nr_run()

  zero <- res$customers[res$customers$customer_id == "c2", ]
  expect_equal(nrow(zero), 1)
  expect_equal(zero$n_orders, 0L)
  expect_equal(zero$gross_cents, 0)
  expect_equal(zero$refund_cents, 0)
  expect_equal(zero$net_revenue_cents, 0)
})

test_that("multiple partial refunds never multiply an order row", {
  res <- nr_run()

  t1 <- res$customers[res$customers$customer_id == "t1", ]
  # o1 = 5000 with refunds 1000 + 500; o2 = 2500 with no refund.
  expect_equal(t1$n_orders, 2L)
  expect_equal(t1$gross_cents, 7500)
  expect_equal(t1$refund_cents, 1500)
  expect_equal(t1$net_revenue_cents, 6000)

  o1 <- res$orders[res$orders$order_id == "o1", ]
  expect_equal(nrow(o1), 1)
  expect_equal(o1$refund_cents, 1500)
  expect_equal(o1$net_cents, 3500)
})

test_that("only orders in the half-open exposure window are counted", {
  # Window is [exposure, exposure + 10 days).
  start <- nr_run(
    nr_two_customer_fixture(order_time = as.Date("2024-01-01"))
  )
  expect_true(start$orders$counted)

  inside <- nr_run(
    nr_two_customer_fixture(order_time = as.Date("2024-01-10"))
  )
  expect_true(inside$orders$counted)

  end <- nr_run(
    nr_two_customer_fixture(order_time = as.Date("2024-01-11"))
  )
  expect_false(end$orders$counted)
  expect_equal(end$customers$net_revenue_cents[1], 0)
})

test_that("refunds are counted only through the cutoff", {
  at_cutoff <- nr_run(
    nr_two_customer_fixture(
      refund_amount = 100,
      refund_time = as.Date("2024-03-01")
    ),
    cutoff = as.Date("2024-03-01")
  )
  expect_equal(at_cutoff$customers$refund_cents[1], 100)
  expect_equal(at_cutoff$customers$net_revenue_cents[1], 900)

  after_cutoff <- nr_run(
    nr_two_customer_fixture(
      refund_amount = 100,
      refund_time = as.Date("2024-03-02")
    ),
    cutoff = as.Date("2024-03-01")
  )
  expect_equal(after_cutoff$customers$refund_cents[1], 0)
  expect_equal(after_cutoff$customers$net_revenue_cents[1], 1000)
})

test_that("amounts are handled as exact integer cents", {
  fx <- nr_two_customer_fixture()
  many <- data.frame(
    order_id = sprintf("o%04d", 1:1000),
    customer_id = "a",
    order_time = as.Date("2024-01-02"),
    order_amount = rep(999, 1000)
  )

  res <- net_revenue_test(
    fx$customers, many, NULL,
    cutoff = as.Date("2024-02-01"),
    window = 10,
    reps = 10,
    seed = 1
  )

  expect_equal(res$customers$gross_cents[1], 999000)
  expect_identical(res$customers$gross_cents[1], 999000)
})

test_that("the estimate is treatment minus control on customer means", {
  res <- nr_run()
  customers <- res$customers

  treatment <- customers$net_revenue_cents[customers$group == "treatment"]
  control <- customers$net_revenue_cents[customers$group == "control"]

  # treatment: 6000, 1000; control: 3600, 0, 0.
  expect_equal(res$estimate, mean(treatment) - mean(control))
  expect_equal(res$estimate, 2300)
})

test_that("permutation is customer-level and keeps group sizes", {
  res <- nr_run()
  groups <- res$groups

  expect_equal(groups$treatment, "treatment")
  expect_equal(groups$control, "control")
  # Mature counts, not order counts.
  expect_equal(groups$n_treatment, 2)
  expect_equal(groups$n_control, 3)

  expect_named(res$null_distribution, c("replicate", "stat"))
  expect_equal(nrow(res$null_distribution), 200)

  # Every replicate reuses all five mature customers' net revenues; if orders
  # or purchasers only were permuted, the reachable statistics would differ.
  reachable <- unique(res$null_distribution$stat)
  expect_lte(length(reachable), choose(5, 2))
})

test_that("permuting keeps exactly the observed customers and their values", {
  fx <- nr_fixture()
  res <- nr_run(fx)

  expect_setequal(res$customers$customer_id, c("t1", "t2", "c1", "c2", "c3"))
  expect_equal(
    sort(res$customers$net_revenue_cents),
    c(0, 0, 1000, 3600, 6000)
  )
})

test_that("the p-value is two-sided", {
  res <- nr_run()
  obs <- res$estimate
  null <- res$null_distribution$stat

  expected <- 2 * min(mean(null <= obs), mean(null >= obs))
  expected <- min(expected, 1)

  expect_equal(res$p_value, expected)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
})

test_that("seeding is reproducible and honors the global RNG", {
  fx <- nr_fixture()

  a <- nr_run(fx, seed = 7)
  b <- nr_run(fx, seed = 7)
  expect_identical(a$null_distribution$stat, b$null_distribution$stat)

  set.seed(123)
  c <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = fx$cutoff, window = fx$window, reps = 50
  )
  set.seed(123)
  d <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = fx$cutoff, window = fx$window, reps = 50
  )
  expect_identical(c$null_distribution$stat, d$null_distribution$stat)
})

test_that("orders are attributed with a clear status", {
  res <- nr_run()

  expect_named(
    res$orders,
    c(
      "order_id", "customer_id", "order_time", "order_amount_cents",
      "customer_mature", "counted", "refund_cents", "net_cents",
      "attribution"
    )
  )

  o5 <- res$orders[res$orders$order_id == "o5", ]
  expect_false(o5$counted)
  expect_identical(o5$attribution, "immature_customer")
  expect_equal(o5$net_cents, 0)
})

test_that("all-zero revenue still yields a valid test", {
  fx <- nr_two_customer_fixture()

  res <- net_revenue_test(
    fx$customers, nr_empty_orders(), NULL,
    cutoff = as.Date("2024-02-01"),
    window = 10,
    reps = 50,
    seed = 1
  )

  expect_equal(res$estimate, 0)
  expect_equal(res$p_value, 1)
  expect_equal(res$customers$net_revenue_cents, c(0, 0))
})

test_that("exposure starts and refunds are joined to the right customer", {
  fx <- nr_two_customer_fixture(
    refund_amount = c(300, 200),
    refund_time = as.Date(c("2024-01-03", "2024-01-04"))
  )

  res <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = as.Date("2024-02-01"),
    window = 10,
    reps = 10,
    seed = 1
  )

  a <- res$customers[res$customers$customer_id == "a", ]
  expect_equal(a$refund_cents, 500)
  expect_equal(a$net_revenue_cents, 500)
})

test_that("POSIXct times work with a difftime window", {
  fx <- nr_fixture()
  fx$customers$exposure <- as.POSIXct(fx$customers$exposure, tz = "UTC")
  fx$orders$order_time <- as.POSIXct(fx$orders$order_time, tz = "UTC")
  fx$refunds$refund_time <- as.POSIXct(fx$refunds$refund_time, tz = "UTC")
  fx$cutoff <- as.POSIXct("2024-01-31", tz = "UTC")

  res <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = fx$cutoff,
    window = as.difftime(14, units = "days"),
    reps = 50,
    seed = 1
  )

  expect_equal(res$estimate, 2300)
})

test_that("treatment and control can be supplied explicitly", {
  fx <- nr_fixture()
  fx$customers$group <- as.character(fx$customers$group)

  expect_error(net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = fx$cutoff, window = fx$window, reps = 10
  ))

  res <- net_revenue_test(
    fx$customers, fx$orders, fx$refunds,
    cutoff = fx$cutoff, window = fx$window,
    treatment = "treatment", control = "control",
    reps = 50, seed = 1
  )

  expect_equal(res$estimate, 2300)
})

test_that("print method summarises the test", {
  res <- suppressMessages(nr_run())

  expect_snapshot(print(res))
})
