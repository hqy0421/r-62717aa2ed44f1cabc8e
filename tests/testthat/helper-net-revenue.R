# Shared fixtures for net_revenue_test() tests.
#
# The canonical fixture has six customers and one common cutoff. `t1` has two
# orders whose partial refunds sum to 1500, `t2` has a single order, `c1` has
# one order, `c2` has no orders (a zero-revenue customer), and `t4` is exposed
# too late to be mature. One order (`o5`) belongs to the immature customer.
nr_fixture <- function() {
  customers <- data.frame(
    customer_id = c("t1", "t2", "c1", "c2", "t4", "c3"),
    group = factor(
      c("treatment", "treatment", "control", "control", "treatment", "control"),
      levels = c("treatment", "control")
    ),
    exposure = as.Date(c(
      "2024-01-01", "2024-01-02", "2024-01-01", "2024-01-03",
      "2024-01-20", "2024-01-02"
    ))
  )

  orders <- data.frame(
    order_id = c("o1", "o2", "o3", "o4", "o5"),
    customer_id = c("t1", "t1", "c1", "t2", "t4"),
    order_time = as.Date(c(
      "2024-01-02", "2024-01-03", "2024-01-02", "2024-01-02", "2024-01-25"
    )),
    order_amount = c(5000, 2500, 4000, 1000, 7000)
  )

  refunds <- data.frame(
    refund_id = c("r1", "r2", "r3"),
    order_id = c("o1", "o1", "o3"),
    refund_time = as.Date(c("2024-01-04", "2024-01-05", "2024-01-06")),
    refund_amount = c(1000, 500, 400)
  )

  list(
    customers = customers,
    orders = orders,
    refunds = refunds,
    cutoff = as.Date("2024-01-31"),
    window = 14
  )
}

nr_run <- function(fx = nr_fixture(), ..., cutoff = NULL, window = NULL,
                   reps = 200, seed = 1) {
  if (is.null(cutoff)) {
    cutoff <- fx$cutoff
  }
  if (is.null(window)) {
    window <- fx$window
  }

  net_revenue_test(
    fx$customers,
    fx$orders,
    fx$refunds,
    cutoff = cutoff,
    window = window,
    reps = reps,
    seed = seed,
    ...
  )
}

# An order table with the default column names but no rows.
nr_empty_orders <- function() {
  data.frame(
    order_id = character(),
    customer_id = character(),
    order_time = as.Date(character()),
    order_amount = numeric()
  )
}

nr_two_customer_fixture <- function(
  order_amount = 1000,
  order_time = as.Date("2024-01-02"),
  refund_amount = numeric(),
  refund_time = NULL
) {
  customers <- data.frame(
    customer_id = c("a", "b"),
    group = factor(c("t", "c"), levels = c("t", "c")),
    exposure = as.Date(c("2024-01-01", "2024-01-01"))
  )
  orders <- data.frame(
    order_id = "o1",
    customer_id = "a",
    order_time = order_time,
    order_amount = order_amount
  )
  if (length(refund_amount) == 0) {
    refunds <- NULL
  } else {
    if (is.null(refund_time)) {
      refund_time <- rep(as.Date("2024-01-03"), length(refund_amount))
    }
    refunds <- data.frame(
      refund_id = paste0("r", seq_along(refund_amount)),
      order_id = "o1",
      refund_time = refund_time,
      refund_amount = refund_amount
    )
  }

  list(
    customers = customers,
    orders = orders,
    refunds = refunds,
    cutoff = as.Date("2024-02-01"),
    window = 10
  )
}
