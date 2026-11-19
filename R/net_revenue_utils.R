# Internal helpers for `net_revenue_test()` ---------------------------------
#
# These helpers keep the customer-level net revenue pipeline honest:
# every validation failure is raised with `cli::cli_abort()`, every monetary
# value is handled as a whole number of cents, and refunds are aggregated to
# the order level *before* any join so that multiple partial refunds can never
# multiply order rows.

nr_abort <- function(message, ..., call = rlang::caller_env()) {
  # Evaluate cli glue in the caller's frame so error messages can interpolate
  # the helper's arguments (e.g. `name`, `what`).
  cli::cli_abort(
    message,
    ...,
    .envir = rlang::caller_env(),
    call = call
  )
}

nr_check_data_frame <- function(x, arg, call = rlang::caller_env()) {
  if (!is.data.frame(x)) {
    nr_abort("`{arg}` must be a data frame, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  invisible(x)
}

# Pull a named column out of a data frame, failing with a helpful message if
# the argument is not a single column name or the column is absent.
nr_column <- function(x, name, arg, what, call = rlang::caller_env()) {
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)
  ) {
    nr_abort("`{arg}` must be a single column name.", call = call)
  }

  if (!name %in% names(x)) {
    nr_abort(
      "Column {.field {name}} supplied to `{arg}` was not found in `{what}`.",
      call = call
    )
  }

  x[[name]]
}

# Coerce IDs to character and insist the result uniquely identifies rows.
nr_ids <- function(x, name, what, call = rlang::caller_env()) {
  if (anyNA(x)) {
    nr_abort(
      "`{name}` in `{what}` must not contain missing values.",
      call = call
    )
  }

  chr <- as.character(x)

  if (anyNA(chr)) {
    nr_abort("`{name}` in `{what}` could not be coerced to character IDs.",
      call = call
    )
  }

  duplicated_ids <- unique(chr[duplicated(chr)])
  if (length(duplicated_ids) > 0) {
    examples <- paste(utils::head(duplicated_ids, 5), collapse = ", ")
    nr_abort(c(
      "`{name}` in `{what}` must be unique, but {length(duplicated_ids)} \\
       ID{?s} {?is/are} duplicated.",
      "i" = "Duplicated ID{?s}: {examples}."
    ), call = call)
  }

  chr
}

# Group labels are kept as character so both factor and character columns work.
nr_group_values <- function(x, name, what, call = rlang::caller_env()) {
  if (anyNA(x)) {
    nr_abort(
      "`{name}` in `{what}` must not contain missing values.",
      call = call
    )
  }

  chr <- as.character(x)

  if (anyNA(chr)) {
    nr_abort("`{name}` in `{what}` could not be coerced to character labels.",
      call = call
    )
  }

  chr
}

# Monetary columns must be whole cents. Handling them as whole numbers in
# double precision keeps every amount exact (all values are < 2^53).
nr_money <- function(x, name, what, call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    nr_abort("`{name}` in `{what}` must be numeric integer cents.", call = call)
  }

  if (anyNA(x)) {
    nr_abort(
      "`{name}` in `{what}` must not contain missing values.",
      call = call
    )
  }

  if (any(!is.finite(x))) {
    nr_abort("`{name}` in `{what}` must be finite.", call = call)
  }

  if (any(x < 0)) {
    nr_abort("`{name}` in `{what}` must not be negative.", call = call)
  }

  if (any(abs(x) >= 2^53)) {
    nr_abort(
      "`{name}` in `{what}` contains values too large to be represented \\
       exactly as whole cents.",
      call = call
    )
  }

  if (any(x != trunc(x))) {
    bad <- utils::head(x[x != trunc(x)], 5)
    examples <- paste(format(bad, trim = TRUE), collapse = ", ")
    nr_abort(c(
      "`{name}` in `{what}` must be whole numbers of cents, but contains \\
       fractional cents.",
      "i" = "Offending value{?s}: {examples}.",
      "i" = "Convert currency amounts to integer cents before calling \\
             {.fun net_revenue_test}."
    ), call = call)
  }

  as.numeric(x)
}

nr_time_class <- function(x) {
  if (inherits(x, "Date")) {
    "Date"
  } else if (inherits(x, "POSIXct")) {
    "POSIXct"
  } else if (inherits(x, "POSIXlt")) {
    "POSIXlt"
  } else {
    NA_character_
  }
}

nr_time <- function(x, name, what, call = rlang::caller_env()) {
  cls <- nr_time_class(x)

  if (is.na(cls)) {
    nr_abort(
      "`{name}` in `{what}` must be a `Date` or `POSIXct` time column.",
      call = call
    )
  }

  if (anyNA(x)) {
    nr_abort(
      "`{name}` in `{what}` must not contain missing values.",
      call = call
    )
  }

  if (identical(cls, "POSIXlt")) {
    x <- as.POSIXct(x)
  }

  x
}

nr_single_time <- function(x, arg, cls, call = rlang::caller_env()) {
  if (length(x) != 1L || is.na(x)) {
    nr_abort("`{arg}` must be a single, non-missing time.", call = call)
  }

  x_cls <- nr_time_class(x)

  if (is.na(x_cls)) {
    nr_abort("`{arg}` must be a `Date` or `POSIXct` time.", call = call)
  }

  if (identical(x_cls, "POSIXlt")) {
    x <- as.POSIXct(x)
    x_cls <- "POSIXct"
  }

  if (!identical(x_cls, cls)) {
    nr_abort(
      "`{arg}` must use the same time type as the exposure column \\
       ({cls}), not {x_cls}.",
      call = call
    )
  }

  x
}

# A fixed window is a positive length of time. For `Date` times it must be a
# whole number of days, since date arithmetic silently drops fractions.
nr_window <- function(window, cls, call = rlang::caller_env()) {
  if (inherits(window, "difftime")) {
    if (length(window) != 1L || is.na(window)) {
      nr_abort(
        "`window` must be a single, non-missing `difftime`.",
        call = call
      )
    }
    seconds <- as.numeric(window, units = "secs")
  } else if (is.numeric(window) && length(window) == 1L && is.finite(window)) {
    seconds <- window * 86400
    window <- as.difftime(window, units = "days")
  } else {
    nr_abort(
      "`window` must be a single `difftime` or a positive number of days.",
      call = call
    )
  }

  if (!is.finite(seconds) || seconds <= 0) {
    nr_abort("`window` must be strictly positive.", call = call)
  }

  if (identical(cls, "Date")) {
    days <- seconds / 86400
    if (days != trunc(days)) {
      nr_abort(
        "`window` must be a whole number of days when exposure times are \\
         `Date`s.",
        call = call
      )
    }
    window <- as.difftime(days, units = "days")
  }

  window
}

# Sum `values` into `n` bins given integer `index` values. Aggregating refunds
# this way (rather than joining refund rows to orders) is what keeps repeated
# partial refunds from duplicating order rows.
nr_sum_by <- function(values, index, n) {
  out <- numeric(n)

  if (length(values) == 0) {
    return(out)
  }

  sums <- tapply(values, index, sum)
  out[as.integer(names(sums))] <- as.numeric(sums)

  out
}

# Build a minimal infer object directly. `specify()` computes a t-test
# parameter at construction time and errors on essentially-constant groups,
# which net revenue (many all-zero customers) routinely produces. Constructing
# the object manually lets the *native* `generate()` / `calculate()` /
# `get_p_value()` code run unchanged.
nr_infer_object <- function(
  net_cents,
  group,
  treatment,
  control,
  hypothesized
) {
  x <- tibble::tibble(
    net_revenue_cents = as.numeric(net_cents),
    group = factor(group, levels = c(treatment, control))
  )

  attr(x, "response") <- rlang::sym("net_revenue_cents")
  attr(x, "explanatory") <- rlang::sym("group")
  attr(x, "response_type") <- "numeric"
  attr(x, "explanatory_type") <- "factor"
  attr(x, "type_desc_response") <- "num"
  attr(x, "type_desc_explanatory") <- "bin"
  attr(x, "theory_type") <- "Two sample t"
  attr(x, "generated") <- FALSE
  attr(x, "fitted") <- FALSE
  attr(x, "hypothesized") <- hypothesized
  attr(x, "type") <- if (hypothesized) "permute" else "bootstrap"

  if (hypothesized) {
    attr(x, "null") <- "independence"
  }

  class(x) <- c("infer", class(x))

  x
}
