.require_optional_package <- function(pkg, feature) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "`", pkg, "` is required for ", feature,
      ". Install it or keep this experimental path disabled.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.lstm_forecast_col <- function(target_col, lookback, horizon) {
  paste0("lstm_forecast_", target_col, "_", lookback, "_", horizon)
}

.make_lstm_arrays <- function(DT, feature_cols, target_col, lookback, horizon, scale = TRUE) {
  x_mat <- as.matrix(DT[, feature_cols, with = FALSE])
  y_vec <- DT[[target_col]]
  if (!is.numeric(x_mat) || !is.numeric(y_vec)) {
    stop("LSTM features and target must be numeric.", call. = FALSE)
  }
  n <- nrow(x_mat)
  last_end <- n - horizon
  if (last_end < lookback) {
    stop("Not enough rows for the requested `lookback` and `horizon`.", call. = FALSE)
  }

  x_center <- rep(0, ncol(x_mat))
  x_scale <- rep(1, ncol(x_mat))
  y_center <- 0
  y_scale <- 1
  if (scale) {
    x_center <- apply(x_mat, 2L, mean, na.rm = TRUE)
    x_scale <- apply(x_mat, 2L, stats::sd, na.rm = TRUE)
    x_scale[!is.finite(x_scale) | x_scale == 0] <- 1
    y_center <- mean(y_vec, na.rm = TRUE)
    y_scale <- stats::sd(y_vec, na.rm = TRUE)
    if (!is.finite(y_scale) || y_scale == 0) y_scale <- 1
    x_mat <- sweep(sweep(x_mat, 2L, x_center), 2L, x_scale, "/")
    y_vec <- (y_vec - y_center) / y_scale
  }

  end_idx <- seq.int(lookback, last_end)
  n_obs <- length(end_idx)
  n_feat <- length(feature_cols)
  x_arr <- array(NA_real_, dim = c(n_obs, lookback, n_feat))
  y <- rep(NA_real_, n_obs)
  for (i in seq_along(end_idx)) {
    e <- end_idx[[i]]
    x_arr[i, , ] <- x_mat[seq.int(e - lookback + 1L, e), , drop = FALSE]
    y[[i]] <- y_vec[[e + horizon]]
  }
  valid <- stats::complete.cases(matrix(x_arr, nrow = n_obs), y)
  list(
    x = x_arr[valid, , , drop = FALSE],
    y = y[valid],
    end_idx = end_idx[valid],
    x_center = x_center,
    x_scale = x_scale,
    y_center = y_center,
    y_scale = y_scale
  )
}

#' LSTM Forecasts
#'
#' Trains a minimal experimental LSTM regression model with `torch` and returns
#' one-step-or-multi-step forecasts aligned to the last input row of each
#' lookback window. This helper is optional: `torch` is a suggested dependency,
#' not part of the core package runtime.
#'
#' @param DT A `data.table` containing numeric feature and target columns.
#' @param feature_cols Character vector of numeric feature columns.
#' @param target_col Numeric target column to forecast.
#' @param lookback Integer number of historical rows in each sequence.
#' @param horizon Integer forecast horizon in rows.
#' @param train_ratio Fraction of available windows used for training.
#' @param epochs Integer training epochs.
#' @param hidden_size Integer LSTM hidden size.
#' @param num_layers Integer number of LSTM layers.
#' @param lr Numeric optimizer learning rate.
#' @param scale Logical; standardize features and target before training.
#' @param seed Optional integer random seed.
#' @param forecast_col Column name used when `add_col = TRUE`.
#' @param add_col Logical; when `TRUE`, add forecasts to `DT` in place.
#' @param debug Logical; when `TRUE`, return model, forecasts, and metadata.
#'
#' @return A numeric forecast vector, or a list when `debug = TRUE`.
#' @export
calc_lstm_forecast <- function(
  DT,
  feature_cols = "close",
  target_col = "close",
  lookback = 20L,
  horizon = 1L,
  train_ratio = 0.8,
  epochs = 20L,
  hidden_size = 16L,
  num_layers = 1L,
  lr = 0.001,
  scale = TRUE,
  seed = NULL,
  forecast_col = NULL,
  add_col = FALSE,
  debug = FALSE
) {
  self <- NULL
  .require_optional_package("torch", "LSTM forecasting")
  .validate_market_dt(DT, c(feature_cols, target_col))
  stopifnot(length(lookback) == 1L, lookback >= 2L)
  stopifnot(length(horizon) == 1L, horizon >= 1L)
  stopifnot(length(train_ratio) == 1L, is.finite(train_ratio), train_ratio > 0, train_ratio <= 1)
  stopifnot(length(epochs) == 1L, epochs >= 1L)
  stopifnot(length(hidden_size) == 1L, hidden_size >= 1L)
  stopifnot(length(num_layers) == 1L, num_layers >= 1L)
  stopifnot(length(lr) == 1L, is.finite(lr), lr > 0)

  if (!is.null(seed)) {
    torch::torch_manual_seed(as.integer(seed))
  }
  arr <- .make_lstm_arrays(DT, feature_cols, target_col, as.integer(lookback), as.integer(horizon), scale = scale)
  n_obs <- dim(arr$x)[[1L]]
  train_n <- max(1L, floor(n_obs * train_ratio))
  train_idx <- seq_len(train_n)

  x_train <- torch::torch_tensor(arr$x[train_idx, , , drop = FALSE], dtype = torch::torch_float())
  y_train <- torch::torch_tensor(matrix(arr$y[train_idx], ncol = 1L), dtype = torch::torch_float())

  model_def <- torch::nn_module(
    "strategyr_lstm_forecaster",
    initialize = function(input_size, hidden_size, num_layers) {
      self$lstm <- torch::nn_lstm(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        batch_first = TRUE
      )
      self$head <- torch::nn_linear(hidden_size, 1L)
    },
    forward = function(x) {
      lstm_out <- self$lstm(x)[[1L]]
      last_step <- lstm_out[, dim(lstm_out)[[2L]], ]
      self$head(last_step)
    }
  )
  model <- model_def(
    input_size = length(feature_cols),
    hidden_size = as.integer(hidden_size),
    num_layers = as.integer(num_layers)
  )
  optimizer <- torch::optim_adam(model$parameters, lr = lr)
  loss_fn <- torch::nn_mse_loss()

  model$train()
  for (epoch in seq_len(as.integer(epochs))) {
    optimizer$zero_grad()
    pred <- model(x_train)
    loss <- loss_fn(pred, y_train)
    loss$backward()
    optimizer$step()
  }

  model$eval()
  x_all <- torch::torch_tensor(arr$x, dtype = torch::torch_float())
  pred_scaled <- as.numeric(model(x_all)$detach())
  pred <- pred_scaled * arr$y_scale + arr$y_center
  forecast <- rep(NA_real_, nrow(DT))
  forecast[arr$end_idx] <- pred

  if (add_col) {
    if (is.null(forecast_col)) {
      forecast_col <- .lstm_forecast_col(target_col, lookback, horizon)
    }
    data.table::set(DT, j = forecast_col, value = forecast)
  }

  if (debug) {
    return(list(
      forecast = forecast,
      model = model,
      feature_cols = feature_cols,
      target_col = target_col,
      lookback = lookback,
      horizon = horizon,
      train_n = train_n,
      forecast_col = forecast_col
    ))
  }
  forecast
}
