.ppo_policy_signal <- function(actions, action_map = c(-1, 0, 1), target_size = 1.0) {
  idx <- as.integer(actions) + 1L
  out <- rep(0.0, length(idx))
  valid <- !is.na(idx) & idx >= 1L & idx <= length(action_map)
  out[valid] <- action_map[idx[valid]] * target_size
  out
}

#' Train PPO Policy Through Python
#'
#' Experimental adapter around Python `stable_baselines3.PPO`. This keeps PPO
#' outside the core package runtime: both `reticulate` and the Python package
#' must be installed by the user.
#'
#' @param env Python gymnasium/gym-style environment object.
#' @param policy Policy name passed to `stable_baselines3.PPO`.
#' @param total_timesteps Integer number of training timesteps.
#' @param ppo_kwargs Named list of additional PPO constructor arguments.
#' @param seed Optional integer random seed.
#' @param verbose Integer verbosity passed to PPO.
#'
#' @return A Python PPO model object.
#' @export
train_ppo_policy_py <- function(env, policy = "MlpPolicy", total_timesteps = 10000L, ppo_kwargs = list(), seed = NULL, verbose = 0L) {
  .require_optional_package("reticulate", "PPO training")
  sb3 <- reticulate::import("stable_baselines3", delay_load = FALSE)
  args <- c(
    list(policy = policy, env = env, verbose = as.integer(verbose)),
    ppo_kwargs
  )
  if (!is.null(seed)) args$seed <- as.integer(seed)
  model <- do.call(sb3$PPO, args)
  model$learn(total_timesteps = as.integer(total_timesteps))
  model
}

#' PPO-Policy Target Positions
#'
#' Converts actions from a PPO-like policy into `strategyr` target positions.
#' The `model` can be a Python object with a `predict()` method or an R
#' function that accepts a numeric feature matrix and returns integer actions.
#'
#' @param DT A `data.table` containing policy feature columns.
#' @param model PPO-like policy object or R function.
#' @param feature_cols Character vector of numeric policy input columns.
#' @param action_map Numeric mapping from zero-based discrete action ids to
#'   signed exposure units. Defaults to short/flat/long.
#' @param target_size Numeric exposure multiplier.
#' @param deterministic Logical passed to Python `model$predict()`.
#' @param debug Logical; when `TRUE`, return actions and target positions.
#'
#' @return A numeric target-position vector, or a list when `debug = TRUE`.
#' @export
strat_ppo_policy_tgt_pos <- function(DT, model, feature_cols = "close", action_map = c(-1, 0, 1), target_size = 1.0, deterministic = TRUE, debug = FALSE) {
  .validate_market_dt(DT, feature_cols)
  obs <- as.matrix(DT[, feature_cols, with = FALSE])
  if (!is.numeric(obs)) {
    stop("PPO policy features must be numeric.", call. = FALSE)
  }

  if (is.function(model)) {
    actions <- model(obs)
  } else if (!is.null(model$predict)) {
    .require_optional_package("reticulate", "PPO policy prediction")
    pred <- model$predict(obs, deterministic = deterministic)
    actions <- if (is.list(pred)) pred[[1L]] else pred
    actions <- as.vector(actions)
  } else {
    stop("`model` must be an R function or a Python object with `predict()`.", call. = FALSE)
  }

  tgt_pos <- .ppo_policy_signal(actions, action_map = action_map, target_size = target_size)
  if (debug) {
    return(list(tgt_pos = tgt_pos, actions = actions, feature_cols = feature_cols))
  }
  tgt_pos
}

#' PPO-Policy Action Plan
#'
#' Applies the latest PPO-policy target position and translates it into an
#' executable action plan.
#'
#' @inheritParams strat_ppo_policy_tgt_pos
#' @param state Named list describing current trading state.
#' @param strat_id Integer strategy identifier.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ppo_policy_action_plan <- function(DT, state, model, feature_cols = "close", action_map = c(-1, 0, 1), target_size = 1.0, deterministic = TRUE, strat_id = 902L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ppo_policy_tgt_pos(
    DT,
    model = model,
    feature_cols = feature_cols,
    action_map = action_map,
    target_size = target_size,
    deterministic = deterministic,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}
