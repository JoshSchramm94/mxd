# helper functions -------------------------------------------------------------
## extract column names
var_names <- function(data, variables) {
  colnames(dplyr::select(data, {{ variables }}))
}

## extract dummy column names
dummy_names <- function(design, data, item) {
  item_name <- var_names(design, {{ item }})
  names(data)[startsWith(names(data), paste0(item_name, "_"))]
}

## summarize best worst choices
bw_summary <- function(data, item, ch, group) {
  data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      b = {{ item }}[{{ ch }} == 1],
      w = {{ item }}[{{ ch }} == -1]
    ) %>%
    dplyr::ungroup()
}

## mutate best worst choices
bw_mutate <- function(data, item, ch, group) {
  data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(
      b = {{ item }}[{{ ch }} == 1],
      w = {{ item }}[{{ ch }} == -1],
      var = paste0("var_", seq_len(dplyr::n()))
    ) %>%
    dplyr::ungroup()
}

## prepare best worst choices for direct and unanchored
prepare_best_worst_ch <- function(data, id, cs, vars, bw_ind, stack_pos, type) {
  data <- data %>%
    dplyr::rename("choice" = bw_ind) %>%
    dplyr::mutate(
      bw = stack_pos,
      choice = ifelse(max.col(.[vars]) == choice, 1, 0)
    )

  if (type == "best-worst-seq" && bw_ind == "w") {
    data <- data %>%
      dplyr::filter(max.col(.[vars]) != b)
  }

  if (type == "worst-best-seq" && bw_ind == "b") {
    data <- data %>%
      dplyr::filter(max.col(.[vars]) != w)
  }

  data <- data %>%
    dplyr::mutate(alt = seq_len(dplyr::n()), .by = c({{ id }}, {{ cs }})) %>%
    dplyr::relocate(alt, .after = {{ cs }})

  if (bw_ind == "b") {
    data <- dplyr::select(data, -w)
  }

  if (bw_ind == "w") {
    data <- data %>%
      dplyr::select(-b) %>%
      dplyr::mutate_at(
        dplyr::vars(tidyselect::all_of(vars)),
        function(x) x * -1
      )
  }

  return(data)
}

## prepare best worst choices for indirect anchored
prepare_best_worst_ch_ind <- function(data, id, cs, vars, bw_ind, stack_pos, type) {
  data <- data %>%
    tidyr::drop_na(tidyselect::any_of(bw_ind)) %>%
    dplyr::rename("choice" = bw_ind) %>%
    dplyr::mutate(
      bw = stack_pos,
      choice = ifelse(max.col(.[vars]) == choice, 1, 0)
    )

  if (type == "best-worst-seq" && bw_ind == "w") {
    data <- data %>%
      dplyr::filter(max.col(.[vars]) != b) %>%
      dplyr::mutate_at(dplyr::vars({{ cs }}), ~ .x + .5) %>%
      dplyr::filter(max.col(.[, vars[-length(vars)]]) != b | is.na(b))
  }

  if (type == "best-worst" && bw_ind == "w") {
    data <- data %>%
      dplyr::mutate_at(dplyr::vars({{ cs }}), ~ .x + .5)
  }

  if (type == "worst-best-seq" && bw_ind == "b") {
    data <- data %>%
      dplyr::mutate_at(dplyr::vars({{ cs }}), ~ .x + .5) %>%
      dplyr::filter(max.col(.[vars[-length(vars)]]) != w | is.na(w))
  }

  data <- data %>%
    dplyr::mutate(alt = seq_len(dplyr::n()), .by = c({{ id }}, {{ cs }})) %>%
    dplyr::relocate(alt, .after = {{ cs }})

  if (bw_ind == "b") {
    data <- dplyr::select(data, -w)
  }

  if (bw_ind == "w") {
    data <- data %>%
      dplyr::select(-b) %>%
      dplyr::mutate_at(
        dplyr::vars(tidyselect::all_of(vars)),
        function(x) x * -1
      )
  }

  return(data)
}

## merge best and worst choices
bw_merge <- function(best, worst, id, cs) {
  rbind(best, worst) %>%
    dplyr::arrange({{ id }}, {{ cs }}, bw) %>%
    dplyr::group_by(dplyr::pick({{ id }})) %>%
    dplyr::mutate_at(dplyr::vars({{ cs }}), ~ cumsum(c(1, diff(alt) < 0))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-bw) %>%
    dplyr::relocate(choice, .after = tidyselect::everything())
}

## define choice for exploded paired comparisons
ch_exploded <- function(data) {
  data %>%
    apply(., 1, function(x) {
      ifelse(
        x["item1"] %in% x["b"] | x["item2"] %in% x["b"], x["b"],
        ifelse(x["item1"] %in% x["w"], x["item2"],
          ifelse(x["item2"] %in% x["w"], x["item1"], NA)
        )
      )
    })
}

## mean center variable
mean_center <- function(var) {
  var - mean(var)
}

range_100 <- function(var) {
  100 * ((var - min(var)) / diff(range(var)))
}

prob_scores <- function(var, size) {
  exp(var) / (exp(var) + (size - 1))
}

res_summary <- function(.data, var) {
  .data %>%
    dplyr::reframe(
      dplyr::across(
        {{ var }},
        function(x) {
          c(
            mean(x),
            sd(x),
            stats::quantile(x, probs = 0.025),
            stats::quantile(x, probs = 0.975)
          )
        }
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    stats::setNames(c("mw", "sd", "2.5%", "97.5%"))
}

res_summary_group <- function(.data, var, group) {
  group_names <- var_names(.data, {{ group }})

  .data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      dplyr::across(
        {{ var }},
        function(x) {
          c(
            mean(x),
            sd(x),
            stats::quantile(x, probs = 0.025),
            stats::quantile(x, probs = 0.975)
          )
        }
      )
    ) %>%
    stats::setNames(c(group_names, "res")) %>%
    dplyr::mutate(est = rep(c("mw", "sd", "2.5%", "97.5%"), length.out = nrow(.))) %>%
    tidyr::pivot_wider(
      names_from = est,
      values_from = res
    )
}

args_list <- function(args, def_args) {
  for (i in names(def_args)) {
    if (!(i %in% names(args))) {
      args[[i]] <- def_args[[i]]
    }
  }

  return(args)
}


mnl <- function(.data, variables) {
  var_names <- dplyr::select(.data, {{ variables }}) %>%
    colnames()

  .data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(var_names),
        function(x) exp(x) / rowSums(exp(.[var_names])) * 100
      )
    )
}


percentage <- function(x) {
  x / sum(x)
}


# test -------------------------------------------------------------------------

stanfit_input <- function(
    input,
    arg = rlang::caller_arg(input),
    call = rlang::caller_env()) {
  if (!isS4(input)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be class {.cls stanfit}",
        "{.arg {arg}} is of class {.cls {class(input)}}."
      ),
      call = call
    )
  }
}

labels_length <- function(
    labels,
    no_pars,
    call = rlang::caller_env()) {
  length_labels <- length(labels)
  allowed_length <- no_pars

  if (length_labels != no_pars) {
    cli::cli_abort(
      c(
        "{.arg labels} must have length {.num {no_pars}}",
        "currently, {.arg labels} have length {.num {length_labels}}."
      ),
      call = call
    )
  }
}

check_demo <- function(
    demos,
    length_id,
    call = rlang::caller_env()) {
  if (nrow(demos) != length_id) {
    cli::cli_abort(
      c(
        "Input for{.arg demos} must match length of {.arg id}",
        "currently, {.arg demos} have {.num {nrow(demos)}} rows",
        "{.arg id} has length {.num {length_id}}."
      ),
      call = call
    )
  }
}

allowed_class <- function(
    input,
    allowed,
    arg = rlang::caller_arg(input),
    call = rlang::caller_env()) {
  correct_input <- any(class(input) %in% allowed)

  if (!correct_input) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be class {.cls {allowed}}",
        "{.arg {arg}} currently class {.cls {class(input)}}."
      ),
      call = call
    )
  }
}

arg_not_defined <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
  if (rlang::is_missing(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing."
      ),
      call = call
    )
  }
}

id_match <- function(
    id1,
    id2,
    call = rlang::caller_env()) {
  if (!(all(id1 %in% id2) && all(id2 %in% id1))) {
    cli::cli_abort(
      c(
        "ids do not match."
      ),
      call = call
    )
  }

  if (class(id1) != class(id2)) {
    cli::cli_abort(
      c(
        "class of ids do not match",
        "id currently have type {.cls {class(id1)}} and {.cls {class(id2)}}",
        "class must match."
      ),
      call = call
    )
  }
}

post_check <- function(
    betas,
    arg = rlang::caller_arg(betas),
    call = rlang::caller_env()) {
  # check dimensions
  dim_must <- dim(betas[[1]])
  wrong_input <- all(unlist(lapply(betas, function(x) all(dim(x) == dim_must))))

  if (!wrong_input) {
    cli::cli_abort(
      c(
        "dimension of {.arg {arg}} do not match across {.arg {arg}}."
      ),
      call = call
    )
  }

  # check column names
  col_nam_must <- names(betas[[1]])
  wrong_input <- all(unlist(
    lapply(betas, function(x) {
      all(names(x) %in% col_nam_must) &&
        all(col_nam_must %in% names(x))
    })
  ))

  if (!wrong_input) {
    cli::cli_abort(
      c(
        "column names of {.arg {arg}} do not match across {.arg {arg}}."
      ),
      call = call
    )
  }
}

missing_allowed <- function(data,
                            var,
                            allowed = c("yes", "no"),
                            arg = rlang::caller_arg(var),
                            call = rlang::caller_env()) {
  var <- dplyr::select(data, {{ var }})

  if (allowed == "yes" && anyNA(var)) {
    cli::cli_warn(
      c(
        "{.arg {arg}} contain {.cls NA} values."
      )
    )
  }

  if (allowed == "no" && anyNA(var)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} contain {.cls NA} values."
      )
    )
  }
}

choice_per_cs <- function(
  data,
  id,
  cs,
  choice,
  call = rlang::caller_env()) {

  ws <- reframe(data, ch = sum({{ choice }}), .by = c({{ id }}, {{ cs }}))

  if (!(all(ws[["ch"]] == 1))) {
    cli::cli_abort(
      c(
        "Only one {.arg alt} can be chosen per {.arg cs}."
      )
    )
  }

}

bw_per_cs <- function(
    data,
    id,
    cs,
    choice,
    call = rlang::caller_env()) {

  ws <- reframe(data, b = sum({{ choice }} == 1),
                      w = sum({{ choice }} == -1),
                .by = c({{ id }}, {{ cs }}))

  if (!(all(ws[["b"]] == 1) && all(ws[["w"]] == -1))) {
    cli::cli_abort(
      c(
        "Only one {.arg alt} can be chosen per {.arg cs}."
      )
    )
  }

}

# taken from validateHOT -------------------------------------------------------
allowed_input <- function(
    input,
    allowed,
    arg = rlang::caller_arg(input),
    call = rlang::caller_env()) {
  correct_input <- all(input %in% allowed)

  if (!correct_input) {
    cli::cli_abort(
      c(
        "{.arg {arg}} can only have values {.val {allowed}}."
      ),
      call = call
    )
  }
}

ncol_input <- function(
    data,
    variable,
    argument,
    arg = rlang::caller_arg(argument),
    call = rlang::caller_env()) {
  var <- dplyr::select(data, {{ variable }}) %>% colnames()

  if (length(var) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} can only be {.num 1} variable.",
        "{.num {ncol(data[var])}} variabes are provided."
      ),
      call = call
    )
  }
}
