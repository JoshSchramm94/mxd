#' Converting csv design into design matrix
#'
#' Converts a Lighthouse Studio csv export to a design matrix including
#' assumed choice process. Allows for both direct or indirect anchoring.
#'
#' @param design csv design provided by Lighthouse Studio
#' @param id column name of participants' identifier
#' @param cs column name of the choice set variable
#' @param item column name of the item variable
#' @param ch column name of the choice variable
#' @param anchor character to specify anchor, if unanchored approach
#' leave empty
#' @param mxd_tasks numeric input to specify number of MaxDiff tasks
#' @param type character to specify choice process
#'
#' @details
#' `csv_to_dm()` converts a design export (including participants' responses)
#' from Lighthouse Studio into a design matrix required for mxd. Users have to
#' specify the design (`design`), the participant's unique identifier (`id`),
#' the variable indicating the choice set (`cs`), the column that indicates
#' which item was shown (`item`), the choice column (`ch`), whether it was an
#' anchored MaxDiff (if yes, participants can choose between `direct` and
#' `indirect`; if is an unanchored MaxDiff leave argument empty), the number of
#' MaxDiff tasks (`mxd_tasks`). Finally, the participant has to choose the
#' assumed decision process. mxd currently offers the following assumed
#' decision processes:
#'
#' \describe{
#'   \item{best-worst}{participants make best and worst choices simultaneously,
#'   i.e., they choose both best and worst choices from the same choice set}
#'   \item{best-worst-seq}{sequential best first and worst second, thus, the
#'   participant chooses the worst choice choice from a reduced set of
#'   alternatives (i.e., best choice is omitted from worst choice set)}
#'   \item{worst-best-seq}{sequential worst first and best second, thus, the
#'   participant chooses the best choice choice from a reduced set of
#'   alternatives (i.e., worst choice is omitted from best choice set)}
#'   \item{maxdiff}{maximum difference model, i.e., participant compares each
#'   possible pair of alternatives and chooses the one that has the maximum
#'   difference}
#'   \item{exploded}{exploded logit pairs, where each choice set is transformed
#'   into multiple pairwise comparisons (e.g., in a choice set of 4
#'   alternatives, where the participant chooses both the best and worst
#'   alternative, there are 5 pairwise comparisons)}
#'   \item{best-only}{only the best choice from each choice set is considered}
#'   \item{worst-only}{only the worst choice from each choice set is considered}
#' }
#'
#' Currently, all types are working for the standard MaxDiff and the direct
#' anchored MaxDiff. For the indirect anchored MaxDiff, only `best-worst`,
#' `best-worst-seq`, and `worst-best-seq` is working.
#'
#' @returns a data frame object
#'
#'
#' @export
#'
csv_to_dm <- function(
  design, id, cs, item, ch, anchor = NULL, mxd_tasks,
  type
) {
  # check whether all arguments are defined ------------------------------------
  check_input(
    must = c("type", "mxd_tasks", "design", "id", "cs", "ch", "item"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # check input for type
  if (isTRUE(is.null(anchor)) || isTRUE(anchor == "direct")) {
    allowed_input(type, c(
      "best-worst", "best-worst-seq", "worst-best-seq",
      "best-only", "worst-only", "maxdiff", "exploded"
    ))
  }

  if (isFALSE(is.null(anchor)) && isTRUE(anchor == "indirect")) {
    allowed_input(type, c(
      "best-worst", "best-worst-seq", "worst-best-seq",
      "best-only", "worst-only"
    ))
  }

  # check length of input
  ncol_input(design, {{ id }}, "id")
  ncol_input(design, {{ cs }}, "cs")
  ncol_input(design, {{ item }}, "item")
  ncol_input(design, {{ ch }}, "ch")

  # check for numeric / integer input
  allowed_class(mxd_tasks, c("numeric", "integer"))

  # check right input
  check_integer(list(
    "mxd_tasks" = mxd_tasks
  ))

  # check input of anchor
  if (!is.null(anchor)) {
    choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})
    allowed_input(anchor, c("direct", "indirect"))
  }

  # need best and worst choice per set
  if (is.null(anchor)) {
    bw_per_cs(
      dplyr::filter(design, {{ cs }} <= mxd_tasks),
      {{ id }}, {{ cs }}, {{ ch }}
    )
  }

  # preps ----------------------------------------------------------------------
  # select relevant variables from the design
  design <- dplyr::select(
    design,
    {{ id }}, {{ cs }}, {{ item }}, {{ ch }}
  )

  # check whether anchor is empty or set to direct
  if (isTRUE(is.null(anchor)) || isTRUE(anchor == "direct")) {
    # best-worst coding
    if (type %in% c("best-worst", "best-worst-seq", "worst-best-seq", "best-only", "worst-only")) {
      unanchored <- design %>%
        dplyr::filter({{ cs }} <= mxd_tasks) %>%
        bw_summary(., {{ item }}, {{ ch }}, c({{ id }}, {{ cs }})) %>%
        merge(
          x = design,
          y = .,
          by = c(var_names(design, variables = c({{ id }}, {{ cs }})))
        ) %>%
        dplyr::arrange({{ id }}, {{ cs }}) %>%
        dplyr::select(-{{ ch }}) %>%
        fastDummies::dummy_cols(.,
          select_columns = var_names(design, {{ item }}),
          remove_selected_columns = TRUE
        )

      # dummy vars
      item_vars <- dummy_names(design, unanchored, {{ item }})

      # prepare best choice data frame
      if (!(type %in% c("worst-best-seq", "worst-only"))) {
        best <- prepare_best_worst_ch(
          data = unanchored, id = {{ id }}, cs = {{ cs }}, vars = item_vars,
          bw_ind = "b", stack_pos = 1, type = type
        )
      } else if (type == "worst-best-seq") {
        best <- prepare_best_worst_ch(
          data = unanchored, id = {{ id }}, cs = {{ cs }}, vars = item_vars,
          bw_ind = "b", stack_pos = 2, type = type
        )
      }

      if (type %in% c("worst-best-seq", "worst-only")) {
        worst <- prepare_best_worst_ch(
          data = unanchored, id = {{ id }}, cs = {{ cs }}, vars = item_vars,
          bw_ind = "w", stack_pos = 1, type = type
        )
      } else if (type %in% c("best-worst", "best-worst-seq")) {
        worst <- prepare_best_worst_ch(
          data = unanchored, id = {{ id }}, cs = {{ cs }}, vars = item_vars,
          bw_ind = "w", stack_pos = 2, type = type
        )
      }

      # prepare worst choice data frame
      if (type == "best-only") {
        df_md <- best %>%
          dplyr::select(-bw) %>%
          dplyr::relocate(choice, .after = tidyselect::everything())
      }

      if (type == "worst-only") {
        df_md <- worst %>%
          dplyr::select(-bw) %>%
          dplyr::relocate(choice, .after = tidyselect::everything())
      }

      if (!grepl("-only", type)) {
        df_md <- bw_merge(best, worst, {{ id }}, {{ cs }})
      }
    }

    # maxdiff coding
    if (type == "maxdiff") {
      df_md <- design %>%
        dplyr::filter({{ cs }} <= mxd_tasks) %>%
        bw_mutate(., {{ item }}, {{ ch }}, c({{ id }}, {{ cs }})) %>%
        dplyr::arrange({{ id }}, {{ cs }}) %>%
        dplyr::group_by(dplyr::pick({{ id }}, {{ cs }}), b, w) %>%
        tidyr::expand(item = {{ item }}, alt2 = {{ item }}) %>%
        dplyr::ungroup() %>%
        dplyr::filter(item != alt2) %>%
        dplyr::mutate(item = factor(
          x = item,
          levels = seq.int(length(unique(item)))
        )) %>%
        fastDummies::dummy_cols(
          select_columns = "item",
          remove_selected_columns = FALSE
        ) %>%
        dplyr::mutate(
          alt = cumsum(c(1, diff({{ cs }}) == 0)),
          .by = c({{ id }}, {{ cs }})
        ) %>%
        dplyr::mutate(choice = ifelse(b == item & w == alt2, 1, 0))

      add_index <- which(colnames(df_md) == "item_1") - 1

      for (i in seq_len(nrow(df_md))) {
        df_md[i, (df_md[i, ][["alt2"]] + add_index)] <- -1
      }

      df_md <- df_md %>%
        dplyr::select(
          tidyselect::all_of(var_names(design, {{ id }})),
          tidyselect::all_of(var_names(design, {{ cs }})),
          alt, choice,
          tidyselect::all_of(tidyselect::starts_with("item_"))
        )

      colnames(df_md)[5:ncol(df_md)] <- paste0(
        var_names(design, {{ item }}),
        "_",
        seq.int(ncol(df_md) - 4L)
      )

      df_md <- df_md %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }

    # type exploded
    if (type == "exploded") {
      unanchored <- design %>%
        dplyr::filter({{ cs }} <= mxd_tasks) %>%
        bw_mutate(., {{ item }}, {{ ch }}, c({{ id }}, {{ cs }})) %>%
        dplyr::arrange({{ id }}, {{ cs }}) %>%
        dplyr::select(-{{ ch }}) %>%
        tidyr::pivot_wider(.,
          values_from = {{ item }},
          names_from = var
        )

      df <- data.frame()
      df_md <- purrr::map(seq_len(nrow(unanchored)), function(x) {
        vars <- names(unanchored)[startsWith(names(unanchored), "var_")]

        df <- rbind(
          df,
          unlist(unanchored[x, ][vars]) %>%
            .[!is.na(.)] %>%
            combi(., order = FALSE) %>%
            stats::setNames(paste0("item", seq_len(2))) %>%
            dplyr::mutate(
              id = unanchored[x, ][[var_names(design, {{ id }})]],
              b = unanchored[x, ][["b"]],
              w = unanchored[x, ][["w"]],
              alt = seq_len(nrow(.))
            ) %>%
            dplyr::filter(item1 %in% c(b, w) | item2 %in% c(b, w))
        )
      }) %>%
        purrr::list_rbind() %>%
        dplyr::select(paste0("item", seq_len(2)), "b", "w", "id", alt)

      names(df_md)[names(df_md) == "id"] <- var_names(design, {{ id }})
      df_md <- df_md %>%
        dplyr::mutate(choice = ch_exploded(.)) %>%
        tidyr::pivot_longer(
          cols = c(item1, item2),
          names_to = "bw",
          values_to = var_names(design, {{ item }})
        ) %>%
        fastDummies::dummy_cols(
          select_columns = var_names(design, {{ item }}),
          remove_selected_columns = FALSE
        ) %>%
        dplyr::mutate(alt = rep(seq_len(2), length.out = nrow(.))) %>%
        dplyr::mutate(
          cs = cumsum(c(1, diff(alt) < 0)),
          .by = {{ id }}
        )

      # rename
      names(df_md)[names(df_md) == "cs"] <- var_names(design, {{ cs }})

      # dummy vars
      item_vars <- dummy_names(design, df_md, {{ item }})

      #
      df_md <- df_md %>%
        dplyr::mutate(choice = ifelse({{ item }} == choice, 1, 0)) %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::all_of(item_vars),
            ~ ifelse(choice == 0, .x * -1, .x)
          )
        )

      # select relevant variables
      df_md <- dplyr::select(
        df_md, {{ id }}, {{ cs }}, alt, all_of(item_vars), choice
      )
    }


    # modify direct anchor
    if (!is.null(anchor)) {
      # first define the maximum number of choice sets to append accordingly
      maxcs <- dplyr::reframe(df_md, mx = max({{ cs }} + 1)) %>%
        unlist() %>%
        unname()

      # create direct anchor
      anchor_df <- design %>%
        dplyr::filter({{ cs }} > mxd_tasks) %>%
        dplyr::reframe(
          b = {{ item }}[{{ ch }} == 1],
          .by = c({{ id }}, {{ cs }})
        ) %>%
        merge(
          x = (dplyr::filter(design, {{ cs }} > mxd_tasks)),
          y = .,
          by = c(var_names(design, c({{ id }}, {{ cs }})))
        ) %>%
        dplyr::select(-{{ ch }}) %>%
        dplyr::rename("choice" = "b") %>%
        fastDummies::dummy_cols(.,
          select_columns = var_names(design, {{ item }}),
          remove_selected_columns = TRUE
        ) %>%
        dplyr::arrange({{ id }}, {{ cs }})

      # define the variable names of the anchor
      item_vars_anc <- dummy_names(design, anchor_df, {{ item }})


      anchor_df <- anchor_df %>%
        dplyr::mutate(
          newcs = cumsum(c(maxcs, diff({{ cs }}) != 0)),
          .by = {{ id }}
        ) %>%
        dplyr::mutate(
          choice = ifelse(max.col(.[item_vars_anc]) == choice, 1, 0)
        ) %>%
        dplyr::select(
          {{ id }}, newcs, choice, tidyselect::all_of(item_vars_anc)
        ) %>%
        dplyr::mutate(alt = seq_len(dplyr::n()), .by = c({{ id }}, newcs)) %>%
        dplyr::relocate(alt, .after = newcs)

      colnames(anchor_df)[which(colnames(anchor_df) == "newcs")] <- var_names(design, {{ cs }})

      vars <- setdiff(names(anchor_df), names(df_md))

      df_md[vars] <- 0

      df_md <- rbind(anchor_df, df_md) %>%
        dplyr::arrange({{ id }}, {{ cs }}, alt) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }
  }

  #-----------------------------------------------------------------------------
  # fix indirect anchor

  if (isFALSE(is.null(anchor)) && isTRUE(anchor == "indirect")) {
    unanchored <- design %>%
      dplyr::reframe(
        b = ifelse(any({{ ch }} != -1), {{ item }}[{{ ch }} == 1], NA),
        w = ifelse(any({{ ch }} != 1), {{ item }}[{{ ch }} == -1], NA),
        .by = c({{ id }}, {{ cs }})
      ) %>%
      merge(
        x = design,
        y = .,
        by = c(var_names(design, c({{ id }}, {{ cs }})))
      ) %>%
      dplyr::arrange({{ id }}, {{ cs }}) %>%
      dplyr::select(-{{ ch }}) %>%
      fastDummies::dummy_cols(.,
        select_columns = var_names(design, {{ item }}),
        remove_selected_columns = TRUE
      ) %>% # change to dummy coding
      dplyr::arrange({{ id }}, {{ cs }}) # sort data frame

    # dummy vars
    item_vars <- dummy_names(design, unanchored, {{ item }})

    # prepare best choice data frame
    if (!(type %in% c("worst-best-seq", "worst-only"))) {
      best <- prepare_best_worst_ch_ind(
        unanchored, {{ id }}, {{ cs }}, item_vars, "b", 1, type
      )
    } else if (type == "worst-best-seq") {
      best <- prepare_best_worst_ch_ind(
        unanchored, {{ id }}, {{ cs }}, item_vars, "b", 2, type
      )
    }

    if (type %in% c("worst-best-seq", "worst-only")) {
      worst <- prepare_best_worst_ch_ind(
        unanchored, {{ id }}, {{ cs }}, item_vars, "w", 1, type
      )
    } else if (type %in% c("best-worst", "best-worst-seq")) {
      worst <- prepare_best_worst_ch_ind(
        unanchored, {{ id }}, {{ cs }}, item_vars, "w", 2, type
      )
    }

    # prepare worst choice data frame
    if (type == "best-only") {
      df_md <- best %>%
        dplyr::select(-bw) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }

    if (type == "worst-only") {
      df_md <- worst %>%
        dplyr::select(-bw) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }

    if (!grepl("-only", type)) {
      df_md <- bw_merge(best, worst, {{ id }}, {{ cs }})
    }

    # prepare worst choice data frame
    if (type == "best-only") {
      df_md <- best %>%
        dplyr::select(-bw) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }

    if (type == "worst-only") {
      df_md <- worst %>%
        dplyr::select(-bw) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }

    if (!grepl("-only", type)) {
      df_md <- bw_merge(best, worst, {{ id }}, {{ cs }})
    }
  }
  return(as.data.frame(df_md))
}
