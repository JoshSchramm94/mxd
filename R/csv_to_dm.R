#' Title
#'
#' @param design csv design provided by Sawtooth software
#' @param id column name of the id variable
#' @param cs column name of the choice set variable
#' @param item column name of the item variable
#' @param ch column name of the choice variable
#' @param anchor character to specify anchor, if unanchored approach
#' leave empty
#' @param mxd_tasks numeric input to specify number of MaxDiff tasks
#' @param type character to specify coding method
#' @param pos column name of the position variable
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
convert_csv_to_design_matrix <- function(
    design,
    id,
    cs,
    item,
    ch,
    anchor = NULL,
    mxd_tasks,
    type,
    pos) {


  # select relevant variables from the design
  design <- dplyr::select(design,
                          {{ id }}, {{ cs }}, {{ pos }}, {{ item }}, {{ ch }})


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
          by = c(var_names(design, variables = c({{ id }}, {{ cs }})))) %>%
        dplyr::arrange({{ id }}, {{ cs }}, {{ pos }}) %>%
        dplyr::select(-c({{ pos }}, {{ ch }})) %>%
        fastDummies::dummy_cols(.,
                                select_columns = var_names(design, {{ item }}),
                                remove_selected_columns = TRUE
        )

      # dummy vars
      item_vars <- dummy_names(design, unanchored, {{ item }})

      # prepare best choice data frame
      if (!(type %in% c("worst_best_seq", "worst-only"))) {
        best <- prepare_best_worst_ch(unanchored, {{ id }}, {{cs}}, item_vars, "b", 1, type)
      } else if (type == "worst_best_seq") {
        best <- prepare_best_worst_ch(unanchored, {{ id }}, {{cs}}, item_vars, "b", 2, type)
      }

      if (type %in% c("worst_best_seq", "worst-only")) {
        worst <- prepare_best_worst_ch(unanchored, {{ id }}, {{cs}}, item_vars, "w", 1, type)
      } else if (type %in% c("best-worst", "best-worst-seq")) {
        worst <- prepare_best_worst_ch(unanchored, {{ id }}, {{cs}}, item_vars, "w", 2, type)
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

      if (!grepl("-only", type)){

        df_md <- bw_merge(best, worst, {{ id }}, {{ cs }})

      }

    }


    # maxdiff coding
    if (type == "maxdiff" || type == "exploded") {
      unanchored <- design %>%
        dplyr::filter(., {{ cs }} <= mxd_tasks) %>%
        bw_mutate(., {{ item }}, {{ ch }}, c({{ id }}, {{ cs }})) %>%
        dplyr::arrange({{ id }}, {{ cs }}, {{ pos }}) %>% # order the data frame
        dplyr::select(-c({{ pos }}, {{ ch}})) %>% # delete position (just used for ordering purposes)
        tidyr::pivot_wider(.,
                           values_from = {{ item }},
                           names_from = var
        )
    }

    if (type == "maxdiff"){

      df <- data.frame()
      df_md = purrr::map(seq_len(nrow(unanchored)), function(x) {
         vars <- names(unanchored)[startsWith(names(unanchored), "var_")]

         df <- rbind(df,
                     unlist(unanchored[x, ][vars]) %>%
                       DescTools::CombSet(., 2, repl = FALSE, ord = TRUE) %>%
                       tibble::as_tibble() %>%
                       setNames(paste0("item", seq_len(2))) %>%
                       dplyr::mutate(
                         id = unanchored[x, ][[var_names(design, {{ id }})]],
                         b = unanchored[x, ][["b"]],
                         w = unanchored[x, ][["w"]],
                         choice = ifelse(item1 %in% b & item2 %in% w, 1, 0),
                         alt = seq_len(nrow(.))
                       ))
       }) %>%
         purrr::list_rbind() %>%
         dplyr::select(paste0("item", seq_len(2)), "id", choice, alt)

       names(df_md)[1] <- var_names(design, {{ item }})

        df_md <- df_md %>%
          fastDummies::dummy_cols(
            select_columns = var_names(design, {{ item }}),
            remove_selected_columns = TRUE
          ) %>%
          dplyr::mutate(item2 = paste0(var_names(design, {{ item }}), "_", item2))

        for (i in seq_len(nrow(df_md))) {
          df_md[i, df_md[i, ][["item2"]]] <- -1
        }

        names(df_md)[names(df_md) == "id"] <- var_names(design, {{ id }})

        df_md <- df_md %>%
          dplyr::select(-item2) %>%
          dplyr::mutate(
            cs = cumsum(c(1, diff(alt) < 0)),
            .by = {{ id }}
          ) %>%
          dplyr::relocate(cs, .before = alt) %>%
          dplyr::relocate(choice, .after = tidyselect::everything())

        names(df_md)[names(df_md) == "cs"] <- var_names(design, {{ cs }})

       return(df_md)
    }

    if (type == "exploded") {
      df <- data.frame()
      df_md = purrr::map(seq_len(nrow(unanchored)), function(x) {
        vars <- names(unanchored)[startsWith(names(unanchored), "var_")]

        df <- rbind(df,
                    unlist(unanchored[x, ][vars]) %>%
                      DescTools::CombSet(., 2, repl = FALSE, ord = FALSE) %>%
                      tibble::as_tibble() %>%
                      setNames(paste0("item", seq_len(2))) %>%
                      dplyr::mutate(
                        id = unanchored[x, ][[var_names(design, {{ id }})]],
                        b = unanchored[x, ][["b"]],
                        w = unanchored[x, ][["w"]],
                        alt = seq_len(nrow(.))
                      ) %>%
                      dplyr::filter(item1 %in% c(b, w) | item2 %in% c(b, w)))
      }) %>%
        purrr::list_rbind() %>%
        dplyr::select(paste0("item", seq_len(2)), "b", "w", "id", alt)

      names(df_md)[names(df_md) == "id"] <- var_names(design, {{ id }})
      df_md = df_md %>%
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
      df_md <- dplyr::select(df_md, {{ id }}, {{ cs }}, alt, all_of(item_vars), choice)

      return(df_md)
    }
  }
}


      # create for each observation
      for (i in 1:nrow(unanchored)) {
        Items <- unanchored %>%
          dplyr::filter(obs == i) %>% # filter relevant observation
          dplyr::select(tidyselect::all_of(tidyselect::starts_with("var_"))) %>% # select the variables where the item ids are stored
          unlist() %>% # unlist
          as.vector() # store as vector

        b <- unname(unlist(unanchored[i, "b"])) # store best choice
        w <- unname(unlist(unanchored[i, "w"])) # store worst choice

        explo <- utils::combn(Items, m = 2) %>% # create each potential paired comparison
          t() %>% # transpose
          as.data.frame() %>% # create data frame
          dplyr::filter(V1 %in% c(b, w) | V2 %in% c(b, w)) %>% # use only rows where we can infer the results (at least one must be chosen as best or worst)
          dplyr::rename(
            "Item_b" = 1,
            "Item_w" = 2
          ) %>% # rename both variables
          dplyr::mutate(
            id = unlist(unanchored %>% dplyr::filter(obs == i) %>% dplyr::select(., {{ id }})), # store id
            cs = unlist(unanchored %>% dplyr::filter(obs == i) %>% dplyr::select(., {{ cs }})), # store cs
            alt = seq(1, dplyr::n()), # create alt
            choice = dplyr::case_when(
              Item_b %in% b | Item_w %in% b ~ b,
              Item_b %in% w ~ Item_w,
              Item_w %in% w ~ Item_b
            ) # recode the choices in terms of best and worst choice
          ) %>%
          dplyr::relocate(id, cs, alt, .before = tidyselect::everything()) %>%
          tidyr::pivot_longer(.,
                              cols = tidyselect::starts_with("Item_"),
                              values_to = "Item",
                              names_to = "bw"
          ) %>% # change to longer format
          dplyr::select(-bw) %>% # delete irrelevant column
          dplyr::rename("comp" = "alt") %>%
          dplyr::group_by(comp) %>%
          dplyr::mutate(alt = seq(1, dplyr::n())) %>% # create alt variable
          dplyr::ungroup() %>%
          dplyr::group_by(id) %>%
          dplyr::ungroup() %>%
          dplyr::relocate(alt, .after = comp) # reorder data frame



        # create data frame
        if (i == 1) {
          df_md <- explo
        } else {
          df_md <- rbind(df_md, explo)
        }

        if (i == nrow(unanchored)) {
          df_md <- df_md %>%
            fastDummies::dummy_cols(., select_columns = "Item") %>% # change into dummy coding
            dplyr::mutate(choice = dplyr::case_when(
              choice == Item ~ 1,
              .default = 0
            )) %>% # create design matrix
            dplyr::select(-c(Item, cs)) %>%
            dplyr::group_by(id) %>%
            dplyr::mutate(
              comp = cumsum(c(1, diff(alt) < 0))
            ) %>%
            dplyr::ungroup() %>%
            dplyr::relocate(choice, .after = tidyselect::everything()) # reorder data frame

          # store column names
          col_names <- c(
            (design %>% dplyr::select(., {{ id }}) %>% colnames()),
            (design %>% dplyr::select(., {{ cs }}) %>% colnames()),
            "alt",
            c(df_md %>%
                dplyr::select(tidyselect::all_of(tidyselect::starts_with("Item_"))) %>%
                colnames(.)),
            "choice"
          )

          # rename finished data frame
          colnames(df_md) <- col_names
        }
      }
    }


    # modify direct anchor
    if (!is.null(anchor)) {
      # first define the maximum number of choice sets to append accordingly
      maxcs <- df_md %>%
        dplyr::reframe(mx = max({{ cs }} + 1)) %>%
        unlist() %>%
        unname()

      # create direct anchor
      anchor_df <- design %>%
        dplyr::filter(., {{ cs }} > mxd_tasks) %>% # select all cs above usual MaxDiff Tasks
        dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
        dplyr::reframe(
          b = {{ item }}[{{ ch }} == 1] # define the best choice
        ) %>%
        merge(x = (design %>% dplyr::filter(., {{ cs }} > mxd_tasks)), y = ., by = c(design %>% dplyr::select(., {{ id }}, {{ cs }}) %>% colnames())) %>% # merge with design
        dplyr::select(-{{ ch }}) %>% # delete "ch"
        dplyr::rename("choice" = "b") %>% # rename
        fastDummies::dummy_cols(., select_columns = (design %>% dplyr::select(., {{ item }}) %>% colnames()), remove_selected_columns = T) %>%  # change to dummy coding matrix
        dplyr::arrange({{ id }}, {{ cs }}) # order

      # define the variable names of the anchor
      item_vars_anc <- anchor_df %>%
        dplyr::select(tidyselect::all_of(tidyselect::starts_with(paste0((design %>% dplyr::select(., {{ item }}) %>% colnames()), "_")))) %>%
        colnames()

      anchor_df <- anchor_df %>%
        dplyr::group_by(dplyr::pick({{ id }})) %>%
        dplyr::mutate(
          newcs = cumsum(c(maxcs, diff({{ cs }}) != 0)) # fix choice set id
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          choice = ifelse(max.col(.[, item_vars_anc]) == choice, 1, 0) # recode choice to 0 (not chosen) or 1 (chosen)
        ) %>%
        dplyr::select({{ id }}, newcs, choice, tidyselect::all_of(item_vars_anc)) %>%
        dplyr::select({{ id }}, newcs, choice, tidyselect::all_of(item_vars_anc)) %>%
        dplyr::select(-ncol(.)) %>% # delete the anchor
        dplyr::group_by(dplyr::pick({{ id }}, newcs)) %>%
        dplyr::mutate(alt = seq(1, dplyr::n())) %>%
        dplyr::ungroup() %>%
        dplyr::relocate(alt, .after = newcs)


      colnames(anchor_df)[which(colnames(anchor_df) == "newcs")] <- design %>%
        dplyr::select(., {{ cs }}) %>%
        colnames(.)

      # take care if not all items were also asked for in the anchor question
      vars <- setdiff(names(df_md), names(anchor_df))

      # if yes, create these variables and set them to 0 in anchor_df
      if (length(vars) != 0) {
        for (i in seq(1, length(vars))) {
          anchor_df[[vars[i]]] <- 0
        }
      }

      df_md <- df_md %>%
        rbind(., anchor_df) %>%
        dplyr::arrange({{ id }}, {{ cs }}, alt)
    } else if (is.null(anchor)) {
      df_md[, (ncol(df_md) - 1)] <- NULL # delete one item in the design matrix to prevent linear dependency
    }
  }

  ##############################################################################
  # fix indirect anchor

  if (isFALSE(is.null(anchor)) & isTRUE(anchor == "indirect")) {
    unanchored <- design %>%
      dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
      dplyr::reframe(
        b = ifelse(any({{ ch }} != -1), {{ item }}[{{ ch }} == 1], NA), # fix anchor question where no best choice is given (all are a purchase option)
        w = ifelse(any({{ ch }} != 1), {{ item }}[{{ ch }} == -1], NA), # fix anchor question where no best choice is given (none is a purchase option)
      ) %>%
      dplyr::ungroup() %>%
      merge(
        x = design,
        y = .,
        by = c(design %>% dplyr::select(., {{ id }}, {{ cs }}) %>% colnames())
      ) %>% # merge with design
      dplyr::select(-{{ ch }}) %>% # delete actual choice
      dplyr::arrange({{ id }}, {{ cs }}, {{ pos }}) %>% # arrange according to id, choice set, and position
      dplyr::select(-{{ pos }}) %>% # delete position (just used for ordering purposes)
      fastDummies::dummy_cols(.,
                              select_columns = (design %>% dplyr::select(., {{ item }}) %>% colnames()),
                              remove_selected_columns = T
      ) %>% # change to dummy coding
      dplyr::arrange({{ id }}, {{ cs }}) # sort data frame

    item_vars <- unanchored %>%
      dplyr::select(tidyselect::all_of(tidyselect::starts_with(paste0((design %>% dplyr::select(., {{ item }}) %>% colnames()), "_")))) %>%
      colnames() # store the column names of the dummy coded variables (items in the MaxDiff)

    if (type == "best-worst" || type == "best-worst-seq" || type == "worst-best-seq") {
      if (type == "best-worst" || type == "best-worst-seq") {
        # preparing best choice
        best <- unanchored %>%
          tidyr::drop_na(., tidyselect::any_of("b")) %>% # drop those that have a missing for the b
          dplyr::rename("choice" = "b") %>% # rename "b" (best choice) to choice for merging purposes
          dplyr::relocate(choice, .after = tidyselect::everything()) %>% # relocate choice at the end of the data frame
          mutate(
            choice = ifelse(max.col(.[, item_vars]) == choice, 1, 0) # recode choice to 0 (not chosen) or 1 (chosen)
          ) %>%
          dplyr::select({{ id }}, {{ cs }}, choice, tidyselect::all_of(item_vars[-length(item_vars)])) %>% # select relevant variables
          dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
          mutate(alt = seq(1, dplyr::n())) %>% # create alternative variable
          ungroup() %>%
          relocate(alt, .after = {{ cs }}) # relocate "alt" variable
      }

      # fix best data set for worst-best-sequential coding // see comments above
      if (type == "worst-best-seq") {
        best <- unanchored %>%
          tidyr::drop_na(., tidyselect::any_of("b")) %>%
          dplyr::rename("choice" = "b") %>%
          dplyr::relocate(choice, .after = tidyselect::everything()) %>%
          dplyr::mutate(
            choice = ifelse(max.col(.[, item_vars]) == choice, 1, 0)
          ) %>%
          dplyr::mutate_at(dplyr::vars({{ cs }}), ~ .x + .5) %>% # for sorting purposes add .5 to cs
          dplyr::filter(max.col(.[, item_vars[-length(item_vars)]]) != w | is.na(w)) %>% # delete worst choice from the set
          dplyr::select({{ id }}, {{ cs }}, choice, tidyselect::all_of(item_vars[-length(item_vars)])) %>% # select relevant variables
          dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
          dplyr::mutate(alt = seq(1, dplyr::n())) %>% # create alternative variable
          dplyr::ungroup() %>%
          dplyr::relocate(alt, .after = {{ cs }}) # relocate "alt" variable
      }
      # create worst choice data set
      if (type == "best-worst" | type == "worst-best-seq") {
        worst <- unanchored %>%
          tidyr::drop_na(., tidyselect::any_of("w")) %>% # delete all that have a missing in worst choice
          dplyr::rename("choice" = "w") %>% # rename "w" (worst choice) to choice for merging purposes
          dplyr::relocate(choice, .after = tidyselect::everything()) %>% # relocate choice at the end of the data frame
          dplyr::mutate(
            choice = ifelse(max.col(.[, item_vars]) == choice, 1, 0) # recode choice to 0 (not chosen) or 1 (chosen)
          ) %>%
          dplyr::mutate_at(dplyr::vars(tidyselect::all_of(item_vars)), ~ .x * -1) %>% # multiply design matrix with -1
          dplyr::select({{ id }}, {{ cs }}, choice, tidyselect::all_of(item_vars[-length(item_vars)])) %>% # select relevant variables
          dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
          dplyr::mutate(alt = seq(1, dplyr::n())) %>%
          dplyr::ungroup() %>%
          dplyr::relocate(alt, .after = {{ cs }})
      }
      # fix worst choice set for ordering purposes
      if (type == "best-worst") {
        worst <- worst %>%
          dplyr::mutate_at(dplyr::vars({{ cs }}), ~ .x + .5) # for sorting purposes add .5 to cs
      }
      # fix worst choice set for sequential best-worst
      if (type == "best-worst-seq") {
        worst <- unanchored %>%
          tidyr::drop_na(., tidyselect::any_of("w")) %>% # drop na for worst choices
          dplyr::rename("choice" = "w") %>% # rename "w" (worst choice) to choice for merging purposes
          dplyr::relocate(choice, .after = tidyselect::everything()) %>% # relocate choice at the end of the data frame
          dplyr::mutate(
            choice = ifelse(max.col(.[, item_vars]) == choice, 1, 0), # recode choice to 0 (not chosen) or 1 (chosen)
            Set = Set + .5 # for sorting purposes add .5 to cs
          ) %>%
          dplyr::filter(max.col(.[, item_vars[-length(item_vars)]]) != b | is.na(b)) %>% # delete best choice from the set
          dplyr::mutate_at(dplyr::vars(tidyselect::all_of(item_vars)), ~ .x * -1) %>% # multiply design matrix with -1
          dplyr::select({{ id }}, {{ cs }}, choice, tidyselect::all_of(item_vars[-length(item_vars)])) %>% # select relevant variables
          dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
          dplyr::mutate(alt = seq(1, dplyr::n())) %>% # create alternative variable
          dplyr::ungroup() %>%
          dplyr::relocate(alt, .after = {{ cs }}) # relocate "alt" variable
      }
      # merge data frames
      df_md <- rbind(
        best, worst
      ) %>%
        dplyr::arrange({{ id }}, {{ cs }}) %>% # sort data frame
        dplyr::group_by(dplyr::pick({{ id }})) %>%
        dplyr::mutate_at(dplyr::vars({{ cs }}), ~ cumsum(c(1, diff(alt) < 0))) %>% # fix choice set
        dplyr::relocate(choice, .after = tidyselect::everything()) %>% # order data frame
        dplyr::ungroup()
    }
    # create data frame for best-only
    if (type == "best-only") {
      df_md <- unanchored %>%
        dplyr::rename("choice" = "b") %>% # rename "b" (best choice) to choice for merging purposes
        dplyr::relocate(choice, .after = tidyselect::everything()) %>% # relocate choice at the end of the data frame
        dplyr::mutate(
          choice = ifelse(max.col(.[, item_vars]) == choice, 1, 0) # recode choice to 0 (not chosen) or 1 (chosen)
        ) %>%
        dplyr::mutate_at(dplyr::vars(tidyselect::all_of(item_vars)), ~ ifelse(is.na(choice), .x * -1, .x)) %>% # mutate anchor if choice is na
        dplyr::mutate(choice = ifelse(is.na(choice), ifelse(max.col(abs(.[, item_vars])) == w, 1, 0), choice)) %>% # fix choice
        dplyr::select({{ id }}, {{ cs }}, choice, tidyselect::all_of(item_vars[-length(item_vars)])) %>% # delete anchor to prevent linear dependency
        dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
        dplyr::mutate(alt = seq(1, dplyr::n())) %>%
        dplyr::ungroup() %>%
        dplyr::relocate(alt, .after = {{ cs }}) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }
    if (type == "worst-only") {
      # see comments for best
      df_md <- unanchored %>%
        dplyr::rename("choice" = "w") %>%
        dplyr::relocate(choice, .after = tidyselect::everything()) %>%
        dplyr::mutate(
          choice = ifelse(max.col(.[, item_vars]) == choice, 1, 0)
        ) %>%
        dplyr::mutate_at(dplyr::vars(tidyselect::all_of(item_vars)), ~ ifelse(is.na(choice), .x, .x * -1)) %>% # mutate anchor if choice is na
        dplyr::mutate(choice = ifelse(is.na(choice), ifelse(max.col(abs(.[, item_vars])) == b, 1, 0), choice)) %>%
        dplyr::select({{ id }}, {{ cs }}, choice, tidyselect::all_of(item_vars[-length(item_vars)])) %>%
        dplyr::group_by(dplyr::pick({{ id }}, {{ cs }})) %>%
        dplyr::mutate(alt = seq(1, dplyr::n())) %>%
        dplyr::ungroup() %>%
        dplyr::relocate(alt, .after = {{ cs }}) %>%
        dplyr::relocate(choice, .after = tidyselect::everything())
    }
  }
  return(df_md)
}
