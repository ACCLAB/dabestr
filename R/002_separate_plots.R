# Contains functions responsible for separate plot functionalities via flow = FALSE.
#
# Contains `separate_idx`, `remove_last_ele_from_nested_list`, `create_xlabs_for_sankey` functions.

# Separate idx function
# TODO Add documentation with the roxygen format
separate_idx <- function(idx, paired) {
  separated_idx <- list()

  if (paired == "baseline") {
    for (group in idx) {
      ctrl_grp <- group[1]
      for (index in 2:length(group)) {
        test_grp <- group[index]
        separated_idx <- c(separated_idx, list(c(ctrl_grp, test_grp)))
      }
    }
  } else {
    for (group in idx) {
      for (index in 1:(length(group) - 1)) {
        ctrl_grp <- group[index]
        test_grp <- group[index + 1]
        separated_idx <- c(separated_idx, list(c(ctrl_grp, test_grp)))
      }
    }
  }

  return(separated_idx)
}

# Function that removes the last element from each subgroup within a list()
# TODO Add documentation with the roxygen format
remove_last_ele_from_nested_list <- function(nested_list) {
  ## nested_array can be in the form of list[][] or list(vectors[])
  for (index in 1:length(nested_list)) {
    sub_group <- nested_list[[index]]
    nested_list[[index]] <- sub_group[-length(sub_group)]
  }
  return(nested_list)
}

# Function that creates xlabels for separated sankey diagrams
# TODO Add documentation
create_xlabs_for_sankey <- function(idx,
                                    Ns,
                                    enquo_x) {
  sankey_x_labels <- c()
  Ns_sankey <- dplyr::ungroup(Ns)
  for (group in idx) {
    group_length <- length(group)
    for (i in 1:(group_length - 1)) {
      ctrl <- group[i]
      treat <- group[i + 1]
      count_for_pair <- Ns_sankey %>%
        dplyr::filter(!!enquo_x == treat) %>%
        dplyr::select(n) %>%
        dplyr::pull()
      label <- paste(ctrl, "\nv.s.\n", treat, "\nN=", count_for_pair)
      sankey_x_labels <- c(sankey_x_labels, label)
    }
  }
  return(sankey_x_labels)
}
