data_type = function(u){
  if(is.character(u) | is.factor(u)){
    type = "cat"
  } else{
    u_unique = unique(u) |> sort()
    integers_to_check = 0:max(u_unique)
    if(all(u_unique %in% integers_to_check)){
      type = "count"
    } else{
      type = "contin"
    }
  }
  return(type)
}

is_count_data = function(u){
  if(is_cat_data(u)){
    return(FALSE)
  } else{
    u_unique = unique(u) |> sort()
    integers_to_check = 0:max(u_unique)
    is_count_data = all(u_unique %in% integers_to_check)
    return(is_count_data)
  }
}

is_cat_data = function(u){
  is_cat_data = is.character(u) | is.factor(u)
  return(is_cat_data)
}

is_contin_data = function(u){
  return(!(is_count_data(u) | is_cat_data(u)))
}

really_has_method = function(mth, obj){
  sapply(class(obj), function(cl){!is.null(utils::getS3method(mth, cl, optional = TRUE))}) |> any()
}

#' Plot simulated data from a fitted regression model against the real data.
#'
#' @param obj The fitted regression model.
#' @param predictor Name of a predictor (independent) variable. Optional.
#' @details
#' The type of plot created depends on the predicted/dependent variable ("y") and whether "predictor" is specified. There are several cases depending on the type of data:
#'
#' 1) "predictor" is not specified, "y" is continuous: kernel density estimates
#' 2) "predictor" is not specified, "y" is count or categorical: bar plot of counts
#' 3) "predictor" is specified, "y" is continuous: scatter plot with "predictor" on the x-axis and "y" on the y-axis
#' 4) "predictor" is continuous and "y" is categorical or count: scatter plot with "predictor" on the x-axis and "y" (or a code for it if categorical) on the y-axis"
#' 5) "predictor" is categorical and "y" is categorical or count: bar plot of counts, separated by "predictor" (with different colors)
#' 6) "predictor" is count and "y" is categorical or count: "geom_count" style modified scatter plot with "predictor" on the x-axis and "y" (or a code for it if categorical) on the y-axis"; this will prevent overplotting
#'
#' @export
sim_plot = function(obj, predictor = NA, seed = NA){
  # set random seed (optionally)
  if(!is.na(seed)){
    set.seed(seed)
  }

  # get real data
  real_data = data.frame(y = model.frame(obj)[,1],
                         data_set = "real",
                         type = "real")
  if(is.factor(real_data$y)){ # fix factor data
    real_data$y = as.character(real_data$y)
  }
  y_data_type = data_type(real_data$y) # classify

  # simulate data from the fitted model
  sim_data = simulate(obj, nsim = 3)
  if(is.factor(sim_data[,1])){ # fix factor data
    sim_data = sim_data |> lapply(as.character)
  }
  sim_data = sim_data |>
    stack() |>
    dplyr::rename(y = values, data_set = ind) |>
    dplyr::mutate(type = "simulated")

  # combine real and simulated data
  combo_data = rbind(real_data, sim_data)

  # add predictor if needed
  if(!is.na(predictor)){
    combo_data$predictor = model.frame(obj)[,predictor]
    pred_data_type = data_type(combo_data$predictor) # classify
  }

  # make the plot
  if(is.na(predictor)){ # marginal distribution plot
    plot_obj = combo_data |> ggplot2::ggplot(aes(x = y))
    if(y_data_type == "contin"){
      plot_obj = plot_obj + ggplot2::geom_density(fill = "lightblue") # case 1
    } else{
      plot_obj = plot_obj + ggplot2::geom_bar() # case 2
    }
  } else{ # plot with predictor
    plot_obj = combo_data |> ggplot2::ggplot()
    if(y_data_type == "contin"){
      plot_obj = plot_obj + ggplot2::geom_point(aes(x = predictor, y = y)) # case 3
    } else if(pred_data_type == "contin" & y_data_type %in% c("cat", "count")){
      plot_obj = plot_obj + ggplot2::geom_point(aes(x = predictor, y = y)) # case 4
    } else if(pred_data_type == "cat" & y_data_type %in% c("cat", "count")){
      plot_obj = plot_obj + ggplot2::geom_bar(aes(x = y, fill = predictor, group = predictor), position = position_dodge2(preserve = "single")) # case 5
    } else if(pred_data_type == "count" & y_data_type %in% c("cat", "count")){
      plot_obj = plot_obj + ggplot2::geom_count(aes(x = predictor, y = y)) # case 6
    }
  }
  plot_obj = plot_obj + ggplot2::facet_wrap(~ data_set)

  return(plot_obj)
}
