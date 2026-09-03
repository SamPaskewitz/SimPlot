is_count_data = function(y){
  y_unique = unique(y) |> sort()
  integers_to_check = 0:(length(y_unique) - 1)
  matches = all(y_unique %in% integers_to_check)
  return(matches)
}

really_has_method = function(mth, obj){
  sapply(class(obj), function(cl){!is.null(utils::getS3method(mth, cl, optional = TRUE))}) |> any()
}

sim_plot_bayesplot = function(obj, group = NA, xvar = NA, nsim = 5){
  # simulate data from the fitted model
  if(really_has_method("posterior_predict", obj)){
    # use "posterior_predict" if available
    ysim = posterior_predict(obj, nsim = nsim)
  } else{
    # otherwise use "simulate"
    ysim = simulate(obj, nsim = nsim)
    if(is.factor(ysim[,1])){ # fix factor data
      ysim = ysim |> sapply(function(x){as.numeric(x) - 1})
    }
    ysim = ysim |> as.matrix() |> t()
  }

  # get actual data (y)
  y = model.frame(obj)[,1]
  if(is.factor(y)){ # fix factor data
    y = as.numeric(y) - 1
  }

  # select type of post pred plot
  if(is.na(xvar)){
    # discrete data (y)
    if(is_count_data(ysim[1,])){
      if(is.na(group)){
        pt = bayesplot::ppc_bars(y = y, yrep = ysim, prob = 0)
      } else{
        pt = bayesplot::ppc_bars_grouped(y = y, yrep = ysim, group = model.frame(obj)[,group], prob = 0)
      }
    } else{
      # continuous data (y)
      if(is.na(group)){
        pt = bayesplot::ppc_dens_overlay(y = y, yrep = ysim)
      } else{
        pt = bayesplot::ppc_dens_overlay_grouped(y = y, yrep = ysim, group = model.frame(obj)[,group])
      }
    }
  } else{
    if(is.na(group)){
      pt = bayesplot::ppc_intervals(y = y, yrep = ysim[sample(1:nrow(ysim), size = 1),,drop=FALSE], x = model.frame(obj)[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    } else{
      pt = bayesplot::ppc_intervals_grouped(y = y, yrep = ysim[sample(1:nrow(ysim), size = 1),,drop=FALSE], group = model.frame(obj)[,group], x = model.frame(obj)[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    }
  }

  return(pt)
}

sim_plot = function(obj, group = NA, xvar = NA){
  # get real data
  yreal = model.frame(obj)[,1]
  #if(is.factor(yreal)){ # fix factor data
  #  yreal = as.numeric(yreal) - 1
  #}

  # simulate data from the fitted model
  ysim = simulate(obj, nsim = 1)[,1] |> as.vector()
  #if(is.factor(ysim)){ # fix factor data
  #  ysim = as.numeric(ysim) - 1
  #}

  # combine real and simulated data
  plot_data = data.frame(y = c(yreal, ysim),
                         type = rep(c("real", "simulated"), each = length(ysim)))

  # add grouping variable if needed
  if(!is.na(group)){
    plot_data$group = model.frame(obj)[,group]
  }

  # make the plot
  if(is.na(xvar)){ # marginal distribution plot
    if(is_count_data(ysim)){
      plot_obj = plot_data |> ggplot(aes(x = y, color = type)) +
        geom_bar()
    } else{
      plot_obj = plot_data |> ggplot(aes(x = y, color = type)) +
        geom_density()
    }
  } else{ # scatterplot
    plot_data$x = model.frame(obj)[,xvar]
    plot_obj = plot_data |> ggplot(aes(x = x, y = y, color = type)) +
      geom_point()
  }

  # facet by group if needed
  if(!is.na(group)){
    plot_obj = plot_obj + facet_wrap(group)
  }

  return(plot_obj)
}
