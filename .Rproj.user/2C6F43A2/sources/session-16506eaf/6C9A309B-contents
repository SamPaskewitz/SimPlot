is_count_data = function(y){
  y_unique = unique(y) |> sort()
  integers_to_check = 0:(length(y_unique) - 1)
  matches = all(y_unique %in% integers_to_check)
  return(matches)
}

really_has_method = function(mth, obj){
  sapply(class(obj), function(cl){!is.null(utils::getS3method(mth, cl, optional = TRUE))}) |> any()
}

sim_plot = function(obj, group = NA, xvar = NA, nsim = 5){
  # simulate data from the fitted model
  if(really_has_method("posterior_predict", obj)){
    # use "posterior_predict" if available
    ysim = posterior_predict(obj, nsim = nsim)
  } else{
    # otherwise use "simulate"
    ysim = simulate(obj, nsim = nsim) |> as.matrix() |> t()
  }

  # fix up y if it's binary (so bayesplot won't complain)
  if(obj$y |> is.factor()){
    y = obj$y |> as.numeric() - 1
  } else{
    y = obj$y
  }

  # select type of post pred plot
  if(is.na(xvar)){
    # discrete data (y)
    if(is_count_data(ysim[1,])){
      if(is.na(group)){
        pt = bayesplot::ppc_bars(y = y, yrep = ysim, prob = 0)
      } else{
        pt = bayesplot::ppc_bars_grouped(y = y, yrep = ysim, group = obj$data[,group], prob = 0)
      }
    } else{
      # continuous data (y)
      if(is.na(group)){
        pt = bayesplot::ppc_dens_overlay(y = y, yrep = ysim)
      } else{
        pt = bayesplot::ppc_dens_overlay_grouped(y = y, yrep = ysim, group = obj$data[,group])
      }
    }
  } else{
    if(is.na(group)){
      pt = bayesplot::ppc_intervals(y = y, yrep = ysim[sample(1:nrow(ysim), size = 1),,drop=FALSE], x = obj$data[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    } else{
      pt = bayesplot::ppc_intervals_grouped(y = y, yrep = ysim[sample(1:nrow(ysim), size = 1),,drop=FALSE], group = obj$data[,group], x = obj$data[,xvar], prob = 0.001, prob_outer = 0.001) + ggplot2::xlab(xvar)
    }
  }
  return(pt)
}
