b.star <- np::b.star
jackknife <- bootstrap::jackknife
density <- stats::density


stationary_bootstrap <- function(Yobs, nboot=2000){
  bstar <- b.star(Yobs)
  p <- 1/(bstar[1])
  l <- length(Yobs)
  ind_matr <- matrix(0,nrow=l,ncol=nboot)
  ind_matr[1,] <- sample(1:l,nboot,replace=TRUE)
  sel <- matrix(runif(nboot*l,0.0,1.0)<p,l,nboot)
  ind_matr[sel] <- sample(1:l,sum(sel),replace=TRUE)
  for (i in 2:l){
    ind_matr[i,!ind_matr[i,]] <- ind_matr[i-1,!ind_matr[i,]] + 1
    ind_matr[i,ind_matr[i,]== (l+1)] <- ind_matr[1,ind_matr[i,]== (l+1)]
  }
  return(ind_matr)
}



selective_rmse <- function(x,xdata){
  return(sqrt(mean((xdata[x,1]-xdata[x,2])^2)))
}

selective_nse <- function(x,xdata){
  obs=xdata[x,1];
  pred=xdata[x,2];
  l <- length(obs);
  rmse_x = selective_rmse(x,xdata)
  return(1-rmse_x^2/((sd(obs)^2)*(l-1)/l))
}

rmse <- function(x){
  return(selective_rmse(1:nrow(x),x))
}

nse <- function(x){
  return(selective_nse(1:nrow(x),x))
}

fz0 <- function(bootstrap_stat,stat){
  return(qnorm(mean(bootstrap_stat<stat) + mean(bootstrap_stat==stat)))
}


bca <- function(data,data_stat,selective_function,bootstraps_stat,alpha){
  l <- nrow(data)
  jacknife_out <- jackknife(1:l,selective_function,data)
  mean_jvals <- mean(jacknife_out$jack.values)
  score <- mean_jvals - jacknife_out$jack.values
  if(all(score==0)){skew<-0}else{skew <- mean(score^3)/(sqrt(l)*(mean(score^2))^1.5)}
  acc <- skew/6
  z0 <- fz0(bootstraps_stat,data_stat)
  z_alpha1 <- qnorm(alpha/2)
  z_alpha2 <- -z_alpha1
  if(z0==Inf) {
    pct1 <- 1
    pct2 <- 1
  } else if (z0==-Inf) {
    pct1 <- 0
    pct2 <- 0
  } else {
    pct1 <- pnorm(z0 +(z0+z_alpha1)/(1-acc*(z0+z_alpha1)))
    pct2 <- pnorm(z0 +(z0+z_alpha2)/(1-acc*(z0+z_alpha2)))
  }
  ci <- quantile(bootstraps_stat,probs=c(pct1,pct2))
  return(ci)
}

evaluate_ci <- function(ci,classlim){
  if (ci[1]<classlim[1] & ci[2]<classlim[1]) {
    ci_eval <- 'Unsatisfactory'
  } else if (ci[1]>=classlim[1] & ci[2]<classlim[2]) {
    ci_eval <- 'Acceptable'
  } else if (ci[1]>=classlim[2] & ci[2]<classlim[3]) {
     ci_eval <- 'Good'
  } else if (ci[1]>=classlim[3] & ci[2]>=classlim[3]) {
     ci_eval <- 'Very Good'
  } else{
    if (ci[1]<classlim[1]) {
      t1 <- 'Unsatisfactory'
    } else if (ci[1]<classlim[2]) {
      t1 <- 'Acceptable'
    } else{
      t1 <- 'Good'
    } 
    if (ci[2] >= classlim[3]) {
      t2 <- 'Very Good'
    } else if (ci[2] >= classlim[2]) {
      t2 <- 'Good'
    } else{
      t2 <- 'Acceptable'
    }
    ci_eval <- paste('From',t1,'to',t2,sep=' ')
  }
  return(ci_eval)
}



evaluate_bias <- function(Yobs,Ypred,BiasValue){
  min_data <- min(c(Yobs,Ypred))
  if(min_data<0){
    SHFYP = Ypred - min_data
    SHFYO = Yobs - min_data
  } else {
    SHFYP=Yprd
    SHFYO=Yobs
  }
  mSHFYO <- mean(SHFYO)
  if (mSHFYO==0){
    rel_bias = 100*mean(SHFYP-SHFYO)
  }else{
    rel_bias = 100*mean(SHFYP-SHFYO)/mSHFYO
  }
  bias_text<-list()
  if(rel_bias < (-1*BiasValue)){
    bias_text[1] <- paste('Model Bias: Underprediction by ',round(rel_bias,2), '% of the mean',sep='')
    bias_text[2] <- 'NSE may be influenced by model bias'
    if (rel_bias < -500) {
      bias_text[1] <- 'Model Bias: Underprediction by <-500% of the mean'
    }
  } else if (rel_bias > BiasValue) {
    bias_text[1] <- paste('Model Bias: Overprediction by ',round(rel_bias,2), '% of the mean', sep='')
    bias_text[2] <- 'NSE may be influenced by model bias'
    if (rel_bias > 500) {
      bias_text[1] <- 'Model Bias: Overprediction by >500% of the mean'
    }
  } else {
    bias_text[1] <- 'Model Bias: No'
    bias_text[2] <- ' '
  }
  return(bias_text)
}