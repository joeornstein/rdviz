#' Visualize a Regression Discontinuity
#'
#' @param x The running variable
#' @param y The dependent variable
#' @param c The RD cutoff
#' @param p The polynomial order (NOTE: currently only works for p=1 and p=2)
#' @param kernel The kernel ('triangular' or 'uniform')
#' @param binnedPoints A boolean. Display the binned scatter plot or no?
#' @param xlab The x-axis label
#' @param ylab The y-axis label
#' @return A ggplot object
#' @examples
#' rdviz(x,y)

rdviz <- function(x, y, c=0, p=1, kernel='tri',
                  xlab='X',ylab='Y',
                  binnedPoints = T){

  library(dplyr)
  library(ggplot2)
  library(rdrobust)

  dat <- tibble(X=x,Y=y)

  # Recenter x variables around cutoff
  dat <- dat %>%
    mutate(X = X - c)

  # Get bandwidth, slope, and intercept parameters from 'rdrobust'
  rd <- rdrobust(y = dat$Y, x = dat$X, c=0, p=p, kernel=kernel)

  interceptLeft <- rd$beta_p_l[1,1]
  interceptRight <- rd$beta_p_r[1,1]
  slopeLeft <- rd$beta_p_l[2,1]
  slopeRight <- rd$beta_p_r[2,1]
  if(p==2){
    concavityLeft <- rd$beta_p_l[3,1]
    concavityRight <- rd$beta_p_r[3,1]
    leftFormula <- function(x) interceptLeft + slopeLeft * x + concavityLeft * x^2
    rightFormula <- function(x) interceptRight + slopeRight * x + concavityRight * x^2
  }else{
    leftFormula <- function(x) interceptLeft + slopeLeft * x
    rightFormula <- function(x) interceptRight + slopeRight * x
  }

  h <- rd$bws['h','right']


  # Get optimal binned scatterplot from 'rdplot'
  rdp <- rdplot(y=dat$Y, x=dat$X, hide = T)
  binned_points <- tibble(x =  rdp$vars_bins$rdplot_mean_x,
                          y = rdp$vars_bins$rdplot_mean_y)


  plot <- ggplot() +

    #Raw scatter
    geom_point(data = dat, aes(x=X,y=Y), size = 1, alpha = 0.1) +

    #Add Local Polynomials
    stat_function(data = dat, aes(x=X,y=Y),
                  fun=leftFormula, xlim=c(-h,0),
                  size=1) +
    stat_function(data = dat, aes(x=X,y=Y),
                  fun=rightFormula, xlim=c(0,h),
                  size=1) +

    # Vertical Lines representing cutoff and bandwidths
    geom_vline(xintercept = 0, linetype = 'solid') +
    geom_vline(xintercept = -h, linetype = 'dashed') +
    geom_vline(xintercept = h, linetype = 'dashed') +

    #Theme
    theme_bw() +

    #Axis Labels
    ylab(ylab)

  if(c != 0){
    plot <- plot + xlab(paste0(xlab, ' (centered around cutoff)'))
  }else{
    plot <- plot + xlab(xlab)
  }

  if(binnedPoints){
    return(plot + geom_point(data=binned_points, aes(x=x,y=y), size=2, color='black'))
  }else{
    return(plot)
  }

}
