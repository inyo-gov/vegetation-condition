
# time series plot functions----
plot.dtw <- function(PID, dtw) {

  # Extract the parcel of interest.

  pcldtw <- dtw %>%
    filter(Parcel == PID) %>%
    arrange(Year)
  pcldtw <- pcldtw %>% mutate(phreatic.zone = MIN - 3)
  n <- dim(pcldtw)[1]

  # Find the maximum value that DTW attained over the time span to
  # be plotted.  Allow for missing data.  Set up the plot limits.
  # specifying the max first, then min in ylim sets up the reverse scale we want for DTW plotting where 0 is at top or soil surface representation.

  # if (is.na(pcldtw$DTW)) {
  #   ylim <- c(15, 0)
  # } else {
  #   this.max.DTW <- max(pcldtw$DTW, na.rm = TRUE) + 1
  #   # this.min.DTW <- 0)
  #   ylim <- c(this.max.DTW, 0)
  # }
  this.max.DTW <- max(pcldtw$DTW, na.rm = TRUE) + 1
  this.min.DTW <- min(pcldtw$DTW, na.rm = TRUE) - 1
  # ylim <- c(15,0)
  ylim <- c(this.max.DTW,min(this.min.DTW,0))
  # ylim <- c(this.max.DTW,this.min.DTW)
  plot(xlim, ylim, xlab = "", ylab = "DTW (ft BGS)", xlim = xlim, ylim = ylim, yaxs = "i", type = "n", axes = F) # Draw solid lines at important depth points.

  # could control ylim in global chunk but if so, need to change above plot function and below axis setup
  # add the axes and a frame.
  axis(side = 1, at = seq(xlim[1], xlim[2]), labels = FALSE, tcl = -0.2)
  axis(side = 1, at = pretty(xlim))
  axis(side = 2, at = pretty(ylim), las = 2)
  abline(h = pretty(ylim), col = "lightgray")
  box()


  # abline(h = 5, lwd = 1, col = "blue")
  # text(rmarg, 6, 'D/E', adj=0, xpd=NA)

  bldtw <- pcldtw %>% filter(Year %in% c("1984","1985","1986","1987"))

  # NdviMeanBl <- mean(bl$NDVI_SUR)
  # ndviMean <- mean(rspcl$NDVI_SUR)
  text(rmarg, mean(bldtw$DTW), 'DTW 1984-87 Avg.', adj=0, xpd=NA)
  abline(h=mean(bldtw$DTW), lwd=1, col ='blue')



  # abline(h = 6, lwd = 1, col = "darkgreen")
  # text(rmarg, 7, 'C', adj=0, xpd=NA)
  #
  # abline(h = 12, lwd = 1, col = "lightgreen")
  # text(rmarg, 13, 'B', adj=0, xpd=NA)

  # abline(h = 18, lwd = 1, col = "yellowgreen")
  # text(rmarg, 18, 'A', adj=0, xpd=NA)

  # text(rmarg, stats$Mean[1], 'Baseline', adj=0, xpd=NA)

  # abline(h=Parcel$DTW[1], lwd=1, col='blue')
  # text(rmarg, Parcel$DTW[1], '1985 DTW', adj=0, xpd=NA)


  # Assess reliability and present data only in cases where
  # the data are reliable or relative recovery reliable.
  # if (as.character(Attribute$DTW.Reliability) %in% c('Reliable', 'Relative Recovery Reliable' , 'Baseline Not Reliable', 'Current DTW Not Reliable','Current DTW Reliable','Need hydrograph Evaluation','NoData','Not Reliable','Baseline Reliable','Validation Needed)) {

  # Draw in a lines and points graph.
  lines(pcldtw$Year, pcldtw$DTW, type = "l", pch = 8, col = "blue")
  # lines(pcldtw$Year, pcldtw$DTW - 3, type = "l", pch = 5, col = "lightblue")
  # lines(Parcel$Year, Parcel$DTWcap1, type='l', pch=16, col="lightblue")
  # lines(pcldtw$Year, pcldtw$MIN, type='b', pch=16, col="darkblue")
  # lines(pcldtw$Year, pcldtw$MAX, type='b', pch=16, col="grey")
  # lines(pcldtw$Year, pcldtw$phreatic.zone, type='b', pch=16, col="blue")
  # } else {
  #   # Draw nothing.
  # }
  # Indicate the DTW reliability.
  # text(
  #   xlim[1] + (xlim[2] - xlim[1])*dtw.xy[1],
  #   dtw.ylim[1] * dtw.xy[2],
  #   Attribute$DTW.Reliability, adj=1, family='sans', font=4, cex=1.25
  # )
}


#' Title
#'
#' @param PID
#'
#' @return
#' @export
#'
#' @examples
plot.ndvi <- function(PID, rs) {

  # Extract the parcel of interest.
  #Parcel    <- subset(Covariates, Parcel==PID)
  rspcl <- rs %>% filter(Parcel == PID)


  # Parcel    <- subset(Covariates2, Parcel==PID)
  # Attribute <- subset(Attributes, Parcel==PID)

  # We are only plotting one number per year here.  Get the number of
  # years to plot.

  n <- dim(rspcl)[1]

  # YLIM
  # Check the y-axis limits.  Extend them if necessary.
  ymx <- max(rspcl$NDVI_SUR) + .01
  ymn <- min(rspcl$NDVI_SUR) - .01
  # ymin <- 0
  # ylim <- c(0, .6)
  ylim <- c(ymn, ymx)
  # ylim <- c(0, ymx)
  # ylim <- c(0,.6)

  # if (max(Parcel$NDVI_SUR, na.rm=TRUE) > -1.5) { ylim <- c(ymin, ym) }
  #
  # # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='NDVI', xlim=xlim, ylim=ylim, type='n', yaxs='i', axes=F)

  # Jazz it up with pretty axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="white")
  box()

  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(rspcl$Year[i]-bar.space, 2), rep(rspcl$Year[i]+bar.space, 2))
    bar.y <- c(0, rspcl$NDVI_SUR[i], rspcl$NDVI_SUR[i], 0)

    polygon(bar.x, bar.y, col='darkgreen')
  }

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.
  # Only do this if the data will be analyzed using Dunnett's method.

  bl <- rspcl %>% filter(Year %in% c("1986"))

  # NdviMeanBl <- mean(bl$NDVI_SUR)
  # ndviMean <- mean(rspcl$NDVI_SUR)
  text(rmarg, mean(bl$NDVI_SUR), "1986 NDVI", adj=0, xpd=NA)

  # paste0('Shrub Cover-',stats$Year[1])


  abline(h=mean(bl$NDVI_SUR), lwd=1, col ='darkgreen')

  # text(rmarg, mean(rspcl$NDVI_SUR), 'mean NDVI', adj=0, xpd=NA)
  # abline(h=mean(rspcl$NDVI_SUR), lwd=1, col ='green')

  # text(rmarg, median(rspcl$NDVI_SUR), 'median NDVI', adj=0, xpd=NA)
  # abline(h=median(rspcl$NDVI_SUR), lwd=1, col ='lightgreen')

  # text(rmarg, bl$Mean[1], 'Baseline', adj=0, xpd=NA)
  # abline(h=IQR(rspcl$NDVI_SUR), lwd=1, col ='purple')
  #text(rmarg, Parcel$NDVI_SUR[1], 'Baseline', adj=0, xpd=NA)


}



#' Title
#'
#' @param PID
#'
#' @return
#' @export
#'
#' @examples
plot.ppt <- function(PID, rs) {

  # Extract the parcel of interest.
  #Parcel    <- subset(Covariates, Parcel==PID)
  ppt <- rs %>% filter(Parcel == PID)


  # Parcel    <- subset(Covariates2, Parcel==PID)
  # Attribute <- subset(Attributes, Parcel==PID)

  # We are only plotting one number per year here.  Get the number of
  # years to plot.

  n <- dim(ppt)[1]

  # HARD-CODED NUMBER:
  # Check the y-axis limits.  Extend them if necessary.

  ymxppt <- max(ppt$PPT) + 10
  ymnppt <- min(ppt$PPT) - 10

  # ylims are defined in setup chunk
  ylim <- c(ymnppt,ymxppt)
  # ylim <- c(0,400)


  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Precip. (mm)', xlim=xlim, ylim= ylim, type='n', yaxs='i', axes=F)

  # Jazz it up with pretty axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="white")
  box()

  # abline(h=mean(ppt$PPT), lwd=1, col ='darkblue')
  # Draw in the bars for each row (i.e., each year).

  blppt <- ppt %>% filter(Year %in% c("1986"))

  # NdviMeanBl <- mean(bl$NDVI_SUR)
  # ndviMean <- mean(rspcl$NDVI_SUR)
  text(rmarg, mean(blppt$PPT), '1986 PPT', adj=0, xpd=NA)
  abline(h=mean(blppt$PPT), lwd=1, col ='darkblue')





  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(ppt$Year[i]-bar.space, 2), rep(ppt$Year[i]+bar.space, 2))
    bar.y <- c(0, ppt$PPT[i], ppt$PPT[i], 0)

    polygon(bar.x, bar.y, col='darkblue')
  }
  # abline(h=Parcel$PPT[1], lwd=1,col='blue')

  #text(rmarg, Parcel$PPT[1], 'Baseline', adj=0, xpd=NA)
}


# library(ggplot2)
# library(dplyr)

plot.perennial.shrubcover.gg <- function(PID, transects) {
  # (Previous code...)
  stderr <- function(x) { return(sqrt(var(x)/length(x))) }

  # Extract the parcel of interest.

  Transect <- subset(transects, Parcel==PID)
  # ... (Previous code...)

  # Assuming `Transect` is your dataset

  # ... (Previous code...)

  # Calculate mean and standard error of the mean for each year.
  Shrub.mean <- Transect %>% group_by(Year) %>% dplyr::summarise(Mean = mean(Shrub))
  Shrub.stderr <- Transect %>% group_by(Year) %>% dplyr::summarise(SEM = stderr(Shrub))

  stats <- Shrub.mean %>% left_join(Shrub.stderr, by = "Year") %>% arrange(Year)

  # Creating a dataframe for comparison with the baseline years (1984-1987)
  baseline_years <- filter(stats, Year >= 1984 & Year <= 1987)
  baseline_mean <- mean(baseline_years$Mean)

  # Comparing each year to the baseline and adding significance information
  stats$Significant <- ifelse(stats$Year >= 1988, t.test(stats$Mean, mu = baseline_mean, alternative = "less")$p.value < 0.05, NA)

  # Plotting using ggplot2
  gg <- ggplot(stats, aes(x = factor(Year), y = Mean)) +
    geom_bar(stat = "identity", fill = "brown", width = 0.5) +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2, color = "black") +
    labs(x = "Year", y = "Shrub Cover [%]") +
    theme_minimal()

  gg <- gg +
    geom_text(data = subset(stats, Year >= 1988 & Significant == TRUE),
              aes(label = "**"), vjust = -0.5, color = "red", size = 5)  # Adjust vjust and size as needed

  print(gg)
  # ... (Remaining code...)
}



# plot.perennial.cover()
#
# Function to create the perennial cover plot.

#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.shrubcover <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }

  # Extract the parcel of interest.

  Transect <- subset(transects, Parcel==PID)
  Shrub.mean <- Transect %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Shrub))
  Shrub.stderr <- Transect  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Shrub))
  stats <- Shrub.mean%>%left_join(Shrub.stderr, by="Year") %>% arrange(Year)
  n <- dim(stats)[1]

  # Check the y-axis limits.  Extend them if necessary.
  this.max.shrub <- max(stats$Mean, na.rm=TRUE)+3
  this.min.shrub <- min(stats$Mean, na.rm=TRUE)-2
  ylim <- c(-1, this.max.shrub)
  plot(xlim, ylim, xlab='', ylab='Shrub Cover [%]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.
  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="white")
  box()

  abline(h=stats$Mean[1], lwd=1,col='brown')
  text(rmarg, stats$Mean[1], paste0('Shrub Cover-',stats$Year[1]), adj=0, xpd=NA)

  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='brown')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-2*(stats$SEM[i]), stats$Mean[i]+2*(stats$SEM[i])))#bar vertical
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-2*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+2*(stats$SEM[i]),2))
  }


  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  for (i in 2:n) {

    data1 <- Transect$Shrub[Transect$Year < 1989]
    data2 <- Transect$Shrub[Transect$Year==stats$Year[i]]#all other years in time series

    # per the greenbook update in 2017, I use the two sample t-test for parcels with greater than four transects and non-zero variance; and the one sample t-test with mu = mean of small sample or the live cover value reduced to perennial only from the wvcom variable.
    if (all(data1 == 0, na.rm = TRUE)) {
      # All baseline data is zero, so one-sample test vs. mu = 0
      fit <- t.test(data2, mu = 0, alternative = "greater")
    } else if (length(data1) <= 4) {
      # Very few baseline points; treat baseline mean as mu
      mu.d1 <- mean(data1, na.rm = TRUE)
      fit <- t.test(data2, mu = mu.d1, alternative = "greater")
    } else {
      # Otherwise do a two-sample test
      fit <- t.test(data1, data2, alternative = "greater")
    }


  }
  pvalues[i] <- fit$p.value

  # Add an asterisk according to whether the comparison with
  # baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      # text(stats$Year[i], stats$Mean[i]+2.0*(stats$SEM[i])+ asterisk.offset, '*', adj=.6)
      text(stats$Year[i], stats$Mean[i]+2.0+ asterisk.offset, '**', col = "red", adj=.6)
    }

    # text(stats$Year[i], stats$Mean[i]+2.0+ asterisk.offset, '**', col = "red", adj=.6)

  }

}


# plot.perennial.cover()
#
# Function to create the perennial cover plot.

#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.cover <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }

  # Extract the parcel of interest.
  Transect <- subset(transects, Parcel==PID)
  Cover.mean <- Transect %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Cover))
  Cover.stderr <- Transect  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Cover))
  stats <- Cover.mean%>%left_join(Cover.stderr, by="Year") %>% arrange(Year)
  n <- dim(stats)[1]

  # y-axis limits.
  this.max.cover <- max(stats$Mean, na.rm=TRUE)+3
  this.min.cover <- min(stats$Mean, na.rm=TRUE)-2

  ylim <- c(this.min.cover, this.max.cover)

  # Set up a blank plot first.
  plot(xlim, ylim, xlab='', ylab='Perennial Cover (0-100%)', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.
  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="white")
  box()

  # If there is a baseline, add a horizontal line for the baseline.
  abline(h=stats$Mean[1], lwd=1,col='darkgreen')


  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='green')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 95% CI or 1.96 * standard error of the mean using the usual graphics.

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1.96*(stats$SEM[i]), stats$Mean[i]+1.96*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1.96*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1.96*(stats$SEM[i]),2))

  }

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  for (i in 2:n) {

    data1 <- Transect$Cover[Transect$Year < 1989]
    mu.d1<-mean(data1)
    data2 <- Transect$Cover[Transect$Year==stats$Year[i]]

    ifelse (length(data1) <= 4,

            fit <- t.test(data2, mu = mu.d1),
            fit <- t.test(data1,data2)
    )

    pvalues[i] <- fit$p.value
  }

  # Add an asterisk according to whether the comparison with baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      text(stats$Year[i], stats$Mean[i]+2.0*(stats$SEM[i])+ asterisk.offset, '**', adj=.6)
    }
  }

}



# plot.perennial.grass()
# Function to create the perennial cover plot.
# PID<-"BLK044"
#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.grass <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }
  # Extract the parcel of interest.

  grass.pcl <- subset(transects, Parcel==PID)

  Grass.mean <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Grass))
  Grass.stderr <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Grass))
  stats <- Grass.mean%>%left_join(Grass.stderr, by="Year") %>% arrange(Year)

  # Get the number of years to plot.
  n <- dim(stats)[1]
  this.max.grass <- max(stats$Mean, na.rm=TRUE)+2
  this.min.grass <- min(stats$Mean, na.rm=TRUE)-1
  ylim <- c(-1, this.max.grass)

  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Grass Cover', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.
  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="white")
  box()

  abline(h=stats$Mean[1], lwd=1,col='darkgreen')
  text(rmarg, stats$Mean[1], paste0('Grass Cover-',stats$Year[1]), adj=0, xpd=NA)

  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='darkgreen')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 2 x standard error of the mean using the usual graphics.

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-2*(stats$SEM[i]), stats$Mean[i]+ 2*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-2*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+2*(stats$SEM[i]),2))

  }

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.
  pvalues <- rep(NA, n)

  # consider list (vector) containing the years to be analyzed.
  for (i in 2:n) {#start at 2 because 1 is the baseline year and this for loop is only for each comparison to baseline

    data1 <- grass.pcl$Grass[grass.pcl$Year < 1989]
    data2 <- grass.pcl$Grass[grass.pcl$Year == stats$Year[i]]

    # mu.d1<-mean(data1)

    # If *all* baseline values are zero, do a one-sample test with mu=0.
    # Otherwise, decide between one-sample vs two-sample based on your logic.
    if (all(data1 == 0, na.rm = TRUE)) {
      # All baseline data is zero, so one-sample test vs. mu = 0
      fit <- t.test(data2, mu = 0, alternative = "greater")
    } else if (length(data1) <= 4) {
      # Very few baseline points; treat baseline mean as mu
      mu.d1 <- mean(data1, na.rm = TRUE)
      fit <- t.test(data2, mu = mu.d1, alternative = "greater")
    } else {
      # Otherwise do a two-sample test
      fit <- t.test(data1, data2, alternative = "greater")
    }

    pvalues[i] <- fit$p.value
  }

  for (i in 1:n) {

      if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
        text(stats$Year[i], stats$Mean[i]+2.0+ asterisk.offset, '**', col = "red", adj=.6)
    }
  }
}


# assimilate 5 timeseries----

five_row_timeseries <- function(attributes_pfix, transects, dtw_pfix, rs_pfix, cYear){

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }


  # Set global configurations.

  r.squared.digits <- 2  ### Round the r-squared values to this many digits.
  p.value.digits   <- 4  ### Round the p-values to this many digits
  b.digits         <- 4  ### Round the estimates to this many digits



  # Configure global plot tuning parameters.

  bar.space <- 0.25        ### The space between each bar on the graphs.

  # can define year here for x-axis if doing one off pdf export
  xlim <- c(1985, 2020)   ### The x-axis limits for the graphing
  #xlim <- c(1985, cYear)   ### The x-axis limits for the graphing effort.
  ylim <- c(0, 62.4)       ### The y-axis limits for percentage cover.
  ylim.ndvi<-c(.1,.8)
  dtw.max <- 5             ### Maximum y-axis limits for the DTW measurements.
  dtw.split <- 6           ### Where to switch the y-axis for the DTW measurements.
  dtw.xy <- c(.95, .90)    ### DTW plot annotation location.
  asterisk.offset <- 4     ### How much to space the asterisks.
  rmarg <- cYear + 1.5     ### Tune stuff in the right margin.
  show.caption <- TRUE     ### Print a figure caption or not?

# AttributesPID <- attributes_pfix %>% filter(reinv == "r", Parcel %in% c("BLK094","BLK099"))

  # filter(!Parcel %in% c('BGP013','BLK006',"BGP204", "BGP205", "BLK008", "FSL179", "IND086", "LAW076", "LAW110","MAN038", "PLC113", "PLC220", "TIN006", "UNW074"))

PIDs <- unique(AttributesPID$Parcel)
# all.pid <- PIDs

# layout = "l-page",
# FILEpdf <- paste0(dir, '/output/TimeSeries2018.all.pdf')
FILEpdf <- paste0("output/ts5stack_",cYear,".pdf")
# library(plyr)
fig.num <- 1

pdf(FILEpdf)# opens the dev env
# png(FILEpng)
par(mfrow=c(5,1), oma=c(5,0,5,0), plt=c(.1,.85,.1,.85)) # set up the position of elements

# call functions
for (PID in PIDs) {
  # call functions
  plot.perennial.cover(PID, transects)
  plot.perennial.grass(PID, transects)
  plot.dtw(PID, dtw_pfix)
  plot.ndvi(PID, rs_pfix)
  plot.ppt(PID, rs_pfix)

  # caption variables
  # Parcel <- parcels %>% filter(Parcel==PID)

  Attribute3 <- attributes_pfix %>% filter(Parcel==PID)
  Transect.n <- transects %>% filter(Parcel==PID,Year==cYear)
  n.tran <-  Transect.n %>% group_by(Parcel) %>% dplyr::summarise(count = n()) # doing count summaries for e

  Descriptor <- paste(PID,'(W/C): ',Attribute3$Type,'| Type: ', Attribute3$GB_TYPE,'|', Attribute3$Holland, sep=' ')
  ESD <- paste(Attribute3$taxorder, Attribute3$compname,  '| ESD: ',Attribute3$Ecologic_3, sep=' ')
  geom <- paste('Geomorphic:',Attribute3$geomdesc)
  # last.year <- max(Parcel$Year, na.rm=TRUE)
  test.type <- Attribute3$test_type
  Caption <- paste('Figure ', fig.num, ': ', test.type,': Baseline (',Attribute3$bl.origin,') vs. reinventory (* p < 0.05).\n Baseline sample size (n = ',Attribute3$n86,'). Current year sample size (n = ',n.tran$count,').', ' Error bars = 95% CI.', sep='')
  mtext(side=3, outer=TRUE, line=0, geom)
  mtext(side=3, outer=TRUE, line=3, Descriptor)
  mtext(side=3, outer=TRUE, line=1.5, ESD)
  mtext(side=1, outer=TRUE, line=3, adj=0.15, family='serif', Caption)
  # update fig.num for next plot
  fig.num <- fig.num + 1
  }
dev.off()

return(FILEpdf)
}
