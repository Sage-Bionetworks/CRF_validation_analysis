#### Code for testing if filter coeffs works
library(tidyverse)

## Data
hr.data <- mhealthtools::heartrate_data
sampling_rate_round <- mhealthtools:::get_sampling_rate(hr.data) %>% 
  round()

## Actual filter co-effs / (same as the filter params in file)
bf_low <- signal::butter(7, 5/(sampling_rate_round/2), type = 'low')
b <- bf_low$b # b = (b1, b2,....., b8)
a <- bf_low$a # a = (a1, a2,....., a8)

## Sample signal from data
x <- hr.data$red[1100:1400]
plot(x, type = 'l')

## Actual filter output
xf_signal <- signal::filter(bf_low,x)

########################################################################
## Our implementation of filter
## REF: https://www.mathworks.com/help/signal/ug/filter-implementation-and-analysis.html
##      (Look at Section titled "Filtering with the filter Function")
########################################################################
our_filter <- function(x, b, a){

  xl <- length(x)
  # Length of x
  
  y <- rep(0, xl) 
  # Create a sequence of 0s of the same length as x
  
  # For index i, less than the length of the filter we have
  # a[1] = 1, always, as the filter coeffs are normalized
  y[1] <- b[1]*x[1]
  y[2] <- b[1]*x[2] + b[2]*x[1] - a[2]*y[1]
  y[3] <- b[1]*x[3] + b[2]*x[2] + b[3]*x[1] - a[2]*y[2] - a[3]*y[1]
  y[4] <- b[1]*x[4] + b[2]*x[3] + b[3]*x[2] + b[4]*x[1] - a[2]*y[3] - a[3]*y[2] - a[4]*y[1]
  y[5] <- b[1]*x[5] + b[2]*x[4] + b[3]*x[3] + b[4]*x[2] + b[5]*x[1] - a[2]*y[4] - a[3]*y[3] -
    a[4]*y[2] - a[5]*y[1]
  y[6] <- b[1]*x[6] + b[2]*x[5] + b[3]*x[4] + b[4]*x[3] + b[5]*x[2] + b[6]*x[1] - a[2]*y[5] -
    a[3]*y[4] - a[4]*y[3] - a[5]*y[2] - a[6]*y[1]
  y[7] <- b[1]*x[7] + b[2]*x[6] + b[3]*x[5] + b[4]*x[4] + b[5]*x[3] + b[6]*x[2] + b[7]*x[1] -
    a[2]*y[6] - a[3]*y[5] - a[4]*y[4] - a[5]*y[3] - a[6]*y[2] - a[7]*y[1]
  
  # For index i, greater than or equal to the length of the filter, we have
  for(i in seq(8,length(x))){
    y[i] <- b[1]*x[i] + b[2]*x[i-1] + b[3]*x[i-2] + b[4]*x[i-3] +
      b[5]*x[i-4] + b[6]*x[i-5] + b[7]*x[i-6] + b[8]*x[i-7] -
      a[2]*y[i-1] - a[3]*y[i-2] - a[4]*y[i-3] - a[5]*y[i-4] -
      a[6]*y[i-5] - a[7]*y[i-6] - a[8]*y[i-7]
  }
  
  return(y)
  # return output
}

## Our output
xf_our<-our_filter(x, b, a)
plot(xf_our, type = 'l')

## Checking that our implementation matches with the original implementation
all.equal(as.numeric(xf_our), as.numeric(xf_signal))
