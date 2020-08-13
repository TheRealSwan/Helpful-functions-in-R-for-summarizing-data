###################################################################
#
# Helpful-functions-in-R-for-summarizing-PKPD-data
#
###################################################################




# n count
n3 = function(x){
  (length(x)) - (sum(is.na(x)))
}

# Geometric mean
gm_mean<-function(x,na.rm=T){
  a <- mean(log(x[x>0]),na.rm=T)
  exp(a)
}

# Geometric CV%
geocv<-function(x, na.rm = TRUE){
  sdlog <- sd(log(x[x > 0]), na.rm = na.rm)
  geosd <- exp(sdlog)
  100*(sqrt(exp(log(geosd)^2)-1))
}

# Replace NA values with medians
med_val<- function(x){
  replace(x, is.na(x), median(x, na.rm = TRUE))
}

# Binwidth equation per Freedman-diaconis rule
bw <-function(x){
  2*IQR(x, na.rm=T)/length(x)^(1/3)
}

# Determine outliers
is_outlier <- function(x, na.rm=T) {
  a<-quantile(x, 0.25,na.rm=T)  - 1.5 * IQR(x,na.rm=T)
  b<-quantile(x, 0.75,na.rm=T) + 1.5 * IQR(x,na.rm=T)
  return(x < a | x > b)
}

# Negate
`%!in%`=Negate(`%in%`)