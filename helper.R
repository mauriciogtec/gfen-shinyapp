times_chr = function(hours) {
  widx = (hours - 1) %/% 24 + 1
  t = (hours - 1) %% 24
  wdays = c("Sun", "Mon", "Tue",
            "Wed", "Thu", "Fri", "Sat")
  sprintf("%s %02d:00", wdays[widx], t)
}


time_slice = function(t, space_size) {
  from = (t - 1) * 168 + 1
  to = from + space_size - 1
  return (from:to)
}


# spt_indexer <- function(n) {
#   n # int number of nodes
#   function(i, t) i + (t - 1) * n
# }
# 
# Rcpp::cppFunction("
#   double moment(NumericVector x, NumericVector dens, int k) {
#     int n = dens.size();
#     double out = 0.5 * ((x[1] - x[0]) * std::pow(x[0], k) * dens[0] +
#       (x[n - 1] - x[n - 2]) * std::pow(x[0], k) * dens[n - 1]);
#     #pragma simd
#     for (int i = 1; i < n - 1; i++) 
#       out += (x[i + 1] - x[i]) * std::pow(x[i], k) * dens[i];
#     return out;
#   }
# ")
# 
# Rcpp::cppFunction("
#   double series_quantile(NumericVector x, NumericVector dens, double q) {
#     double S = 0.0;
#     int i;
#     for (i=1; i < dens.size(); i++) {
#         S += 0.5 * (dens[i] + dens[i - 1]) * (x[i] - x[i - 1]);
#         if (S >= q)
#           break;
#     }
#     return x[i - 1];
#   }
# ")
# 
# Rcpp::cppFunction("
#   double integrate(NumericVector x, NumericVector dens, double lower, double upper) {
#     double S = 0.0;
#     int i;
#     for (i=1; i < dens.size(); i++) {
#         if (x[i - 1] >= lower) 
#           S += 0.5 * (dens[i] + dens[i - 1]) * (x[i] - x[i - 1]);
#         if (x[i] > upper)
#           break;
#     }
#     return S;
#   }
# ")
# 
# series_mean <- function(x, vals) moment(x, vals, 1)
# series_var <- function(x, vals) moment(x, vals, 2) - series_mean(x, vals)^2
# series_sd <- function(x, vals) sqrt(series_var(x, vals))
# 
# # timelabels <- function(
# #     times = 1:168,
# #     time0=lubridate::ymd_h("2018-11-03 19")) {
# #   asdates <- time0 + lubridate::hours(times - 1)
# #   times_chr <- paste(as.character(lubridate::wday(asdates, label=TRUE)),
# #                      sprintf("%02d:00", lubridate::hour(asdates)))
# #   times_chr  
# # }

# 
# dsmoother <- function(d, 
#   x=seq(0, 1, length.out=length(d)), 
#   m=length(d)-1, 
#   lower=min(x), 
#   upper=max(x)) 
# {
#   N <- length(x)
#   
#   # integrate with trapezoid rule
#   Fn <- numeric(N)
#   Fn[1] <- d[1] * (x[2] - x[1])
#   Fn[2:N] <- 0.5 * (d[1:(N-1)] + d[2:N]) * diff(x)
#   Fn <- cumsum(Fn) / sum(Fn)
#   
#   # Bernstein polynomial interpolation
#   bb <- Bernstein_basis(numeric_var("x", support = c(lower, upper)),
#                         order = m, ui = "increasing")
#   Fnseq <- Fn[round(seq(1, N, length.out= m + 1))]
#   Fhat <- drop(bb(x) %*% Fnseq)
#   
#   # Numerical differentiation
#   dhat <- numeric(N)
#   dhat[1] <- (Fhat[2] - Fhat[1]) / (x[2] - x[1])
#   dhat[N] <- (Fhat[N - 1] - Fhat[N]) / (x[N - 1] - x[N])
#   dhat[2:(N - 1)] <- 0.5 * diff(Fhat[2:N]) / diff(x[2:N]) + 0.5 * diff(Fhat[1:(N-1)]) / diff(x[1:(N-1)])
#   
#   dhat
# }