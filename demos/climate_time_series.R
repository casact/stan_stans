library(tidyverse)
library(aci)
library(rethinking)
library(splines)

tbl_in <- aci::read_all_monthly("data/ACI_v1.1_Values_Through_May_2020_in_English.xlsx")

tbl_sea <- tbl_in %>% 
  filter(region == "SEA")

tbl_sea %>% 
  filter(smoothed) %>% 
  ggplot(aes(date, sea_level)) + 
  geom_point()

tbl_sea %>% 
  filter(!smoothed) %>% 
  ggplot(aes(date, sea_level)) + 
  geom_line()

tbl_sea_raw <- tbl_sea %>% 
  filter(!smoothed) %>%
  mutate(date_dbl = as.double(date))
  arrange(date)

num_knots <- 50
knot_list <- quantile(
  tbl_sea_raw$date_dbl
  , probs = seq(0, 1, length.out = num_knots)
)

B = bs(
  tbl_sea_raw$date_dbl
  , knots = knot_list %>% head(-1) %>% tail(-1)
  , degree = 3
  , intercept = TRUE
)

plot(
  NULL
  , xlim = range(tbl_sea_raw$date_dbl)
  , ylim = c(0,1)
)

for (i in 1:ncol(B)) lines(tbl_sea_raw$date, B[, i])

mdl_sea_level <- quap(
  alist(
    sea_level ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(0.5, 0.5),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  )
  , data = list(sea_level = tbl_sea_raw$sea_level, B=B)
  , start = list(w = rep(0, ncol(B)))
)

post <- extract.samples(mdl_sea_level)
w <- apply(post$w, 2, mean)
plot(
  NULL
  , xlim = range(tbl_sea_raw$date_dbl)
  , ylim = c(-2,2)
  , xlab = "year"
  , ylab = "basis * weight"
)

for (i in seq_len(ncol(B))) lines(tbl_sea_raw$date_dbl, w[i] * B[, i])

mu <- link(mdl_sea_level)
mu_PI <- apply(mu, 2, PI, 0.97)
plot(
  tbl_sea_raw$date_dbl
  , tbl_sea_raw$sea_level
  , col = col.alpha(rangi2, 0.3)
  , pch = 16
  , type = "l"
)
shade(
  mu_PI
  , tbl_sea_raw$date_dbl
  , col = col.