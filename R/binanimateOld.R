library(gganimate)
library(tidyverse)

make_df <- function(n) {
  a1 <- data.frame(x = 0:n,
                  size = n,
                  prob = dbinom(0:n, n, .5),
                  ways = dbinom(0:n, n, .5)*2^n,
                  stage = n,
                  group = 1:(n+1),
                  fill = "n")

  a2 <- a1 %>% mutate(stage = n + .5, fill = "last flip tails")

  b <- data.frame(x = (0:n) + 1,
                  size = n,
                  prob = dbinom(0:n, n, .5),
                  ways = dbinom(0:n, n, .5)*2^n,
                  stage = n + .5,
                  group = 1:(n+1),
                  fill = "last flip heads")

  rbind(a1, a2, b)
}


df <- purrr::map_df(1:10, make_df)

g <- ggplot(df, aes(x, prob, fill = fill , group = group)) + geom_col() + scale_fill_manual(values = c("purple", "red", "blue"))

anim <- g + transition_states(stage) + exit_recolor(fill = "purple")

animate(anim)




# tweeted

# g <- ggplot(df, aes(x, prob, fill = size)) + geom_col() + scale_fill_viridis_c() + ggtitle("Binomial Distribution")

# anim <- g + transition_states(size) + enter_grow() + shadow_wake(wake_length = .1)

# teal #2B9089

# Version 2

# doesn't jump to double prob

make_df <- function(n) {
  a1 <- data.frame(x = 0:n,
                   size = n,
                   prob = dbinom(0:n, n, .5),
                   ways = dbinom(0:n, n, .5)*2^n,
                   stage = n,
                   group = 1:(n+1),
                   fill = "n")

  a2 <- a1 %>% mutate(stage = n + .5, fill = "last flip tails", prob = prob /2)

  b <- data.frame(x = (0:n) + 1,
                  size = n,
                  prob = dbinom(0:n, n, .5)/2,
                  ways = dbinom(0:n, n, .5)*2^n,
                  stage = n + .5,
                  group = 1:(n+1),
                  fill = "last flip heads")

  rbind(a1, a2, b)
}


df <- purrr::map_df(1:10, make_df)

g <- ggplot(df, aes(x, prob, fill = fill , group = group)) + geom_col() + scale_fill_manual(values = c("purple", "red", "blue"))

anim <- g + transition_states(stage) + exit_recolor(fill = "purple")

animate(anim)
