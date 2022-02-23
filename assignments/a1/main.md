Describe and motivate two possible verbal model of how the Matching
Pennies Task is solved and provide an algorithmic formalization (in R).
Set them to play against randomAgent and WSLSAgent (120 trials, 100
agents). Visualize and comment the simulation.

The algorithmic formalization fo the agent strategies are in
agent\_strategies.R, and the functions for having the agents play
against each other are in games.R.

Random Agent
------------

``` r
trials <- 125
d2 <- NULL

for (noise in seq(0,0.5,0.1)){
  for (rate in seq(0,1,0.1)){
    randomChoice <- rep(NA, trials)
    
    for (t in seq(trials)){
      randomChoice[t] <- RandomAgent(rate, noise)
    }
    
    temp <- tibble(
      trial = seq(trials),
      choice = randomChoice,
      rate,
      noise
    )
    if (is.null(d2)){
      d2 <- temp
    } else {
      d2 <- rbind(d2,temp)
    }
  }
}
```

``` r
d2 <- d2 %>% group_by(rate, noise) %>% 
  mutate(
    cumulative = cumsum(choice) /seq_along(choice)
  )

ggplot(d2, aes(trial, cumulative, group=rate, color=rate)) +
  geom_line() + 
  geom_hline(yintercept=0.5, linetype='dashed') +
  ylim(0,1) + 
  facet_wrap(.~noise) + theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-2-1.png)

RandomAgent (mismatcher) against memoryAgent (matcher)
------------------------------------------------------

One round

``` r
trials <- 125
rate <- 0.5
noise <- 0.2
mem_constraint <- 5

df <- RandomAgainstMemory(trials, rate, noise, mem_constraint)
```

``` r
ggplot(df) + 
  geom_line(color='red', aes(trial, matcher)) +  # matcher
  geom_line(color='blue', aes(trial, mismatcher)) +  # mismatcher
  theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(df) + 
  geom_line(color='red', aes(trial, cumulativeMatcher)) +  # matcher
  geom_line(color='blue', aes(trial, cumulativeMismatcher)) +  # mismatcher
  theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-5-1.png) 100 rounds
with different parameter values

``` r
# j <- 0
# for (i in 1:100){  # number of repetions
#   for(r in seq(0,1,0.2)){  # rate
#     for (n in seq(0,0.5,0.2)){  # noise
#       tmp <- RandomAgainstMemory(
#         trials = 120,
#         rate = r,
#         noise = n,
#         mem_constraint= 120
#       )
# 
#       tmp$rate <- r
#       tmp$noise <- n
#       tmp$mem_constraint <- m
#       tmp$rep <- i
# 
#       if (i ==1){ # first round
#         df <- tmp
#       } else {
#         df <- rbind(df, tmp)
#       }
# 
#       # print statement
#       if (i != j){
#         print(paste('Round:', i))
#       }
#       j <- i
#     }
#   }
# }
# 
# write_csv(df, "RandomAgainstMemory.csv")
```

``` r
df <- read_csv('RandomAgainstMemory.csv')
```

    ## Rows: 213960 Columns: 10

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (10): matcher, mismatcher, trial, Feedback, cumulativeMatcher, cumulativ...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df %>% group_by(rate, mem_constraint, noise) %>% 
  summarize(
    mean(Feedback)
  )
```

    ## `summarise()` has grouped output by 'rate', 'mem_constraint'. You can override using the `.groups` argument.

    ## # A tibble: 18 x 4
    ## # Groups:   rate, mem_constraint [6]
    ##     rate mem_constraint noise `mean(Feedback)`
    ##    <dbl>          <dbl> <dbl>            <dbl>
    ##  1   0              125   0              0.996
    ##  2   0              125   0.2            0.821
    ##  3   0              125   0.4            0.678
    ##  4   0.2            125   0              0.789
    ##  5   0.2            125   0.2            0.677
    ##  6   0.2            125   0.4            0.599
    ##  7   0.4            125   0              0.585
    ##  8   0.4            125   0.2            0.551
    ##  9   0.4            125   0.4            0.526
    ## 10   0.6            125   0              0.581
    ## 11   0.6            125   0.2            0.547
    ## 12   0.6            125   0.4            0.522
    ## 13   0.8            125   0              0.797
    ## 14   0.8            125   0.2            0.682
    ## 15   0.8            125   0.4            0.605
    ## 16   1              125   0              0.996
    ## 17   1              125   0.2            0.817
    ## 18   1              125   0.4            0.679

``` r
df_plot <- df %>% group_by(trial, noise, rate) %>% 
  summarize(
    cumulativeMatcher = mean(cumsum(Feedback)/seq_along(Feedback)),
    sd = sd(cumsum(Feedback)/seq_along(Feedback))
  )
```

    ## `summarise()` has grouped output by 'trial', 'noise'. You can override using the `.groups` argument.

``` r
ggplot(data=df_plot, aes(x=trial, y=cumulativeMatcher, color=as.factor(noise))) + 
  geom_ribbon(aes(ymin=cumulativeMatcher-sd, ymax=cumulativeMatcher+sd), alpha=0.1, linetype=0) + 
  geom_line() +  
  facet_wrap(.~rate) + 
  theme_bw() + labs(title = 'Memory agent (matcher) against Random agent by rate', color='Noise') + scale_colour_brewer(palette = 'Accent')
```

![](main_files/figure-markdown_github/unnamed-chunk-8-1.png)

WSLSAgent (mismatcher) against MemoryAgent (matcher)
----------------------------------------------------

One round

``` r
trials <- 125
noise <- 0.0
mem_constraint <- 125

df <- WSLSAgainstMemory(trials, noise, mem_constraint)
```

``` r
ggplot(df) + 
  geom_line(color='red', aes(trial, matcher)) +  # matcher
  geom_line(color='blue', aes(trial, mismatcher)) +  # mismatcher
  theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ggplot(df) + 
  geom_line(color='red', aes(trial, cumulativeMatcher)) +  # matcher
  geom_line(color='blue', aes(trial, cumulativeMismatcher)) +  # mismatcher
  theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-11-1.png)

100 rounds with different parameter values

``` r
# j <- 0
# for (i in 1:100){  # number of repetions
#   for (n in seq(0,0.5,0.2)){  # noise
#     for (m in seq(5,125,30)){  # memory constraint
#       tmp <- WSLSAgainstMemory(
#         trials = 120,
#         noise = n,
#         mem_constraint= m
#       )
# 
#       tmp$noise <- n
#       tmp$mem_constraint <- m
#       tmp$rep <- i
# 
#       if (i ==1){ # first round
#         df <- tmp
#       } else {
#         df <- rbind(df, tmp)
#       }
# 
#       # print statement
#       if (i != j){
#         print(paste('Round:', i))
#       }
#       j <- i
#     }
#   }
# }
# 
# write_csv(df, "WSLSAgainstMemory.csv")
```

``` r
df <- read_csv('WSLSAgainstMemory.csv')
```

    ## Rows: 178320 Columns: 9

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (9): matcher, mismatcher, trial, Feedback, cumulativeMatcher, cumulative...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df %>% group_by(mem_constraint, noise) %>% 
  summarize(
    mean(Feedback),
    sd(Feedback)
  )
```

    ## `summarise()` has grouped output by 'mem_constraint'. You can override using the `.groups` argument.

    ## # A tibble: 15 x 4
    ## # Groups:   mem_constraint [5]
    ##    mem_constraint noise `mean(Feedback)` `sd(Feedback)`
    ##             <dbl> <dbl>            <dbl>          <dbl>
    ##  1              5   0              0.248          0.432
    ##  2              5   0.2            0.387          0.487
    ##  3              5   0.4            0.444          0.497
    ##  4             35   0              0.318          0.466
    ##  5             35   0.2            0.386          0.487
    ##  6             35   0.4            0.433          0.495
    ##  7             65   0              0.328          0.470
    ##  8             65   0.2            0.389          0.487
    ##  9             65   0.4            0.438          0.496
    ## 10             95   0              0.337          0.473
    ## 11             95   0.2            0.390          0.488
    ## 12             95   0.4            0.441          0.497
    ## 13            125   0              0.339          0.473
    ## 14            125   0.2            0.390          0.488
    ## 15            125   0.4            0.441          0.497

``` r
df_plot <- df %>% group_by(trial, noise) %>% 
  summarize(
    cumulativeMatcher = mean(cumsum(Feedback)/seq_along(Feedback)),
    sd = sd(cumsum(Feedback)/seq_along(Feedback))
  )
```

    ## `summarise()` has grouped output by 'trial'. You can override using the `.groups` argument.

``` r
ggplot(data=df_plot, aes(x=trial, y=cumulativeMatcher, color=as.factor(noise))) + 
  geom_ribbon(aes(ymin=cumulativeMatcher-sd, ymax=cumulativeMatcher+sd), alpha=0.1, linetype=0) + 
  geom_line(size=1) + 
  theme_bw() + labs(title = 'Memory agent (matcher) against WSLS probability agent', color='Noise') + scale_color_brewer(palette = 'Accent')
```

![](main_files/figure-markdown_github/unnamed-chunk-14-1.png)

WSLS prob (mismatcher) against memory (matcher)
-----------------------------------------------

One round

``` r
trials <- 125
noise <- 0.0
mem_constraint <- 125
prob <- 0.8

df <- WSLSpAgainstMemory(trials, noise, mem_constraint, prob)
```

``` r
ggplot(df) + 
  geom_line(color='red', aes(trial, matcher)) +  # matcher
  geom_line(color='blue', aes(trial, mismatcher)) +  # mismatcher
  theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
ggplot(df) + 
  geom_line(color='red', aes(trial, cumulativeMatcher)) +  # matcher
  geom_line(color='blue', aes(trial, cumulativeMismatcher)) +  # mismatcher
  theme_bw()
```

![](main_files/figure-markdown_github/unnamed-chunk-17-1.png)

100 rounds with different parameter values

``` r
# j <- 0
# for (i in 1:100){  # number of repetions
#   for (n in seq(0,0.5,0.2)){  # noise
#     for (m in seq(5,125,30)){  # memory constraint
#       tmp <- WSLSpAgainstMemory(
#         trials = 120,
#         noise = n,
#         mem_constraint= m,
#         prob=0.8
#       )
# 
#       tmp$noise <- n
#       tmp$mem_constraint <- m
#       tmp$rep <- i
# 
#       if (i ==1){ # first round
#         df <- tmp
#       } else {
#         df <- rbind(df, tmp)
#       }
# 
#       # print statement
#       if (i != j){
#         print(paste('Round:', i))
#       }
#       j <- i
#     }
#   }
# }
# 
# write_csv(df, "WSLSprobAgainstMemory.csv")
```

``` r
df <- read_csv('WSLSprobAgainstMemory.csv')
```

    ## Rows: 178320 Columns: 9

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (9): matcher, mismatcher, trial, Feedback, cumulativeMatcher, cumulative...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df %>% group_by(mem_constraint, noise) %>% 
  summarize(
    mean(Feedback),
    sd(Feedback)
  )
```

    ## `summarise()` has grouped output by 'mem_constraint'. You can override using the `.groups` argument.

    ## # A tibble: 15 x 4
    ## # Groups:   mem_constraint [5]
    ##    mem_constraint noise `mean(Feedback)` `sd(Feedback)`
    ##             <dbl> <dbl>            <dbl>          <dbl>
    ##  1              5   0              0.376          0.484
    ##  2              5   0.2            0.436          0.496
    ##  3              5   0.4            0.468          0.499
    ##  4             35   0              0.363          0.481
    ##  5             35   0.2            0.419          0.493
    ##  6             35   0.4            0.456          0.498
    ##  7             65   0              0.367          0.482
    ##  8             65   0.2            0.417          0.493
    ##  9             65   0.4            0.455          0.498
    ## 10             95   0              0.370          0.483
    ## 11             95   0.2            0.416          0.493
    ## 12             95   0.4            0.461          0.499
    ## 13            125   0              0.367          0.482
    ## 14            125   0.2            0.416          0.493
    ## 15            125   0.4            0.461          0.498

``` r
df_plot <- df %>% group_by(trial, noise) %>% 
  summarize(
    cumulativeMatcher = mean(cumsum(Feedback)/seq_along(Feedback)),
    sd = sd(cumsum(Feedback)/seq_along(Feedback)),
  )
```

    ## `summarise()` has grouped output by 'trial'. You can override using the `.groups` argument.

``` r
ggplot(data=df_plot, aes(x=trial, y=cumulativeMatcher, color=as.factor(noise))) + 
  geom_ribbon(aes(ymin=cumulativeMatcher-sd, ymax=cumulativeMatcher+sd), alpha=0.1, linetype=0) + 
  geom_line(size=1) + 
  theme_bw() + labs(title = 'Memory agent (matcher) against WSLS probability agent', color='Noise') + scale_color_brewer(palette = 'Accent')
```

![](main_files/figure-markdown_github/unnamed-chunk-20-1.png)

Compare different strategies against memory
-------------------------------------------

``` r
df_random <- read_csv('RandomAgainstMemory.csv') %>% group_by(trial) %>% 
  summarize(
    cumulativeMatcher = mean(cumsum(Feedback)/seq_along(Feedback)),
    sd = sd(cumsum(Feedback)/seq_along(Feedback)),
    opponent = 'Random',
    player = 'Memory'
  )
```

    ## Rows: 213960 Columns: 10

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (10): matcher, mismatcher, trial, Feedback, cumulativeMatcher, cumulativ...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df_WSLS <- read_csv('WSLSAgainstMemory.csv') %>% group_by(trial) %>% 
  summarize(
    cumulativeMatcher = mean(cumsum(Feedback)/seq_along(Feedback)),
    sd = sd(cumsum(Feedback)/seq_along(Feedback)),
    opponent = 'WSLS',
    player = 'Memory'
  )
```

    ## Rows: 178320 Columns: 9

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (9): matcher, mismatcher, trial, Feedback, cumulativeMatcher, cumulative...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df_WSLSp <- read_csv('WSLSprobAgainstMemory.csv') %>% group_by(trial) %>% 
  summarize(
    cumulativeMatcher = mean(cumsum(Feedback)/seq_along(Feedback)),
    sd = sd(cumsum(Feedback)/seq_along(Feedback)),
    opponent = 'WSLS_prob',
    player = 'Memory'
  )
```

    ## Rows: 178320 Columns: 9

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (9): matcher, mismatcher, trial, Feedback, cumulativeMatcher, cumulative...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df_plot <- rbind(df_random, df_WSLS, df_WSLSp)
```

``` r
ggplot(data=df_plot, aes(x=trial, y=cumulativeMatcher, color=opponent)) + 
  geom_ribbon(aes(ymin=cumulativeMatcher-sd, ymax=cumulativeMatcher+sd), alpha=0.1, linetype=0) + 
  geom_line(size=1) + 
  theme_bw() + labs(title = 'Memory (matcher) agent against opponent (mismatcher)', color='Opponent') + scale_color_brewer(palette = 'Accent')
```

![](main_files/figure-markdown_github/unnamed-chunk-22-1.png)
