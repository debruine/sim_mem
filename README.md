Understanding mixed effects models through simulating data
================
Lisa M. DeBruine & Dale J. Barr

Abstract
--------

Experimental designs that sample both subjects and stimuli from a larger population need to account for random effects of both subjects and stimuli using mixed effects models. However, much of this research is analyzed using ANOVA on aggregated responses because researchers are not confident specifying and interpreting mixed effects models. The tutorial will explain how to simulate data with random effects structure and analyse the data using linear mixed effects regression (with the lme4 R package). The focus will be on interpreting the LMER output in light of the simulated parameters and comparing the results to by-items and by-subjects ANOVA.

### Prerequisite knowledge or skills

-   Basic familiarity with experimental designs where subjects respond to stimuli
-   Basic familiarity with R

#### Who will benefit from this tutorial?

Researchers who use experimental designs that need to account for crossed random effects (e.g., designs that sample subjects and stimuli). For example, a large amount of experimental research in face perception or social cognition uses designs that would be better analysed using mixed effects models.

``` r
library(tidyverse) # for data wrangling and visualisation
library(afex)      # for LMEM and ANOVA
library(faux)      # devtools::install_github("debruine/faux")
library(broom.mixed) # for extracting data from mixed effect models
set.seed(8675309)  # this makes sure your script uses the same set of random numbers each time you run the full script 
                   # (never set this inside a function or loop)
```

Pilot Data
----------

In this tutorial we will simulate data from an [Implicit Association Task](https://implicit.harvard.edu/implicit/iatdetails.html). In our example study, people are asked to classify face images as male or female, and words as relating to math or literature. The side of the response options could be *congruent* (male and math on one side, female and literature on the other side) or *incongruent* (female and math on one side, male and literature on the other side). If people have social steoreotypes linking men with math and women with literature, reaction times should be faster for congruent trials than incongruent trials.

### Load the data

We're going to use the `IATData` dataset from the IAT package to figure out some realistic values for our variables. We have to do a bit of processing to filter, simplify, and recode this dataset; the code for that is at [github](https://github.com/debruine/sim_mem/blob/master/R/data_prep.R), but you can get the dataset using the code below.

``` r
iat_data <- readr::read_csv("iat_data.csv")
#iat_data <- readr::read_csv("https://raw.githubusercontent.com/debruine/sim_mem/master/iat_data.csv")
```

The dataset has 6441 rows, each representing a single trial, with the following columns:

-   `sub_id`: The subject ID
-   `stim_id`: The name of the stimulus
-   `condition`: Whether the trial was *congruent* or *incongruent*
-   `rt`: Trial latency
-   `stim_type`: Whether the stimulus is a *word* or a *face*
-   `stim_sex`: Whether the stimulus is stereotyped as *male* or *female*

### Aggregate the data

To start, we'll aggregate the data by subject and condition to calculate mean reaction times. This will create a table of 176 rows with columns `sub_id`, `condition`, and `rt`.

``` r
agg_data <- iat_data %>%
  group_by(sub_id, condition) %>%
  summarise(rt = mean(rt)) %>%
  ungroup()
```

The first step to understanding a new dataset is to plot it.

``` r
agg_data %>%
  ggplot(aes(condition, rt, fill = condition)) +
  geom_violin(trim = FALSE, show.legend = FALSE) +
  geom_boxplot(fill = "white", width = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("red", "dodgerblue"))
```

![](README_files/figure-markdown_github/pilot-plot-1.png)

### Analyse the data

We'll use a paired-samples t-test to test if mean reaction times in the congruent condition are faster than those in the incongruent condition.

``` r
t.test(rt~condition, agg_data, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  rt by condition
    ## t = -7.7558, df = 87, p-value = 1.547e-11
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -133.53782  -79.05579
    ## sample estimates:
    ## mean of the differences 
    ##               -106.2968

### Simulate the data

We're going to use this data to get some realistic values to describe the variables. First we put the data into wide format so there is one column for each cell.

``` r
vars <- agg_data %>%
  unite(var, condition) %>%
  spread(var, rt) %>%
  select(-sub_id)
```

Then calculate the means and standard deviations for each cell, as well as the correlation between the congruent and incongruent scores.

| var         |  congruent|  incongruent|    mean|      sd|
|:------------|----------:|------------:|-------:|-------:|
| congruent   |       1.00|         0.58|  669.94|  111.84|
| incongruent |       0.58|         1.00|  776.24|  155.09|

``` r
sub_n <- nrow(vars)
m1    <- mean(vars$congruent)
m2    <- mean(vars$incongruent)
sd1   <- sd(vars$congruent)
sd2   <- sd(vars$incongruent)
r     <- cor(vars$congruent, vars$incongruent)
```

You can use the `rnorm_multi()` function from faux to simulate normally-distributed within-subject variables with specified means, SDs and correlations.

``` r
sim_vars <- faux::rnorm_multi(
  n = sub_n,
  vars = 2,
  cors = r,
  mu = c(m1, m2),
  sd = c(sd1, sd2),
  varnames = c("congruent", "incongruent")
)
```

| var         |  congruent|  incongruent|    mean|      sd|
|:------------|----------:|------------:|-------:|-------:|
| congruent   |       1.00|         0.52|  675.61|   93.85|
| incongruent |       0.52|         1.00|  780.73|  152.19|

You can do this quickly using the [`simdf()`](https://debruine.github.io/posts/simdf/) function in faux, which generates a new dataframe from an existing dataframe, simulating all numeric columns from normal distributions with the same mean and SD as the existing data and the same correlation structure as the existing data. However, this method doesn't let you simulate data with different parameters than your pilot data.

``` r
sim_vars <- faux::simdf(vars, n = sub_n)
faux::check_sim_stats(sim_vars, usekable = TRUE)
```

| var         |  congruent|  incongruent|    mean|      sd|
|:------------|----------:|------------:|-------:|-------:|
| congruent   |       1.00|         0.67|  681.81|  111.34|
| incongruent |       0.67|         1.00|  778.22|  167.22|

Add a subject ID and put the data back into long format.

``` r
sim_data <- sim_vars %>%
  mutate(sub_id = 1:sub_n) %>%
  gather(condition, rt, congruent:incongruent)
```

Plot the simulated data along with the pilot data to make sure you did everything correctly.

``` r
bind_rows(
  mutate(agg_data, type = "pilot"),
  mutate(sim_data, type = "simulation")
) %>%
  ggplot(aes(condition, rt, fill = condition)) +
  facet_grid(~type) +
  geom_violin(trim = FALSE, show.legend = FALSE) +
  geom_boxplot(fill = "white", width = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("red", "dodgerblue"))
```

![](README_files/figure-markdown_github/sim-plot-1.png)

### Function

You can wrap this whole procedure in a function so you can easily change the simulation parameters. Simulate the data using the specified parameters, run the t-test, and return a data frame of the test statistics.

``` r
sim_iat <- function(sub_n, m1, m2, sd1, sd2, r) {
  data <- faux::rnorm_multi(
    n = sub_n,
    vars = 2,
    cors = r,
    mu = c(m1, m2),
    sd = c(sd1, sd2),
    varnames = c("congruent", "incongruent")
  ) %>%
  mutate(sub_id = 1:sub_n) %>%
  gather(condition, rt, congruent:incongruent)
  
  t.test(rt~condition, data, paired = TRUE) %>% 
    tidy()
}
```

### Power calculation

Now you can use this function to run many simulations. There are a lot of ways to do this. The pattern below uses the `map_df()` function from the purrr package. This function takes two arguments, a vector and a function, and returns a dataframe. It runs the function once for each item in the vector, so the vector 1:1000 below runs the function 1000 times.

The effect here is enormous, so can we get away with only testing 20 people? You can replicate this analysis 1000 times with `sub_n = 20` and extract the p-value from the test. Calculate power as the proportion of tests where the p-value is less than your alpha. Here, we'll set alpha to 0.01. We

``` r
sim <- purrr::map_df(1:1000, ~sim_iat(20, m1, m2, sd1, sd2, r))
alpha <- 0.01
power <- mean(sim$p.value < alpha)
```

![](README_files/figure-markdown_github/unnamed-chunk-14-1.png)

![](README_files/figure-markdown_github/unnamed-chunk-15-1.png)

Compare this to the power calculation from `pwr::pwr.t.test()`. This function requires the effect size in Cohen's d, so you can calculate this from the difference score's mean and SD.

``` r
diff <- vars$incongruent - vars$congruent

pwr::pwr.t.test(
  n = 20,
  type = "paired",
  d = mean(diff)/sd(diff),
  sig.level = alpha
)
```

    ## 
    ##      Paired t test power calculation 
    ## 
    ##               n = 20
    ##               d = 0.8267731
    ##       sig.level = 0.01
    ##           power = 0.7865117
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number of *pairs*

### ANOVA

See how the ANOVA compares to the t-test. The F-value is the square of the t-value, and the p-values are identical.

``` r
afex::aov_ez(
  id = "sub_id", 
  dv = "rt", 
  within = "condition", 
  data = agg_data
) %>% summary()
```

    ## 
    ## Univariate Type III Repeated-Measures ANOVA Assuming Sphericity
    ## 
    ##               Sum Sq num Df Error SS den Df  F value    Pr(>F)    
    ## (Intercept) 92023403      1  2461787     87 3252.124 < 2.2e-16 ***
    ## condition     497156      1   719046     87   60.153 1.547e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

LMEM
----

We need to *effect code* condition to match the behaviour of ANOVA. We set the congruent condition to -0.5 and the incongruent condition to +0.5 because we predict lower reaction times for the congruent condition.

``` r
agg_data$condition.e <- recode(agg_data$condition, 
                               "congruent" = -0.5, 
                               "incongruent" = 0.5)
```

A mixed effects model is specified in this format: `dv ~ terms + (1 | id)`, where `dv` is the value to be predicted, `terms` are the predictor variables, `1` represents the random intercept, and `id` is the grouping variable. Our formula is `rt ~ condition.e + (1 | sub_id)`.

``` r
lmem <- lmer(rt ~ condition.e + (1 | sub_id), data = agg_data)
```

The `summary()` function is often used to inspect the results.

``` r
summary(lmem)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rt ~ condition.e + (1 | sub_id)
    ##    Data: agg_data
    ## 
    ## REML criterion at convergence: 2179.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9666 -0.5831 -0.0398  0.4517  3.4934 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  sub_id   (Intercept) 10016    100.08  
    ##  Residual              8265     90.91  
    ## Number of obs: 176, groups:  sub_id, 88
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   723.09      12.68  87.00  57.027  < 2e-16 ***
    ## condition.e   106.30      13.71  87.00   7.756 1.55e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## condition.e 0.000

You can also use the `broom.mixed::tidy()` function to show a condensed version of the results.

``` r
tidy(lmem)
```

| effect    | group    | term              |  estimate|  std.error|  statistic|   df|  p.value|
|:----------|:---------|:------------------|---------:|----------:|----------:|----:|--------:|
| fixed     | NA       | (Intercept)       |   723.091|     12.680|     57.027|   87|        0|
| fixed     | NA       | condition.e       |   106.297|     13.705|      7.756|   87|        0|
| ran\_pars | sub\_id  | sd\_\_(Intercept) |   100.079|         NA|         NA|   NA|       NA|
| ran\_pars | Residual | sd\_\_Observation |    90.911|         NA|         NA|   NA|       NA|

First focus on the fixed effects. Note how the t-values and p-values correspond to the t-test and ANOVA. Also note how the estimate for the effect of condition corresponds to the mean difference calculated in the t-test. The estimate for the intercept corresponds to the overall mean reaction time.

Next look at the random effects. You can calculate the proportion of total variance explained by subject-specific intercepts as subject variance divided by total variance. This value (0.548) is approximately equal to the correlation between the congruent and incongruent conditions (0.577).

Simulating Data for LMEM
------------------------

The data simulation process for a mixed effect model uses the random effect standard deviations and the fixed effect estimates.

### Calculate parameters from data statistics

``` r
# you can calculate pooled variance in two ways, 
# depending on what info you have access to
var <- (sd1^2 + sd2^2)/2
var <- sd(vars$incongruent - vars$congruent)^2

grand_i  <- (m1 + m2)/2
cond_eff <- m2 - m1
sub_sd   <- sqrt(var * r)
err_sd   <- sqrt(var * (1-r))
```

| Parameter  |    Value|
|------------|--------:|
| `grand_i`  |  723.091|
| `cond_eff` |  106.297|
| `sub_sd`   |   97.697|
| `err_sd`   |   83.576|

### Get parameters from LMEM

The function `broom.mixed::tidy()` helps you to extract important numbers from mixed effects models.

``` r
params <- broom.mixed::tidy(lmem)
```

| effect    | group    | term              |   estimate|  std.error|  statistic|        df|  p.value|
|:----------|:---------|:------------------|----------:|----------:|----------:|---------:|--------:|
| fixed     | NA       | (Intercept)       |  723.09076|   12.67972|  57.027329|  86.99975|        0|
| fixed     | NA       | condition.e       |  106.29681|   13.70541|   7.755828|  87.00014|        0|
| ran\_pars | sub\_id  | sd\_\_(Intercept) |  100.07893|         NA|         NA|        NA|       NA|
| ran\_pars | Residual | sd\_\_Observation |   90.91141|         NA|         NA|        NA|       NA|

Here, we need the grand intercept (`grand_i`), the effect of condition (`cond_eff`), the standard deviation for subject random intercepts (`sub_sd`) and the standard deviation for the residual, or error, variance (`err_sd`)

``` r
grand_i  <- filter(params, term == "(Intercept)") %>% pull(estimate)
cond_eff <- filter(params, term == "condition.e") %>% pull(estimate)
sub_sd   <- filter(params, group == "sub_id")     %>% pull(estimate)
err_sd   <- filter(params, group == "Residual")   %>% pull(estimate)
```

| Parameter  |    Value|
|------------|--------:|
| `grand_i`  |  723.091|
| `cond_eff` |  106.297|
| `sub_sd`   |  100.079|
| `err_sd`   |   90.911|

### Simulate subjects

Simulate 100 new subjects who have random intercepts with the same standard deviation as the subjects in your pilot data.

``` r
sub_n <- 100

sub <- tibble(
  sub_id = 1:sub_n,
  sub_i = rnorm(sub_n, 0, sub_sd)
)
```

### Simulate trials

Create a data table of each trial by crossing the subject IDs with each within-subject condition using `expand.grid()`. This function creates every possible combination of the factors you specify.

``` r
trials <- expand.grid(
  sub_id = sub$sub_id,
  condition = c("congruent", "incongruent")
)
```

### Simulate the DV

Simulate your data by starting with the trial table, joining in the `sub` table to include each subject's random intercept, effect-coding condition, calculating the error term using the random effects standard deviation for the residual defined above, and calculating the reaction time by adding together the grand intercept (defined above by the intercept fixed effect estimate), the subject-specific intercept, the effect of condition multiplied by the condition effect-code, and the error term.

``` r
sim_data <- trials %>%
  left_join(sub, by = "sub_id") %>%
  mutate(
    condition.e = recode(condition, "incongruent" = 0.5, "congruent" = -0.5),
    error = rnorm(nrow(.), 0, err_sd),
    rt = grand_i + sub_i + (cond_eff * condition.e) + error
  )
```

### Analysis

Run a mixed-effect model on your simulated data and use `tidy()` to output a condensed version of the results.

``` r
lmer(rt ~ condition.e + (1 | sub_id), data = sim_data) %>% tidy()
```

| effect    | group    | term              |  estimate|  std.error|  statistic|   df|  p.value|
|:----------|:---------|:------------------|---------:|----------:|----------:|----:|--------:|
| fixed     | NA       | (Intercept)       |   702.172|     11.453|     61.307|   99|        0|
| fixed     | NA       | condition.e       |   106.707|     14.815|      7.203|   99|        0|
| ran\_pars | sub\_id  | sd\_\_(Intercept) |    87.355|         NA|         NA|   NA|       NA|
| ran\_pars | Residual | sd\_\_Observation |   104.756|         NA|         NA|   NA|       NA|

You can wrap this in a function, as well.

``` r
sim_iat_lme <- function(sub_n, grand_i, cond_eff, sub_sd, err_sd) {
  sub <- tibble(
    sub_id = 1:sub_n,
    sub_i = rnorm(sub_n, 0, sub_sd)
  )
  trials <- expand.grid(
    sub_id = sub$sub_id,
    condition = c("congruent", "incongruent")
  )
  sim_data <- trials %>%
    left_join(sub, by = "sub_id") %>%
    mutate(
      condition.e = recode(condition, "incongruent" = 0.5, "congruent" = -0.5),
      error = rnorm(nrow(.), 0, err_sd),
      rt = grand_i + sub_i + (cond_eff * condition.e) + error
    )
  
  lmer(rt ~ condition.e + (1 | sub_id), data = sim_data) %>%  tidy()
}
```

Run this function with different parameters to see how they change the estimates.

``` r
sim_iat_lme(sub_n = 100, grand_i = 500, cond_eff = 50, sub_sd = 100, err_sd = 100)
```

| effect    | group    | term              |  estimate|  std.error|  statistic|   df|  p.value|
|:----------|:---------|:------------------|---------:|----------:|----------:|----:|--------:|
| fixed     | NA       | (Intercept)       |   489.703|     12.990|     37.698|   99|    0.000|
| fixed     | NA       | condition.e       |    42.139|     14.244|      2.958|   99|    0.004|
| ran\_pars | sub\_id  | sd\_\_(Intercept) |   108.638|         NA|         NA|   NA|       NA|
| ran\_pars | Residual | sd\_\_Observation |   100.719|         NA|         NA|   NA|       NA|

``` r
sim_iat_lme(sub_n = 50, grand_i = 300, cond_eff = 50, sub_sd = 200, err_sd = 75)
```

| effect    | group    | term              |  estimate|  std.error|  statistic|   df|  p.value|
|:----------|:---------|:------------------|---------:|----------:|----------:|----:|--------:|
| fixed     | NA       | (Intercept)       |   329.415|     34.307|      9.602|   49|    0.000|
| fixed     | NA       | condition.e       |    53.438|     15.529|      3.441|   49|    0.001|
| ran\_pars | sub\_id  | sd\_\_(Intercept) |   236.294|         NA|         NA|   NA|       NA|
| ran\_pars | Residual | sd\_\_Observation |    77.645|         NA|         NA|   NA|       NA|

### Power calculation

We can run the power simulation the same way as before. However, this time we get four rows back for each simulation, so we need to filter for `term == "condition.e"` to get the p-value for the effect of condition.

``` r
sim <- purrr::map_df(1:100, ~sim_iat_lme(20, grand_i, cond_eff, sub_sd, err_sd))
```

    ## boundary (singular) fit: see ?isSingular

``` r
alpha <- 0.01
p <- sim %>%
  filter(term == "condition.e") %>%
  pull(p.value) 

power <- mean(p < alpha)
```

![](README_files/figure-markdown_github/unnamed-chunk-37-1.png)

Stop Ignoring Trials
--------------------

``` r
iat_data$condition.e <- recode(iat_data$condition, 
                               "congruent" = -0.5, 
                               "incongruent" = 0.5)

pilot_mod <- lmer(rt ~ condition.e +
              (1 + condition.e | sub_id) + 
              (1 | stim_id), 
            data = iat_data)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00721324
    ## (tol = 0.002, component 1)

``` r
summary(pilot_mod)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rt ~ condition.e + (1 + condition.e | sub_id) + (1 | stim_id)
    ##    Data: iat_data
    ## 
    ## REML criterion at convergence: 89285.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1692 -0.5712 -0.2077  0.2648  5.4806 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  sub_id   (Intercept) 13307    115.36       
    ##           condition.e 13206    114.92   0.43
    ##  stim_id  (Intercept)  3151     56.13       
    ##  Residual             57397    239.58       
    ## Number of obs: 6441, groups:  sub_id, 88; stim_id, 22
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   737.83      17.44  66.49  42.297  < 2e-16 ***
    ## condition.e   106.02      13.64  86.37   7.773 1.49e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## condition.e 0.273 
    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.00721324 (tol = 0.002, component 1)

### Get parameters from LMEM

``` r
params <- broom.mixed::tidy(pilot_mod)

grand_i <- filter(params, term == "(Intercept)") %>% pull(estimate)
cond_eff <- filter(params, term == "condition.e") %>% pull(estimate)
sub_sd <- filter(params, group == "sub_id", term == "sd__(Intercept)") %>% pull(estimate)
stim_sd <- filter(params, group == "stim_id", term == "sd__(Intercept)") %>% pull(estimate)
err_sd <- filter(params, group == "Residual") %>% pull(estimate)
```

Simulate 100 new subjects who have random intercepts with the same standard deviation as the subjects in your pilot data.

``` r
sub_n <- 100

sub <- tibble(
  sub_id = 1:sub_n,
  sub_i = rnorm(sub_n, 0, sub_sd)
)
```

### Get the exact stimuli

``` r
stim_desc <-  iat_data %>%
  group_by(stim_id, stim_type, stim_sex) %>%
  summarise()

stim <- ranef(pilot_mod)$stim_id %>%
  as_tibble(rownames = 'stim_id') %>%
  rename(stim_i = `(Intercept)`) %>%
  left_join(stim_desc, by = "stim_id")
```

### Graph the stimulus intercepts

``` r
ggplot(stim, aes(stim_i)) +
  geom_density()
```

![](README_files/figure-markdown_github/unnamed-chunk-42-1.png)

Hmm, that looks bimodal.

``` r
ggplot(stim, aes(stim_i, color = stim_type)) +
  geom_density()
```

![](README_files/figure-markdown_github/unnamed-chunk-43-1.png)

### Or simulate new stimuli

Let's imagine we fixed this difference with new stimuli.

``` r
stim_n <- 20

stim <- tibble(
  stim_id = 1:stim_n,
  stim_i = rnorm(stim_n, 0, stim_sd)
)
```

### Simulate data

``` r
sim_dat_lmem <- expand.grid(
  sub_id = sub$sub_id,
  stim_id = stim$stim_id,
  condition = c("congruent", "incongruent")
) %>%
  left_join(sub, by = "sub_id") %>%
  left_join(stim, by = "stim_id") %>%
  mutate(
    condition.e = recode(condition, "incongruent" = 0.5, "congruent" = -0.5),
    error = rnorm(nrow(.), 0, err_sd),
    rt = grand_i + sub_i + stim_i + (cond_eff * condition.e) + error
  )
```

``` r
lmer(rt ~ condition.e +
              (1 | sub_id) + 
              (1 | stim_id), 
            data = sim_dat_lmem) %>%
  tidy()
```

    ## # A tibble: 5 x 8
    ##   effect   group  term        estimate std.error statistic     df   p.value
    ##   <chr>    <chr>  <chr>          <dbl>     <dbl>     <dbl>  <dbl>     <dbl>
    ## 1 fixed    <NA>   (Intercept)    726.      16.2       44.7   57.0  4.79e-46
    ## 2 fixed    <NA>   condition.e     98.7      7.57      13.0 3880.   4.50e-38
    ## 3 ran_pars sub_id sd__(Inter…    111.      NA         NA     NA   NA       
    ## 4 ran_pars stim_… sd__(Inter…     50.1     NA         NA     NA   NA       
    ## 5 ran_pars Resid… sd__Observ…    239.      NA         NA     NA   NA

### Function

``` r
sim_func <- function(sub_n, stim_n, grand_i, cond_eff, sub_sd, stim_sd, err_sd) {
  sub <- tibble(
    sub_id = 1:sub_n,
    sub_i = rnorm(sub_n, 0, sub_sd)
  )
  
  stim <- tibble(
    stim_id = 1:stim_n,
    stim_i = rnorm(stim_n, 0, stim_sd)
  )
  
  sim_dat_lmem <- expand.grid(
    sub_id = sub$sub_id,
    stim_id = stim$stim_id,
    condition = c("congruent", "incongruent")
  ) %>%
    left_join(sub, by = "sub_id") %>%
    left_join(stim, by = "stim_id") %>%
    mutate(
      condition.e = recode(condition, "incongruent" = 0.5, "congruent" = -0.5),
      error = rnorm(nrow(.), 0, err_sd),
      rt = grand_i + sub_i + stim_i + (cond_eff * condition.e) + error
    )
  
  lmer(rt ~ condition.e +
              (1 | sub_id) + 
              (1 | stim_id), 
            data = sim_dat_lmem) %>%
  tidy()
}
```

``` r
sim_func(100, 20, 500, 100, 100, 100, 100)
```

    ## # A tibble: 5 x 8
    ##   effect   group  term       estimate std.error statistic     df    p.value
    ##   <chr>    <chr>  <chr>         <dbl>     <dbl>     <dbl>  <dbl>      <dbl>
    ## 1 fixed    <NA>   (Intercep…    468.      21.1       22.2   31.3  9.67e- 21
    ## 2 fixed    <NA>   condition…     96.4      3.21      30.0 3880.   2.21e-178
    ## 3 ran_pars sub_id sd__(Inte…    100.      NA         NA     NA   NA        
    ## 4 ran_pars stim_… sd__(Inte…     82.4     NA         NA     NA   NA        
    ## 5 ran_pars Resid… sd__Obser…    102.      NA         NA     NA   NA

Simulate a null effect

``` r
sim <- purrr::map_df(1:100, ~sim_func(sub_n=100, stim_n=20, grand_i, 
                                      cond_eff=0, sub_sd, stim_sd, err_sd))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00216653
    ## (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00719514
    ## (tol = 0.002, component 1)

``` r
alpha <- 0.01
p <- sim %>%
  filter(term == "condition.e") %>%
  pull(p.value) 

power <- mean(p < alpha)
```

![](README_files/figure-markdown_github/unnamed-chunk-50-1.png)

Terminology
-----------

(derived from broom\_mixed)

-   *terms*: the categorical or continuous predictor variables
-   *fixed effects*: the parameters that describe the population-level effects of predictor variables
-   *random-effect parameters*: the upper-level parameters that describe the distribution of random variables (variance, covariance, precision, standard deviation, or correlation)
-   *random-effect values*: the values that describe the deviation of the observations in a group level from the population-level effect
-   *grouping variable*: the categorical variable (factor) that identifies which group or cluster an observation belongs to
-   *group level* the particular level of a factor that specifies which level of the grouping variable an observation belongs to
