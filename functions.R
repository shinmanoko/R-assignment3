

#####################################################################################

### 2.A.
#
# Difference in the proportion of cases with a specific value between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
# target_value: the value of var that will be counted
#
# RETURN VALUE:
# The percentage of cases in which `var` was equal to `target_value` for the first group,
# minus the percentage of cases in which `var` was equal to `target_value` for the
# second group.
#
difference_in_proportion <- function(d, var, grouping_var, group1, group2,
                                     target_value) {
  # YOUR CODE HERE: calculate the proportion of cases in which `var` is equal to
  # the value specified in `target_value` in the first group, and then again in the
  # second group
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  per_1 <- nrow(dplyr::filter(d_1, get(var)==target_value))/ nrow(d_1)
  per_2 <- nrow(dplyr::filter(d_2, get(var)==target_value))/ nrow(d_2)
  
  # YOUR CODE HERE: assign the difference in these proportions to the variable `result`
  result <- per_1 - per_2
  return(result)
}


#####################################################################################

### 2.B.
#
# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[,var]<-sample(d[[var]], nrow(d))
  return(d)}


# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999, ...) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2, ...)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    fake <- randomize(d, var)
    #print(fake)
    permutation_statistics[i] <- statistic(fake, var, grouping_var, group1, group2, ...)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}

permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}



#####################################################################################

### 3.B.
#
# Perform a permutation test for two implicit groups of binary trials summarized
# by the number of "successes" and trials for each of the two groups, using the
# difference in the proportion of successes (group 1 minus group 2) as a test
# statistic.
#
# ARGUMENTS:
# k1: the number of "successes" (i.e., observations of one of the two types) in group 1
# k2: the number of "successes" in group 2
# n1: the total number of trials in group 1
# n2: the total number of trials in group 2
# n_samples: the number of permutations (defaults to 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permtest_difference_in_props <- function(k1, k2, n1, n2, n_samples=9999) {
  # Create a set of observations with exactly k1 and k2 successes
  obs_1 <- c(rep(TRUE, k1), rep(FALSE, n1 - k1)) # Individual observations from group 1
  obs_2 <- c(rep(TRUE, k2), rep(FALSE, n2 - k2)) # Individual observations from group 2
  observations <- c(obs_1, obs_2)
  # Permute this set of observations n_samples times, saving the result in a
  # matrix
  rep_observations <- matrix(rep(observations, n_samples), n1 + n2)
  perm_observations <- apply(rep_observations, 2, sample, n1 + n2)
  # Generate the proportions in the two groups amongst the permuted observations.
  # Tricks: mean() of a TRUE/FALSE variable is the proportion "TRUE";
  # instead of having explicit "Group" labels that we hold fixed, we just hold fixed
  # that the first n1 rows are "Group 1" and the remaining n2 rows are "Group 2",
  # which amounts to the same thing, and we generate the two percentages directly.
  props_group1 <- colMeans(perm_observations[1:n1,])
  #print(props_group1)
  props_group2 <- colMeans(perm_observations[(n1+1):(n1+n2),])
  #print(props_group2)
  test_stats <- props_group1 - props_group2
  return(list(observed=((k1/n1) - (k2/n2)), permuted=test_stats))
}

v_pdp_pvalue_right <- function(k1_vec, k2_vec, n1, n2, n_samples=9999) {
  
  #library(foreach)
  library(doMC)
  registerDoMC(3) # 设置并行核数, 并注册并行
  #library(doParallel)
  #cl <- makeCluster(2)  # 设置并行核数
  #registerDoParallel(cl) # 注册并行
  
  nexp <- length(k1_vec)
  result <- rep(NA, nexp)
  #foreach(i=1:nexp)  %dopar% {
  for(i in 1:nexp) {
    #result[i] <- permtest_difference_in_props(k1_vec[i],k2_vec[i],n1,n2,n_samples)
    ppp <- permtest_difference_in_props(k1_vec[i],k2_vec[i],n1,n2,n_samples)
    #print(ppp)
    result[i] <- permutation_pvalue_right(ppp)
    #result[i] <- 1 - (sum(ppp$permuted > 0.05)+1) / (length(ppp$permuted)+1)
  }
  return(result)
}


#####################################################################################

### 4.
#
# Pearson's cumulative test statistic
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
# grouping_var: the name of another column of d, provided as a string
#
# RETURN VALUE:
# The value of Pearson's cumulative test statistic, calculated over all possible
# combinations of `var` and `grouping_var`
pearson_x2_stat <- function(d, var, grouping_var) {
  values <- unique(d[[var]])
  groups <- unique(d[[grouping_var]])
  result <- 0
  for (v in values) {
    prop_overall <- mean(d[[var]] == v)
    for (g in groups) {
      d_g <- dplyr::filter(d, get(grouping_var) == g)
      prop_g <- mean(d_g[[var]] == v)
      result <- result + ((prop_g - prop_overall)^2)*(nrow(d_g)/prop_overall)
    } }
  return(result)
}


permutation_test_generalized <- function(d, var_to_permute, group_var, statistic, n_samples=9999) {
  observed_statistic <- statistic(d, var_to_permute, group_var)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    d_perm <- randomize(d, var_to_permute)
    permutation_statistics[i] <- statistic(d_perm, var_to_permute, group_var)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}


