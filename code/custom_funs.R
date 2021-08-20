# custom functions

# round numbers to exactly x decimal places
round_x <- function(n, x = 2) { format(round(n, x), nsmall = x) }

# generate heatmap of factor loadings
heatmap_fun <- function(efa, factor_names = NA){
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # put factors in a standard order when applicable
  bod_factors <- factor_names[grepl("bodily", tolower(factor_names))]
  
  leftovers <- factor_names[!factor_names %in% bod_factors]
  neg_factors <- leftovers[grepl("negative", tolower(leftovers))]
  
  leftovers <- leftovers[!leftovers %in% neg_factors]
  soc_factors <- leftovers[grepl("social", tolower(leftovers))]

  leftovers <- leftovers[!leftovers %in% soc_factors]
  cog_factors <- leftovers[grepl("cognit", tolower(leftovers))]
  
  other_factors <- leftovers[!leftovers %in% cog_factors]
  
  factor_levels <- c(bod_factors, neg_factors, 
                     soc_factors, cog_factors, other_factors)
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels))
  
  # get fa.sort() order
  order <- loadings %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(capacity, order)
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels)) %>%
    mutate(var_shared = paste0(factor, "\n", round(var, 2)*100, "% shared var.,"))
  
  # get percent total variance explained
  total_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Var") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels)) %>%
    mutate(var_total = paste0(round(var, 2)*100, "% total var."))
  
  # make plot
  plot <- ggplot(loadings %>% 
                   left_join(order) %>%
                   left_join(shared_var %>% select(-var)) %>%
                   left_join(total_var %>% select(-var)) %>%
                   mutate(capacity = gsub("_", " ", capacity),
                          factor = factor(factor, levels = factor_levels),
                          xlab = paste(var_shared, var_total, sep = "\n")),
                 aes(x = reorder(xlab, as.numeric(factor)), 
                     y = reorder(capacity, order), 
                     fill = loading, 
                     label = format(round(loading, 2), nsmall = 2))) +
    geom_tile(color = "black") +
    geom_text(size = 3) +
    scale_fill_distiller(limits = c(-1.02, 1.02), 
                         palette = "RdYlBu",
                         guide = guide_colorbar(barheight = 10)) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    theme(axis.title = element_blank()) +
    labs(caption = paste0(
      "Total variance accounted for: ",
      round_x(efa$Vaccounted["Cumulative Var", ncol(efa$Vaccounted)]*100, 0), 
      "%"))
  
  return(plot)
  
}

# suite of functions for implementing weisman et al.'s (2017) criteria
s_moments <- function(p) {p*(p+1)/2}
param_est <- function(p, k) {p*k + p - (k*(k-1)/2)}

check_ok <- function(p, k) {
  a <- (p-k)^2
  b <- p+k
  return(ifelse(a>b, TRUE, FALSE))
}

max_ok <- function(p) {
  df_check <- data.frame()
  for(i in 1:p){
    df_check[i,"check"] <- check_ok(p,i)
  }
  max <- df_check %>% filter(check) %>% nrow()
  return(max)
}

reten_fun <- function(df, rot_type = c("oblimin", "varimax", "none")){
  
  # figure out max number of factors to retain
  n_var <- length(names(df))
  max_k <- max_ok(n_var)
  
  # run efa with max factors, unrotated
  fa_unrot <- fa(df, nfactors = max_k, rotate = "none", 
                 scores = "tenBerge", impute = "median")
  eigen <- fa_unrot$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("param") %>%
    gather(factor, value, -param) %>%
    spread(param, value) %>%
    filter(`SS loadings` > 1, `Proportion Explained` > 0.05)
  retain_k <- nrow(eigen)
  
  fa_rot <- fa(df, nfactors = retain_k, rotate = rot_type,
               scores = "tenBerge", impute = "median")
  
  loadings <- fa_rot$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    count(factor)
  retain_k_final <- nrow(loadings)
  
  return(retain_k_final)
}

# function for highlighting strong factor loadings
bold <- function(x) { ifelse(x >= 0.6, T, F) }


