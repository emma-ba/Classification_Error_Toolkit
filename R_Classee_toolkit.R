###########################
### ENVIRONMENT
###########################
if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse) 
library(ggplot2)

if(!require('Rmisc')) install.packages('Rmisc')
library(Rmisc)

# By default R shows the scientific notation
options(scipen = 999)
options(stringsAsFactors = FALSE)

###########################
### VIZ TOOLKIT
###########################

classee_binary <- function(data=data_sample_binary, title=''){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3' 
  
  data %>%
    mutate(nitem = ifelse(output %in% c('TN','FN'),-1*(nitem %>% abs),(nitem %>% abs))) %>%
    mutate(class = ifelse(output %in% c('TP','FN'),'C1','C0')) %>%
    mutate(class = factor(class, levels=c('C1','C0') )) %>%  
    mutate(threshold = as.character(threshold)) %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN') )) %>% 
    ggplot(aes(x=threshold, y=nitem, fill=output, group=class)) +
    geom_bar(stat='identity', position='dodge', color='#ffffff') +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    annotate("text", x = -0.05, y = 0, hjust = 0.5, vjust = 1, angle=90,
             label = "\u2190 Classified as Negative  |   Classified as Positive \u2192 ",
             size=3.5, color='#333333' ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_line(linetype = "dotted")) %>%
    return
}

classee_binary_facet <- function(data=data_sample_binary_facet, title='', scale_free = FALSE){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3' 
  
  p <- data %>%
    mutate(nitem = ifelse(output %in% c('TN','FN'),-1*(nitem %>% abs),(nitem %>% abs))) %>%
    mutate(class = factor(class, levels=c('C1','C0') )) %>%  
    mutate(threshold = as.character(threshold)) %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN') )) %>% 
    ggplot(aes(x=threshold, y=nitem, fill=output, group=class)) +
    geom_bar(stat='identity', position='dodge', color='#ffffff') +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    theme_void()
  
  if(scale_free) p <- p + facet_wrap(c0 ~ c1, scales='free') 
  else p <- p + facet_wrap(c0 ~ c1) 
  
  return(p)
}

classee_binary_var_alt <- function(data=data_sample_binary, z=1.96, title=''){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3'   
  
  data <- data %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN'))) %>%
    arrange(threshold, output)
  
  data_var <- c()
  for(t in unique(data$threshold) %>% sort){
    slice <- data %>% filter(threshold == t)
    
    fn <- slice %>% filter(output=='FN') %>% select(nitem) %>% as.numeric %>% abs
    tp <- slice %>% filter(output=='TP') %>% select(nitem) %>% as.numeric
    if(fn == 0) sd_tp <- 0
    else if(tp == 0) sd_tp <- 0
    else sd_tp <- sqrt( tp*fn / ((tp+fn)^3) )
    
    fp <- slice %>% filter(output=='FP') %>% select(nitem) %>% as.numeric
    tn <- slice %>% filter(output=='TN') %>% select(nitem) %>% as.numeric %>% abs
    if(tn == 0) sd_tn <- 0
    else if(fp == 0) sd_tn <- 0
    else sd_tn <- sqrt( tn*fp / ((tn+fp)^3) )
    
    if(is.na(sd_tp)) sd_tp <- 0
    if(is.na(sd_tn)) sd_tn <- 0
    
    data_var <- c(data_var, sd_tp * (fn+tp) * z )
    data_var <- c(data_var, -1 * sd_tp * (fn+tp) * z )
    data_var <- c(data_var, sd_tn * (fp+tn) * z )
    data_var <- c(data_var, -1 * sd_tn * (fp+tn) * z)
  }
  
  data <- data %>%
    mutate(ci = data_var) %>%
    mutate(min = nitem - ci) %>%
    mutate(max = nitem + ci)
  
  ggplot(data=data, aes(x=threshold, group=class)) + 
    geom_bar(aes(y=nitem - ci, fill=output), stat='identity', position='dodge', 
             color='#ffffff', size=0.1) +
    geom_crossbar(aes(y=nitem, ymin=min, ymax=max, fill=output), position='dodge',
                  alpha=0.75, color='#ffffff', size=0.2, fatten=4, show.legend=T) +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend:\nLighter areas 95% CI") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    annotate("text", x = -0.05, y = 0, hjust = 0.5, vjust = 1, angle=90,
             label = "\u2190 Classified as Negative  |   Classified as Positive \u2192 ",
             size=3, color='#333333' ) +
    theme_minimal() +
    theme( panel.grid.major.x = element_line(linetype = "dotted") ) %>%
    return
}

classee_binary_var <- function(data=data_sample_binary, z=1.96, title=''){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3'   
  
  data <- data %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN'))) %>%
    arrange(threshold, output)
  
  data_var <- c()
  for(t in unique(data$threshold) %>% sort){
    slice <- data %>% filter(threshold == t)
    
    fn <- slice %>% filter(output=='FN') %>% select(nitem) %>% as.numeric %>% abs
    tp <- slice %>% filter(output=='TP') %>% select(nitem) %>% as.numeric
    if(fn == 0) sd_tp <- 0
    else if(tp == 0) sd_tp <- 0
    else sd_tp <- sqrt( tp*fn / ((tp+fn)^3) )
    
    fp <- slice %>% filter(output=='FP') %>% select(nitem) %>% as.numeric
    tn <- slice %>% filter(output=='TN') %>% select(nitem) %>% as.numeric %>% abs
    if(tn == 0) sd_tn <- 0
    else if(fp == 0) sd_tn <- 0
    else sd_tn <- sqrt( tn*fp / ((tn+fp)^3) )
    
    if(is.na(sd_tp)) sd_tp <- 0
    if(is.na(sd_tn)) sd_tn <- 0
    
    data_var <- c(data_var, sd_tp * (fn+tp) * z )
    data_var <- c(data_var, -1 * sd_tp * (fn+tp) * z )
    data_var <- c(data_var, sd_tn * (fp+tn) * z )
    data_var <- c(data_var, -1 * sd_tn * (fp+tn) * z)
  }
  
  data <- data %>%
    mutate(ci = data_var) %>%
    mutate(min = nitem - ci) %>%
    mutate(max = nitem + ci)
  
  ggplot(data=data, aes(x=threshold, y=nitem - ci, group=class, fill=output)) + 
    geom_bar(stat='identity', position='dodge', color='#ffffff', size=0.1, alpha=0.2) +
    geom_crossbar(aes(ymin=min, ymax=max), position='dodge', colour='#ffffff', size=0.1, fatten=0, alpha=1, show.legend=T) +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend\nDarker colors: 95%CI") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    annotate("text", x = -0.05, y = 0, hjust = 0.5, vjust = 1, angle=90,
             label = "\u2190 Classified as Negative  |   Classified as Positive \u2192 ",
             size=3, color='#333333' ) +
    theme_minimal() +
    theme( panel.grid.major.x = element_line(linetype = "dotted") ) %>%
    return
}

classee_binary_var_facet <- function(data=data_sample_binary_facet, z=1.96, title='', scale_free=F){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3'   
  
  data <- data %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN'))) %>%
    arrange(index, threshold, output)
  
  data_var <- c()
  for(ii in unique(data$index) %>% sort){
    for(t in unique(data$threshold) %>% sort){
      slice <- data %>% filter(index == ii, threshold == t)
      
      fn <- slice %>% filter(output=='FN') %>% select(nitem) %>% as.numeric %>% abs
      tp <- slice %>% filter(output=='TP') %>% select(nitem) %>% as.numeric
      if(fn == 0) sd_tp <- 0
      else if(tp == 0) sd_tp <- 0
      else sd_tp <- sqrt( tp*fn / ((tp+fn)^3) )
      
      fp <- slice %>% filter(output=='FP') %>% select(nitem) %>% as.numeric
      tn <- slice %>% filter(output=='TN') %>% select(nitem) %>% as.numeric %>% abs
      if(tn == 0) sd_tn <- 0
      else if(fp == 0) sd_tn <- 0
      else sd_tn <- sqrt( tn*fp / ((tn+fp)^3) )
      
      if(is.na(sd_tp)) sd_tp <- 0
      if(is.na(sd_tn)) sd_tn <- 0
      
      data_var <- c(data_var, sd_tp * (fn+tp) * z )
      data_var <- c(data_var, -1 * sd_tp * (fn+tp) * z )
      data_var <- c(data_var, sd_tn * (fp+tn) * z )
      data_var <- c(data_var, -1 * sd_tn * (fp+tn) * z)
    }
  }
  
  data <- data %>%
    mutate(ci = data_var) %>%
    mutate(min = nitem - ci) %>%
    mutate(max = nitem + ci)
  
  p <- ggplot(data=data, aes(x=threshold, y=nitem - ci, group=class, fill=output)) + 
    geom_bar(stat='identity', position='dodge', color='#ffffff', size=0.1, alpha=0.2) +
    geom_crossbar(aes(ymin=min, ymax=max), position='dodge', colour='#ffffff', size=0.1, fatten=0, alpha=1, show.legend=T) +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend\nDarker colors: 95%CI") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    theme_void() +
    theme(strip.text = element_text(colour='#cccccc')) 
  
  if(scale_free) p <- p + facet_wrap(c0 ~ c1, scales='free') 
  else p <- p + facet_wrap(c0 ~ c1) 
  
  return(p)
}

classee_multiclass <- function(data=confmat, title='', tp_norm=FALSE){
  blue <- '#1f8ac4' 
  black_fp_1 <- '#000000'
  black_fp_2 <- '#555555'
  black_fp_3 <- '#777777'
  red_fn_1 <- '#a50026'  
  red_fn_2 <- '#c8424a'  
  red_fn_3 <- '#e39d8a' 
  
  if(tp_norm) for(i in 1:ncol(data)) data[,i] <- (100 * data[,i] / data[i,i] ) %>% round
  
  tp <- data %>% as.matrix %>% diag
  nclass <- nrow(data)
  c_name <- colnames(data) 
  
  d <- tibble()
  for(i in 1:nclass){
    fp <- data[i,][-i] %>% sort(decreasing = TRUE)
    fn <- data[,i][-i] %>% sort(decreasing = TRUE)
    fn <- fn * -1
    
    d <- rbind(d, c( paste0('C',i), 'FN rest', (fn[-c(1:2)] %>% sum) ))
    d <- rbind(d, c( paste0('C',i), 'FN 2nd class', fn[2]) )
    d <- rbind(d, c( paste0('C',i), 'FN 1st class', fn[1]) )
    d <- rbind(d, c( paste0('C',i), 'TP', tp[i]) )
    d <- rbind(d, c( paste0('C',i), 'FP 1st class', fp[1]) )
    d <- rbind(d, c( paste0('C',i), 'FP 2nd class', fp[2]) )
    d <- rbind(d, c( paste0('C',i), 'FP rest', (fp[-c(1:2)] %>% sum) ))
  }
  colnames(d) <- c('class','type','nitem')
  
  
  d <- d %>%
    mutate(class = factor(class)) %>% 
    mutate(type = factor(type, levels=c('FN rest','FN 2nd class', 'FN 1st class', 
                                        'TP', 'FP 1st class', 'FP 2nd class', 'FP rest'))) %>% 
    mutate(nitem = as.numeric(nitem)) 
  
  d %>%
    ggplot(aes(x=class, y=nitem, fill=type, 
               group=factor(type, levels=c('FN rest','FN 2nd class', 'FN 1st class', 
                                           'FP rest', 'FP 2nd class', 'FP 1st class', 'TP')))) + 
    geom_bar(stat='identity', color='#ffffff') +
    scale_fill_manual(values=c(red_fn_3, red_fn_2, red_fn_1, blue, black_fp_1, black_fp_2, black_fp_3), name="Legend") +
    guides(fill = guide_legend(reverse=T)) +
    labs(title=title, x="Class", y="Number of Objects") +
    annotate("text", x = -0.05, y = 0, hjust = 0.5, vjust = 1, angle=90,
             label = "\u2190 Discarded  |   Selected \u2192 ", 
             size=3.5, color='#333333' ) +
    theme_minimal() %>%
    return
}


###########################
### SAMPLE DATA
###########################

######## BINARY ########
thres <- 1:19/20
m0 <- 0.4
m1 <- 0.6
sd0 <- sd1 <- 0.1
n0 <- n1 <- 100

data_sample_binary <- tibble(threshold=thres) %>%
  mutate(TN = (pnorm(thres, m0, sd0)*n0) %>% round) %>%
  mutate(FN = (pnorm(thres, m1, sd1)*n1) %>% round) %>%
  mutate(FP = n0 - TN) %>%
  mutate(TP = n1 - FN) %>%
  mutate(TN = -1*TN) %>%
  mutate(FN = -1*FN) %>%
  gather(TN,FN,FP,TP,key='output',value='nitem') %>%
  mutate(threshold = as.character(threshold)) %>%
  mutate(class = ifelse(output %in% c('TP','FN'), 'C1', 'C0'))


######## BINARY WITH FACETS ########
thres <- 1:19/20
mu_0 <- c(0.35,0.45)
mu_1 <- c(0.55,0.65)
n_0. <- n_1. <- c(100, 150)
sd_0 <- sd_1 <- c(0.1,0.2)

#
data_sample_binary_facet <- tibble()
i=0
#
for(sd1 in sd_1){  for(sd0 in sd_0){
  for(n1 in n_1.){  for(n0 in n_0.){
    for(m1 in mu_1){ for(m0 in mu_0){
      i <- i+1
      
      data <- tibble(thres)
      data <- cbind( data, (pnorm(thres, m0, sd0)*n0) %>% round)
      data <- cbind( data, (pnorm(thres, m1, sd1)*n1) %>% round )
      data <- cbind( data, n0 - data[2])
      data <- cbind( data, n1 - data[3])
      
      # IMPORTANT: 
      # The variable `index` is used to order the data to calculate the variance.
      # It is also used to order the graphs with ggplot faceting.
      # (as strings, thus avoid pasting numbers with decimals)
      # The `index` values must be unique for each graph.
      # (1 graph = 1 set of parameters n, mu, sd for classes 0 and 1)
      
      i1_m <- m1 * 100
      i0_m <- m0 * 100
      i1_n <- n1
      i0_n <- n0
      i0_s <- sd0 * 100 
      i1_s <- sd1 * 100
      index <- paste0( i0_n, i0_m, i0_s, i1_n, i1_m, i1_s)
      
      data <- cbind( data, rep(paste0(index,'-', i,' n1:',n1, ' mu1:',m1 %>% round(digits=3), ' sd1:',sd1), length(thres)))
      data <- cbind( data, rep(paste0(index,'-', i,' n0:',n0, ' mu0:',m0 %>% round(digits=3), ' sd0:',sd0), length(thres)))
      data <- cbind( data, rep(paste0( index) ))
      data_sample_binary_facet <- rbind(data_sample_binary_facet, data)
    }}
  }}
}}
colnames(data_sample_binary_facet) <- c('threshold','TN','FN','FP','TP','c1','c0','index')

data_sample_binary_facet <- data_sample_binary_facet %>%
  mutate(TN = -1*TN) %>%
  mutate(FN = -1*FN) %>%
  gather(TN,FN,FP,TP,key='output',value='nitem') %>%
  mutate(threshold = as.character(threshold)) %>%
  mutate(class = ifelse(output %in% c('TP','FN'), 'C1', 'C0'))


######## MULTICLASS ########

# Confusion Matrix:
confmat <- rbind(c(316,0,8,23,51,0,1),
                 c(0,327,0,0,0,0,0),
                 c(0,0,122,6,25,4,0),
                 c(7,3,8,280,20,13,0),
                 c(7,0,192,13,234,0,5),
                 c(0,0,0,8,0,313,0),
                 c(0,0,0,0,0,0,324))

colnames(confmat) <- paste('Actual',1:ncol(confmat))
rownames(confmat) <- paste('Predicted',1:ncol(confmat))