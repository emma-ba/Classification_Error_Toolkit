###########################
### ENVIRONMENT
###########################
if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse) 
library(ggplot2)

if(!require('Rmisc')) install.packages('Rmisc')
library(Rmisc)

options(stringsAsFactors = FALSE)

###########################
### CLASSEE VISUALIZATION
###########################

classee_binary <- function(data=classee_sample_binary(), title='', vertical=FALSE){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3' 
  
  p <- data %>%
    mutate(nitem = ifelse(output %in% c('TN','FN'),-1*(nitem %>% abs),(nitem %>% abs))) %>%
    mutate(class = ifelse(output %in% c('TP','FN'),'C1','C0')) %>%
    mutate(class = factor(class, levels=c('C1','C0') )) %>%  
    mutate(threshold = as.character(threshold))
  
  if(vertical){
    blank <- tibble(threshold=rep(' ',4), output = c('TN','TP','FN','FP'), 
                    nitem=rep(0,4), class = c('C0','C1','C1','C0'))
    t <- c(as.character(1:19/20),' ')
    p <- rbind(blank, p) %>%
      mutate(threshold = factor(threshold, levels=t))
  }
  
  p <- p %>%  
    mutate(output = factor(output, levels=c('TP','FN','FP','TN') )) %>% 
    ggplot(aes(x=threshold, y=nitem, fill=output, group=class)) +
    geom_bar(stat='identity', position='dodge', color='#ffffff') +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    theme_minimal() +
    theme(panel.grid.major.x = element_line(linetype = "dotted"))
  
  if(vertical){
    p <- p + coord_flip() +
      annotate("text", x = Inf, y = 0, hjust = 0.5, vjust = 1, angle=0,
               label = "Classified as Negative    \u2190 | \u2192   Classified as Positive  ",
               size=3.5, color='#333333' )
  } else {
    p <- p + annotate("text", x = -0.07, y = 0, hjust = 0.5, vjust = 1, angle=90,
                      label = "\u2190 Classified as Negative | Classified as Positive \u2192 ",
                      size=3.5, color='#333333' )
  }
  return(p)
}

classee_multiclass <- function(data=classee_sample_multi(), title='', tp_norm=FALSE){
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
### FACET & VARIANCE
###########################

classee_binary_facet <- function(data=classee_sample_binary_facet(), title='', scale_free = FALSE){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3' 
  
  p <- data %>%
    # Prepare data
    mutate(nitem = ifelse(output %in% c('TN','FN'),-1*(nitem %>% abs),(nitem %>% abs))) %>%
    mutate(class = factor(class, levels=c('C1','C0') )) %>%  
    mutate(threshold = as.character(threshold)) %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN') )) %>% 
    # Visualization
    ggplot(aes(x=threshold, y=nitem, fill=output, group=class)) +
    geom_bar(stat='identity', position='dodge', color='#ffffff') +
    scale_fill_manual(values=c(blue, red, black_fp, grey_tn), name="Legend") +
    guides(fill=guide_legend(ncol=2, byrow=FALSE)) +
    labs(title=title, x="Threshold", y="Number of Objects") +
    theme_void() +
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))
  
  if(scale_free) p <- p + facet_wrap(~ facet, scales='free') 
  else p <- p + facet_wrap(~ facet) 
  
  return(p)
}

classee_binary_var <- function(data=classee_sample_binary(), z=1.96, title=''){
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

classee_binary_var_facet <- function(data=classee_sample_binary_facet(), z=1.96, title='', scale_free = FALSE){
  blue <- '#1f8ac4'     
  black_fp <- '#333333'
  red <- '#bc112c'      
  grey_tn <- '#b3b3b3'   
  
  data <- data %>%
    mutate(output = factor(output, levels=c('TP','FN','FP','TN'))) %>%
    arrange(facet, threshold, output)
  
  data_var <- c()
  for(f in unique(data$facet)){
    slice_facet <- data %>% filter(facet == f)
    for(t in unique(data$threshold) %>% sort){
      slice <- slice_facet %>% filter(threshold == t)
      
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
    theme(legend.title=element_text(size=8), 
          legend.text=element_text(size=8))
  
  if(scale_free) p <- p + facet_wrap(~ facet, scales='free') 
  else p <- p + facet_wrap(~ facet) 
  
  return(p)
}


###########################
### SAMPLE DATA
###########################

######## BINARY ########
classee_sample_binary <- function(){
  thres <- 1:19/20
  m0 <- 0.4
  m1 <- 0.6
  sd0 <- sd1 <- 0.1
  n0 <- n1 <- 100
  
  tibble(threshold=thres) %>%
    mutate(TN = (pnorm(thres, m0, sd0)*n0) %>% round) %>%
    mutate(FN = (pnorm(thres, m1, sd1)*n1) %>% round) %>%
    mutate(FP = n0 - TN) %>%
    mutate(TP = n1 - FN) %>%
    mutate(TN = -1*TN) %>%
    mutate(FN = -1*FN) %>%
    gather(TN,FN,FP,TP,key='output',value='nitem') %>%
    mutate(threshold = as.character(threshold)) %>%
    mutate(class = ifelse(output %in% c('TP','FN'), 'C1', 'C0')) %>%
    return
}

######## BINARY WITH FACETS ########
classee_sample_binary_facet <- function(){
  data_facet <- tibble()
  for(i in 1:4){
    data_facet <- classee_sample_binary() %>% 
      mutate(facet = paste('Model',LETTERS[i])) %>% 
      rbind(data_facet)
  }
  return(data_facet)
}

######## MULTICLASS ########
classee_sample_multi <- function(){
  confmat <- rbind(c(316,0,8,23,51,0,1),
                   c(0,327,0,0,0,0,0),
                   c(0,0,122,6,25,4,0),
                   c(7,3,8,280,20,13,0),
                   c(7,0,192,13,234,0,5),
                   c(0,0,0,8,0,313,0),
                   c(0,0,0,0,0,0,324))
  colnames(confmat) <- paste('Actual',1:ncol(confmat))
  rownames(confmat) <- paste('Predicted',1:ncol(confmat))
  return(confmat)
}