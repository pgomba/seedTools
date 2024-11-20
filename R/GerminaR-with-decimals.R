
#Functions were adapted from the GerminaR package (Lozano-Isla et al., 2019) to work with decimal scoring times. Additionally, it allows users to choose the letters to prepend to scoring times.

# Lozano-Isla, Flavio; Benites-Alfaro, Omar Eduardo; Pompelli, Marcelo Francisco (2019). GerminaR: An R package for germination analysis with the interactive web application “GerminaQuant for R.” Ecological Research, 34(2), 339–346. https://doi.org/10.1111/1440-1703.1275

require(tidyverse)

# number of germinated seeds
f_ngs <- function(evalName, data){
  
  evd <-  data %>% 
    dplyr::select(starts_with({{evalName}})) 
  
  ger <- apply(cbind(evd), 1, sum, na.rm = TRUE)
  ger
  
}

# germination percentage
f_gep <- function(SeedN, evalName, data){
  
  sdn <- data[, SeedN]
  sdn <- as.numeric(sdn)
  grs <- f_ngs(evalName, data)
  tmp <- grs/sdn * 100
  tmp
  
}

# mean germination time
f_mgt <- function(evalName, data){
  
  ger <- f_ngs(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  evcn <- colnames(evd)
  time <- as.numeric(gsub(paste0("^", evalName), "", evcn))
  tmp <- t(t(evd) * time)
  rst <- apply(cbind(tmp),1, sum, na.rm = T)
  rst/ger
  
}

# mean germination rate
f_mgr <- function(evalName, data){
  
  mgt <- f_mgt(evalName, data)
  rst <- 1/mgt
  rst
  
}

# Germination Uncertainty
# Adaptation of Shannon index measures the degree of uncertainty in predicting the informational entropy or uncertainty associated with the distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). Low values indicate frequencies with short peaks, i.e. the more concentrated the germination in time. 
#' GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington.
#' LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983.

f_unc <- function(evalName, data){
  
  grs <- f_ngs(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  tmp <- evd/grs * log2(evd/grs)
  rst <- apply(tmp, 1, sum, na.rm = TRUE)
  abs(rst)
  
}

# Germination Synchronization Index
# = 1 when germination of all the seeds occurs at the same time and = 0 when at least two seeds can germinate one each time. Thus, the value of synchrony assessments is the grade of overlap between seed germination.
# RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process? 
# Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.

f_syn <- function(evalName, data){
  
  grs <- f_ngs(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  tmp <- evd*(evd - 1)/2
  cal <- apply(tmp ,1, sum, na.rm = TRUE)
  rst <- cal/(grs*(grs-1)/2)
  rst
  
}

# Variance of the Mean Germination Time
rep_row <- function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
}

f_vgt <- function(evalName, data){
  
  grs <- f_ngs(evalName, data)
  mgt <- f_mgt(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  day <- 0:(ncol(evd)-1)
  dym <- rep_row(day,nrow(evd)) # Matrix for product of matrix
  tmp <- evd * (dym-mgt)^2
  cal <- apply(tmp, 1, sum, na.rm = TRUE)
  rst <- cal/(grs-1)
  rst
  
}

# Standard deviation of the Mean Germination Time
f_sdt <- function(evalName, data){
  
  vgt <- f_vgt(evalName, data)
  rst <- sqrt(vgt)
  rst
  
}

f_germ_sum <- function(SeedN, evalName, data){
  
  evf <- data %>% 
    dplyr::select(!starts_with({{evalName}})) 
  
  gsm <- mutate( evf,
                 num_germ_seeds = f_ngs(evalName, data), 
                 germ_perc = f_gep(SeedN, evalName, data),
                 MGT = f_mgt(evalName, data),
                 MGR = f_mgr(evalName, data),
                 unc = f_unc(evalName, data),
                 syn = f_syn(evalName, data),
                 vgt = f_vgt(evalName, data),
                 sdg = f_sdt(evalName, data)
  )
}  
