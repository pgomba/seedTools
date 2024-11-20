#' GerminaR functions (Lozano-Isla et al., 2019) adapted and modified to work with decimal scoring times and allow for users to  choose the letters to prepend to scoring times.
#'
#' Number of germinated seeds
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data
#'
f_ngs <- function(evalName, data){

  evd <-  data %>%
    dplyr::select(starts_with({{evalName}}))

  ger <- apply(cbind(evd), 1, sum, na.rm = TRUE)
  ger

}

#' GerminaR functions (Lozano-Isla et al., 2019) adapted and modified to work with decimal scoring times and allow for users to  choose the letters to prepend to scoring times.
#'
#' Germination percentage
#' @param evalName A character that prepend scoring time
#' @param SeedN Number of seeds per replicate, or dataframe column with seed number per dish
#' @param data dataframe with data


f_gep <- function(SeedN, evalName, data){

  sdn <- data[, SeedN]
  sdn <- as.numeric(sdn)
  grs <- f_ngs(evalName, data)
  tmp <- grs/sdn * 100
  tmp

}


#' GerminaR functions (Lozano-Isla et al., 2019) adapted and modified to work with decimal scoring times and allow for users to  choose the letters to prepend to scoring times.
#'
#' Mean germination time
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data

f_mgt <- function(evalName, data){

  ger <- f_ngs(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  evcn <- colnames(evd)
  time <- as.numeric(gsub(paste0("^", evalName), "", evcn))
  tmp <- t(t(evd) * time)
  rst <- apply(cbind(tmp),1, sum, na.rm = T)
  rst/ger

}

#' GerminaR functions (Lozano-Isla et al., 2019) adapted and modified to work with decimal scoring times and allow for users to  choose the letters to prepend to scoring times.
#'
#' Mean germination rate
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data

f_mgr <- function(evalName, data){

  mgt <- f_mgt(evalName, data)
  rst <- 1/mgt
  rst

}


#' Germination uncertainty, from Labouriau, G. (1983) adapted and modified to work with decimal scoring times and allow for users to  choose the letters to prepend to scoring times.
#'
#' Adaptation of Shannon index measures the degree of uncertainty in predicting the informational entropy or uncertainty associated with the distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). Low values indicate frequencies with short peaks, i.e. the more concentrated the germination in time.
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data

f_unc <- function(evalName, data){

  grs <- f_ngs(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  tmp <- evd/grs * log2(evd/grs)
  rst <- apply(tmp, 1, sum, na.rm = TRUE)
  abs(rst)

}

#' Germination Synchronization Index, from Ranal, M. A., & Santana, D. G. D. (2006)
#'
#' = 1 when germination of all the seeds occurs at the same time and = 0 when at least two seeds can germinate one each time. Thus, the value of synchrony assessments is the grade of overlap between seed germination.
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data

f_syn <- function(evalName, data){

  grs <- f_ngs(evalName, data)
  evd <- data %>% dplyr::select(starts_with(evalName))
  tmp <- evd*(evd - 1)/2
  cal <- apply(tmp ,1, sum, na.rm = TRUE)
  rst <- cal/(grs*(grs-1)/2)
  rst

}

#' Variance of the Mean Germination Time
#'
#' @param Rseq no idea
#' @param Nrow no idea

rep_row <- function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
}


#' Unknown
#'
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data

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

#' Standard deviation of the Mean Germination Time
#'
#' @param evalName A character that prepend scoring time
#' @param data dataframe with data

f_sdt <- function(evalName, data){

  vgt <- f_vgt(evalName, data)
  rst <- sqrt(vgt)
  rst

}

#' Unknown 2
#' @param evalName A character that prepend scoring time
#' @param SeedN Number of seeds per replicate, or dataframe column with seed number per dish
#' @param data dataframe with data

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
