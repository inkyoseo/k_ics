#' Square a value
#'
#' @param x A numeric value
#'
#' @return a number
#' @export
#' @importFrom dplyr %>%
#' @import dplyr ( >= 0.8.5)
#' @import agop
#' @import copula
#' @examples
#' generate_risk(seed = 1)
generate_risk <- function(seed) {
  if(!is.null(seed)){set.seed(seed)}
  obj.cop <- copula::gumbelCopula(param = 1.38, dim=4)
  samples.cop <- copula::rCopula(120000, obj.cop)
  cor(samples.cop)
  mktlsd <- sqrt(2*(log(500)-6))
  
  tibble::tibble(
  pop.risk_life = (qnorm(    samples.cop[,1], mean=500, sd=1000) - 500),
  pop.risk_pnc = (qpareto2( samples.cop[,2], k=4     , s=1500)  - 500),
  pop.risk_cred = (qt(       samples.cop[,3], df=4)*1000+500     - 500),
  pop.risk_mkt = (exp(qnorm(samples.cop[,4], mean=6, sd=mktlsd))- 500)
  ) %>% as.data.frame() %>%
    mutate(pop.risk_total = (pop.risk_life + pop.risk_pnc + pop.risk_cred + pop.risk_mkt))
}
