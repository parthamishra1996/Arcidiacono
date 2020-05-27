sim_data <- function(.){
  
  attach(.)
  
  Xm1 = ceiling(ceiling(theta[1]*s.val[1]/theta[2]) + (log(epsilon)/log(.5)))
  Xm2 = ceiling(ceiling(theta[1]*s.val[2]/theta[2]) + (log(epsilon)/log(.5)))
  
  calc_ccp <- function( s, Xm){
    tol = 10^(-6)
    error = 10^(1)
    choice_prob = rep(x=0.5, Xm + 1)
    temp =1
    while(error > tol ){
      temp = temp +1
      new_choice_prob = 1/(1 + exp(theta[1]*s - theta[2]*seq(Xm + 1) - beta*(log(choice_prob/choice_prob[1]))))
      error = max(abs(new_choice_prob - choice_prob))
      choice_prob = new_choice_prob
    }
    #print(temp)
    return(choice_prob)
  }
  
  #library(NMF)
  
  df2 <- data.frame(rmatrix(Xm1, pi.s*10^6) > calc_ccp(s.val[1], Xm1)) + 0
  df2_ <- data.frame(rmatrix(Xm2, ((1 - pi.s)*10^6) ) > calc_ccp(s.val[2], Xm2)) + 0
  
  #library(Hmisc)
  hist(array(apply(df2,2,match,x=1)))
  hist(array(apply(df2_,2,match,x=1)))
}