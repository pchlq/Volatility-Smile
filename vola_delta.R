library(dplyr)

vola_delta <- function(tickers, TypeFlag, Sh, Time, Shares){
  
  tickers_toupper <- lapply(tickers, function(x) toupper(x)) %>% unlist()
  
  for (tic in seq_along(tickers_toupper)){
    
    ticker     = tickers_toupper[tic]
    from_smile = get(paste(ticker, 'smile', sep = '_'))
    from_X     = get(paste('X', ticker, sep = '_'))
    IV         = from_smile[1, 3]
    sigma      = from_X[1,11]
    vl         = IV + seq(5, -5) * Sh
    
    if (TypeFlag == "c"){
      price = from_X[5, 5]
    } 
    else if (TypeFlag == "p") {
      price = from_X[5, 2]
    }
    
    options(scipen = 999)
    df <- data.frame(
                      vola = vl,
                      delta = lapply(vl, function(x) 
                                                    fOptions::GBSGreeks(Selection = "delta", 
                                                                        TypeFlag  = TypeFlag, 
                                                                        S         = x, 
                                                                        X         = price * 100,
                                                                        Time      = Time, 
                                                                        r         = 0, 
                                                                        b         = 0, 
                                                                        sigma     = sigma)) %>% 
                                                              unlist() * Shares
                    ) # end df
    
    assign(paste('delta', TypeFlag, ticker, sep = '_'), df, envir = parent.frame())
  } # and 'for' loop
} # end vola_delta function


vola_delta(tickers = c("SPFB.Si", "SPFB.BR"), TypeFlag = 'p', Sh = 2, Time = 90/364, Shares = 700)
