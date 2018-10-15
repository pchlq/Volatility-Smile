library(dplyr)

vola_delta <- function(tickers, Sh, Time, Shares_call, Shares_put){
  
  tickers_toupper <- lapply(tickers, function(x) toupper(x)) %>% unlist()
  
  for (tic in seq_along(tickers_toupper)){
    
    ticker = tickers_toupper[tic]
    
    from_smile = get(paste(ticker, 'smile', sep = '_'))
    from_X     = get(paste('X', ticker, sep = '_'))
    IV         = from_smile[1, 3] + seq(5, -5) * Sh
    sigma      = from_X[1,11]
    
    price_call = from_X[5, 5]
    price_put  = from_X[5, 2]
    
    get_greeks <- function(TypeFlag, S, price){
      fOptions::GBSGreeks(Selection = "delta", 
                          TypeFlag  = TypeFlag, 
                          S         = S, 
                          X         = price * 100,
                          Time      = Time, 
                          r         = 0, 
                          b         = 0, 
                          sigma     = sigma) 
    } # end get_greeks function
    
    
    options(scipen = 999)
    df <- data.frame(
                      vola = IV,
                      delta_call = lapply(IV, function(x) get_greeks(TypeFlag = "c",
                                                                     S        = x,
                                                                     price    = price_call)) %>% 
                                                          unlist() * Shares_call,
                      delta_put = lapply(IV, function(x) get_greeks(TypeFlag = "p",
                                                                    S        = x,
                                                                    price    = price_put)) %>% 
                                                         unlist() * Shares_put
    ) # end df
    
    assign(paste('delta', ticker, sep = '_'), df, envir = parent.frame())
  } # end 'for' loop
} # end vola_delta function


vola_delta(tickers = c("SPFB.Si", "SPFB.BR"), Sh = 2, Time = 90/364, Shares_call = 700, Shares_put = 1000)
