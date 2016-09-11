# Ch 3 policy 2 - similar to policy one except no holding time and allows for multiple orders

policy.2 <- function(signals, market, opened.pos, money,
                     bet = 0.2,
                     exp.prof = 0.025, max.loss = 0.05)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] =='b'){
    quant <- round(bet*money/market[d,'Close'],0)
    if (quant > 0)
      # Note when orders are sent three orders are made 1. open a position, 2. limit order for profit, 3. stop loss
      orders <- rbind(orders, 
                      data.frame(order = c(1,-1,-1), order.type = c(1,2,3),
                                 val = c(quant, 
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)),
                                 action = c('open', 'close', 'close'),
                                 posID = c(NA,NA,NA)))
    # ii) short positions
  } else if (signals[d] == 's') {
    # this is the number of stocks we already need to buy
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type'] == -1, "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'], 0)
    if (quant > 0)
      orders <- rbind(orders, 
                      data.frame(order = c(-1,1,1), order.type = c(1,2,3),
                                 val = c(quant, 
                                         market[d,'Close']*(1-exp.prof),
                                         market[d,'Close']*(1+max.loss)),
                                 action = c('open', 'close', 'close'),
                                 posID = c(NA,NA,NA)))
  }
  
  orders
}