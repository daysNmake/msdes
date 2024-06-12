

#### Put objects here that change as we loop through treatments for each patient (e.g. events can affect fl.tx, but events do not affect mat.mort.s)
#common across trt but changes per pt could be implemented here (if (trt==)... )
unique_pt_inputs <- add_item(
  f.edss0   = 0,
  f.edss1   = 0,
  f.edss2   = 0,
  f.edss3   = 0,
  f.edss4   = 0,
  f.edss5   = 0,
  f.edss6   = 0,
  f.edss7   = 0,
  f.edss8   = 0,
  f.edss9   = 0,
  f.rrms    = 0,
  f.spms    = 0,
  f.relapse = 0,
  f.line1   = 0,
  f.line2   = 0, 
  f.line3   = 0,
  f.line4   = 0,
  f.tx.on   = 0,
  f.tx.off  = 0,
  f.tx.nat  = 0,
  f.tx.clab = 0,
  f.tx.fing = 0,
  f.tx.dac  = 0,
  f.tx.alem = 0,
  f.tx.ib1a = 0,
  f.tx.ib1b = 0,
  f.tx.glat = 0,
  f.tx.ofat = 0,
  f.tx.pone = 0,
  f.tx.ahsct = 0,
  f.rr2sp             = rbernoulli(1,0.1) #10% probability of converting from rrms to spms
)

