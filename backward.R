base = as.formula(Sales~1)

for (i in colnames(sales_data)[2:9]) {
  base = as.formula(paste(deparse(base,width.cutoff = 100), "+", i))
  print(deparse(base))
}

base

backward = function(base) {
  prevbase = base
  s = summary(lm(base))
  p = as.list(s$coefficients[, 4])
  maxp = max(unlist(p))
  print(maxp)
  pnew = p[p < max(unlist(p))]
  pnew = pnew[2:length(pnew)]
  
  base = as.formula(Sales~1)
  
  for (i in names(pnew)) {
    base = as.formula(paste(deparse(base,width.cutoff = 100), "+", i))
  }
  if (maxp > 0.05) {
    backward(base)
  } else {
    return(prevbase)
  }
}

base = backward(base)
summary(lm(base))

