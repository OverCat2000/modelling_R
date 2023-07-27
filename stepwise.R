model = function(base) {
  FM = update(base, .~. + para)
  return(FM)
}


value = function(base) {
  p = c()
  for (i in 2:length(sales_data)) {
    para <<- sales_data[[i]]
    #print(model(para))
    anv = anova(base, model(base))
    print(anv$`Pr(>F)`[2])
    if(is.na(anv$`Pr(>F)`[2])) {
      p = append(p, 1000)
    } else {
      p = append(p, anv$`Pr(>F)`[2])
    }
  }
  
  if(min(p) < 0.05) {
    ind = which(p == min(p))
    term = colnames(sales_data)[ind + 1]
    base <<- lm(as.formula(paste(deparse(as.formula(base)), "+", term)))
  } else {
    print("end")
  }
}

para = Time

base = lm(Sales~1)

value(base)

value(base)
summary(base)

value(base)
summary(base)

value(base)
summary(base)

value(base)
summary(base)





