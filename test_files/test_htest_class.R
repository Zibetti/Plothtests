htest <- list(
  statistic  = statistic,
  parameter  = c(df=test.object$df),
  p.value    = test.object$p.value,
  estimate   = estimate,
  null.value = c("difference in correlations"=result.object@null.value),
  alternative= result.object@alternative,
  method     = all.tests[[test]],
  data.name  = data.description(result.object@data.name, result.object@var.labels),
  conf.int   = conf.int
)
class(htest) <- "htest"
htest.list[[test]] <- htest
htest.list

# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/htest.object