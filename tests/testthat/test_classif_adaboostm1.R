context("classif_adaboostm1")

test_that("classif_adaboostm1", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(W =list(J48, M = 30)),
    list(W = list("DecisionStump"))

  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::AdaBoostM1(formula = binaryclass.formula, data = binaryclass.train, control = ctrl)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = binaryclass.test, type = "probability")
    old.probs.list[[i]] = p[, 1]
    old.predicts.list[[i]] = as.factor(binaryclass.class.levs[ifelse(p[, 2] > 0.5, 2, 1)])

  }


  testSimpleParsets("classif.adaboostm1", binaryclass.df, binaryclass.target,
                    binaryclass.train.inds, old.predicts.list, parset.list)

  testProbParsets("classif.adaboostm1", binaryclass.df, binaryclass.target,
                  binaryclass.train.inds, old.probs.list, parset.list)
})

# test_that("classif_adaboostm1 passes parameters correctly to rpart.control (#732)", {
#   cp.vals = c(0.022, 0.023)
#   loss.vals = c("exponential", "logistic")
#   for (cp in cp.vals) {
#     for (loss in loss.vals) {
#       lrn = makeLearner("classif.adaboostm1", cp = cp, loss = loss)
#       mod = getLearnerModel(train(lrn, pid.task))
#       expect_equal(mod$model$trees[[1]]$control$cp, cp)
#       expect_equal(mod$model$lossObj$loss, loss)
#     }
#   }
# })
