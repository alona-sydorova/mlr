#' @export
makeRLearner.classif.adaboostm1 = function() {
  makeRLearnerClassif(
    cl = "classif.adaboostm1",
    package = c("RWeka", "rpart"),
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "W", default = "DecisionStump"),
      makeLogicalLearnerParam(id = "output-debug-info", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(xval = 0L),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "ada Boosting M1",
    short.name = "adaboostm1",
    note = "",
    callees = c("AdaBoostM1", "Weka_control")
  )
}

#' @export
trainLearner.classif.adaboostm1 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  f = getTaskFormula(.task)
  ctrl = RWeka::Weka_control(...)
  RWeka::AdaBoostM1(f, data = getTaskData(.task, .subset), control = ctrl, na.action = na.pass)
}

#' @export
predictLearner.classif.adaboostm1 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}
