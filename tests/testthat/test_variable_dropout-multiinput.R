context("Check feature_importance() function")

library(keras)

df<- data.frame(id=rep(LETTERS[1:10], each=5), static=rep(1:10, each=5), time=rep(1:5, times=5))
df.cat<- data.frame(id=LETTERS[1:10], cat1=rep(LETTERS[1:5], times=2), cat2=letters[1:10])
df<- merge(df, df.cat)
df$x1<- df$time * df$static
df$x2<- rnorm(nrow(df), mean=df$time * df$static + 10, sd=5)
df$x3<- rnorm(nrow(df), mean=df$time * df$static * 3, sd=2)
df$y<- rnorm(nrow(df), mean=(df$x1 + df$x2) / df$x3, sd=2)

timevar<- "time"
idVars<- "id"
responseVars<- "y"
staticVars<- c("static", "cat1", "cat2")
predTemp<- c("x1", "x2", "x3")
responseTime<- max(df[, timevar], na.rm=TRUE)
regex_time<- "[0-9]+"
hidden_shape.RNN<- 8
hidden_shape.static<- 8
hidden_shape.main<- 16
epochs<- 3
batch_size<- length(unique(df$id))
verbose<- 0

wideTo3Darray.ts<- function(d, vars, idCols){
  d<- as.data.frame(d)
  timevals<- unique(gsub(paste0("^(", paste(vars, collapse="|"), ")_"), "", setdiff(colnames(d), idCols)))

  # Reshape to a 3D array [samples, timesteps, features] Format for RNN layers in NN
  a<- lapply(vars, function(x){
    varTS<- d[, grep(paste0("^", x, "_(", paste(timevals, collapse="|"), ")$"), colnames(d))]
    a<- array(as.matrix(varTS), dim=c(nrow(varTS), ncol(varTS), 1), dimnames=list(case=NULL, t=gsub(paste0("^", x, "_"), "", colnames(varTS)), var=x))
  })
  names(a)<- vars
  a<- abind::abind(a)
  names(dimnames(a))<- c("case", "t", "var")
  dimnames(a)$case<- do.call(paste, c(d[, idCols, drop=FALSE], list(sep="_")))

  return(a)
}

build_modelLTSM<- function(input_shape.ts, input_shape.static=0, output_shape=1,
                           hidden_shape.RNN=32, hidden_shape.static=16, hidden_shape.main=32){
  inputs.ts<- layer_input(shape=input_shape.ts, name="TS_input")
  inputs.static<- layer_input(shape=input_shape.static, name="Static_input")

  predictions.ts<- inputs.ts
  for (i in 1:length(hidden_shape.RNN)){
    predictions.ts<- predictions.ts %>% layer_lstm(units=hidden_shape.RNN[i], name=paste0("LSTM_", i))
  }

  if (input_shape.static > 0){
    predictions.static<- inputs.static
    for (i in 1:length(hidden_shape.static)){
      predictions.static<- predictions.static %>% layer_dense(units=hidden_shape.static[i], name=paste0("Dense_", i))
    }
    output<- layer_concatenate(c(predictions.ts, predictions.static))
  } else {
    output<- predictions.ts
  }

  for (i in 1:length(hidden_shape.main)){
    output<- output %>% layer_dense(units=hidden_shape.main[i], name=paste0("main_dense_", i))
  }
  output<- output %>% layer_dense(units=output_shape, name="main_output")

  if (input_shape.static > 0){
    model<- keras_model(inputs=c(inputs.ts, inputs.static), outputs=output)
  } else {
    model<- keras_model(inputs=inputs.ts, outputs=output)
  }

  compile(model, loss="mse", optimizer=optimizer_rmsprop())

  model
}

predVars<- setdiff(colnames(df), c(idVars, timevar))
predVars.cat<- names(which(!sapply(df[, predVars, drop=FALSE], is.numeric)))
predVars.num<- setdiff(predVars, predVars.cat)

df.catBin<- stats::model.matrix(stats::as.formula(paste("~ -1 +", paste(predVars.cat, collapse="+"))), data=df)
predVars.catBin<- colnames(df.catBin)
df<- cbind(df[, setdiff(colnames(df), predVars.cat)], df.catBin)
predVars<- c(predVars.num, predVars.catBin)
staticVars.cat<- staticVars[staticVars %in% predVars.cat]
staticVars<- c(setdiff(staticVars, staticVars.cat), predVars.catBin)

# crossvalidation for timeseries must be done in the wide format data
responseVars.ts<- paste0(responseVars, "_", responseTime)
predVars.tf<- paste0(setdiff(predVars, staticVars), "_", responseTime)

## df to wide format
dt<- data.table::as.data.table(df)
staticCols<- c(idVars, staticVars)
vars<- setdiff(colnames(dt), c(staticCols, timevar))
timevals<- unique(data.table:::`[.data.table`(x=dt, , j=timevar, with=FALSE))[[1]] # without importing data.table functions
LHS<- setdiff(staticCols, timevar)
form<- paste0(paste(LHS, collapse=" + "), " ~ ", timevar)
dt<- data.table::dcast(dt, formula=stats::formula(form), value.var=vars)  # To wide format (var_time columns)
df.wide<- as.data.frame(dt)
predVars.ts<- setdiff(colnames(df.wide), c(idVars, staticVars)) # WARNING: Includes responseVars.ts
timevals<- unique(df[[timevar]])

idxTrain<- sample(1:nrow(df.wide), 6)

train_y<- df.wide[idxTrain, c(idVars, responseVars.ts), drop=FALSE]
train_data<- df.wide[idxTrain, c(idVars, staticVars, predVars.ts), drop=FALSE]

test_y<- df.wide[-idxTrain, c(idVars, responseVars.ts), drop=FALSE]
test_data<- df.wide[-idxTrain, c(idVars, staticVars, predVars.ts), drop=FALSE]


# Reshape data to 3D arrays [samples, timesteps, features] as expected by LSTM layer
train_data.3d<- wideTo3Darray.ts(d=train_data, vars=setdiff(predVars, staticVars), idCols=idVars)
test_data.3d<- wideTo3Darray.ts(d=test_data, vars=setdiff(predVars, staticVars), idCols=idVars)

train_data.static<- structure(as.matrix(train_data[, staticVars, drop=FALSE]),
                              dimnames=list(case=do.call(paste, c(train_data[, idVars, drop=FALSE], list(sep="_"))), var=staticVars))
test_data.static<- structure(as.matrix(test_data[, staticVars, drop=FALSE]),
                             dimnames=list(case=do.call(paste, c(test_data[, idVars, drop=FALSE], list(sep="_"))), var=staticVars))

train_y<- structure(as.matrix(train_y[, responseVars.ts, drop=FALSE]),
                         dimnames=list(case=do.call(paste, c(train_y[, idVars, drop=FALSE], list(sep="_"))), var=responseVars.ts))
test_y<- structure(as.matrix(test_y[, responseVars.ts, drop=FALSE]),
                        dimnames=list(case=do.call(paste, c(test_y[, idVars, drop=FALSE], list(sep="_"))), var=responseVars.ts))

train_data.3d<- train_data.3d[, setdiff(dimnames(train_data.3d)[[2]], responseTime), ]
test_data.3d<- test_data.3d[, setdiff(dimnames(train_data.3d)[[2]], responseTime), ]

train_data<- list(TS_input=train_data.3d, Static_input=train_data.static)
test_data<- list(TS_input=test_data.3d, Static_input=test_data.static)

modelNN<- build_modelLTSM(input_shape.ts=dim(train_data.3d)[-1], input_shape.static=length(staticVars), output_shape=length(responseVars),
                          hidden_shape.RNN=hidden_shape.RNN, hidden_shape.static=hidden_shape.static, hidden_shape.main=hidden_shape.main)
history<- keras::fit(object=modelNN, x=train_data, y=train_y, batch_size=batch_size,
                     epochs=epochs, verbose=verbose, validation_data=list(test_data, test_y))

## 3D data only in a separate PR
# modelNN.LSTM<- build_modelLTSM(input_shape.ts=dim(train_data.3d)[-1], input_shape.static=0, output_shape=length(responseVars),
#                                hidden_shape.RNN=hidden_shape.RNN, hidden_shape.static=0, hidden_shape.main=hidden_shape.main)
# history<- keras::fit(object=modelNN.LSTM, x=train_data.3d, y=train_y, batch_size=batch_size,
#                      epochs=epochs, verbose=verbose, validation_data=list(test_data.3d, test_y))


# Basics - tests with improper and proper inputs

test_that("Output glm",{
  vd_keras <- feature_importance(x = modelNN, data = test_data, y = test_y,
                                 type = "raw", loss_function = loss_root_mean_square)
  expect_true("feature_importance_explainer" %in% class(vd_keras))
})


# Permutations and subsampling

test_that("feature_importance gives slightly different output on subsequent runs", {
  result_1 <- feature_importance(x = modelNN, data = test_data, y = test_y,)
  result_2 <- feature_importance(x = modelNN, data = test_data, y = test_y,)
  change_12 <- abs(result_1$dropout_loss - result_2$dropout_loss)
  expect_gt(sum(change_12), 0)
})


test_that("feature_importance records number of permutations", {
  result <- feature_importance(x = modelNN, data = test_data, y = test_y, B = 2)
  expect_false(is.null(attr(result, "B")))
  expect_equal(attr(result, "B"), 2)
  expect_equal(max(result$permutation), 2)
  expect_equal(dim(result[result$permutation != 0,]), c(2*nrow(result)/3, 4))
  # because there is no sub-sampling, all the full-model results should be equal
  loss_full <- result[result$variable=="_full_model_",]
  expect_equal(length(unique(loss_full$dropout_loss)), 1)
})

#
# test_that("feature_importance avoids reporting permutations when only one performed", {
#   # by default, one permutation leads to no raw_permutations component
#   result_default <- feature_importance(explainer_rf, B = 1)
#   expect_true(is.null(attr(result_default, "raw_permutations")))
#   result_keep <- feature_importance(explainer_rf, B = 1, keep_raw_permutations = TRUE)
#   expect_false(is.null(attr(result_keep, "raw_permutations")))
# })
#
#
# test_that("feature_importance can avoid recording permutation details", {
#   result <- feature_importance(explainer_rf, B = 2, keep_raw_permutations = FALSE)
#   expect_true(is.null(attr(result, "raw_permutations")))
#   # when keep_raw_permutations is off, output should still signal number of permutations
#   expect_false(is.null(attr(result, "B")))
#   expect_equal(attr(result, "B"), 2)
# })


## Too few cases?
# test_that("feature_importance with subsampling gives different full-model results ", {
#   result <- feature_importance(x = modelNN, data = test_data, y = test_y, B = 2, N=2)
#   # the full model losses should be different in the first and second round
#   # because each round is based on different rows in the data...
#   # but in principle there is a tiny probability the two rounds are based on the same rows
#   loss_full <- result[result$variable=="_full_model_", ]
#   expect_equal(length(unique(loss_full$dropout_loss)), 3)
# })


test_that("feature_importance performs at least one permutation", {
  result <- feature_importance(x = modelNN, data = test_data, y = test_y, B = 0.1)
  expect_false(is.null(attr(result, "B")))
  expect_equal(attr(result, "B"), 1)
})


## Too few cases?
# test_that("feature_importance averaged over many permutations are stable", {
#   # this test uses many permutations, so make a very small titanic dataset for speed
#   tiny <- titanic_small[titanic_small$age > 50,]
#   tiny$country <- tiny$class <- tiny$sibsp <- tiny$embarked <- tiny$gender <- NULL
#
#   tiny_rf <- ranger(survived ~ parch + fare + age, data = tiny, probability = TRUE)
#   tiny_explainer = explain(tiny_rf, data = tiny,
#                            y = tiny$survived == "yes", label = "RF")
#   # compute single permutations importance values
#   result_1 <- feature_importance(tiny_explainer, B = 1)
#   result_2 <- feature_importance(tiny_explainer, B = 1)
#   # compute feature importance with several permutations
#   result_A <- feature_importance(tiny_explainer, B = 40)
#   result_B <- feature_importance(tiny_explainer, B = 40)
#   # two rounds with many permutation should give results closer together
#   # than two rounds with single permutations
#   change_12 <- abs(result_1[result_1$permutation == 0, "dropout_loss"] - result_2[result_2$permutation == 0, "dropout_loss"])
#   change_AB <- abs(result_A[result_A$permutation == 0, "dropout_loss"] - result_B[result_B$permutation == 0, "dropout_loss"])
#   # this test should succeed most of the time... but in principle could fail by accident
#   expect_lt(sum(change_AB), sum(change_12))
# })




# Variable grouping

v_groups.ts <- mapply(function(dimVar, dimNames) {
  v<- lapply(dimVar, function(v) setNames(list(v), dimNames))
  setNames(v, nm = dimVar)
}, dimVar=dimnames(test_data$TS_input)[-1], dimNames=names(dimnames(test_data$TS_input))[-1], SIMPLIFY=FALSE)
v_groups.ts <- do.call(c, v_groups.ts)

v_groups.tsCombDim <- expand.grid(dimnames(test_data$TS_input)[-1], stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE) # All combinations for all dimensions in a dataset
rownames(v_groups.tsCombDim) <- apply(v_groups.tsCombDim, 1, function(v) paste(v, collapse="|"))
v_groups.tsCombDim <- split(v_groups.tsCombDim, rownames(v_groups.tsCombDim))
v_groups.tsCombDim <- lapply(v_groups.tsCombDim, as.list)

v_groups.static<- list(list("static"),
                       list(grep("^cat1", dimnames(test_data$Static_input)$var, value=TRUE)),
                       list(grep("^cat2", dimnames(test_data$Static_input)$var, value=TRUE)))
names(v_groups.static)<- c("static", "cat1", "cat2")
variable_groups<- list(TS_input=v_groups.ts, Static_input=v_groups.static)
variable_groups.combDim<- list(TS_input=v_groups.tsCombDim, Static_input=v_groups.static)

variable_groups.noGrNames<-lapply(variable_groups, function(input){
  names(input)<- NULL
  input
})

variable_groups.combDim.noGrNames<-lapply(variable_groups.combDim, function(input){
  names(input)<- NULL
  input
})

## For visually inspect the construction of variable_groups names
# variable_groups.noVarNames<-lapply(variable_groups, function(input){
#   lapply(input, function(gr){
#     names(gr)<- NULL
#     gr
#   })
# })
# variable_groups.noGrVarNames<-lapply(variable_groups, function(input){
#   names(input)<- NULL
#   lapply(input, function(gr){
#     names(gr)<- NULL
#     gr
#   })
# })
# variable_groups.combDim.noVarNames<-lapply(variable_groups.combDim, function(input){
#   lapply(input, function(gr){
#     names(gr)<- NULL
#     gr
#   })
# })
# variable_groups.combDim.noGrVarNames<-lapply(variable_groups.combDim, function(input){
#   names(input)<- NULL
#   lapply(input, function(gr){
#     names(gr)<- NULL
#     gr
#   })
# })

test_that("Variable groupings validation", {
  result <- feature_importance(x = modelNN, data = test_data, y = test_y,
                              loss_function = loss_root_mean_square,
                              variable_groups = variable_groups)
  expect_is(result, "feature_importance_explainer")
})


test_that("Variable groupings validation with combinations of variables from 2 different dimensions", {
  result <- feature_importance(x = modelNN, data = test_data, y = test_y,
                               variable_groups = variable_groups.combDim)
  expect_is(result, "feature_importance_explainer")
})


test_that("Variable groupings input validation checks", {
  expect_warning(feature_importance(x = modelNN, data = test_data, y = test_y,
                                    loss_function = loss_root_mean_square,
                                    variable_groups = variable_groups.noGrNames),
                 "You have passed an unnamed list. The names of variable groupings will be created from variables names.")

  expect_error( feature_importance(x = modelNN, data = test_data, y = test_y,
                                   loss_function = loss_root_mean_square,
                                   variable_groups = c("x1", "cat1A")
                                   ),
                "variable_groups should be of class list contining lists for each data input")

  variable_groups.wrong<- variable_groups
  variable_groups.wrong$TS_input$t.1<- "-1"
  variable_groups.wrong$Static_input$cat1[1]<- "wrong"
  expect_error(feature_importance(x = modelNN, data = test_data, y = test_y,
                                  loss_function = loss_root_mean_square,
                                  variable_groups = variable_groups.wrong),
               "You have passed wrong variables names in variable_groups argument")
  variable_groups.wrong<- variable_groups
  variable_groups.wrong$Static_input$cat1[[1]]<- as.list(variable_groups.wrong$Static_input$cat1[[1]])
  expect_error(feature_importance(x = modelNN, data = test_data, y = test_y,
                                  loss_function = loss_root_mean_square,
                                  variable_groups = variable_groups.wrong),
               "Elements of variable_groups argument should be of class character")
})




# Output types

test_that("feature_importance with type ratio", {
  # type "ratio" gives $dropout_loss normalized by _full_model_
  result <- feature_importance(x = modelNN, data = test_data, y = test_y, type="ratio")
  expect_equal(result$dropout_loss[result$variable=="_full_model_" & result$permutation == 0], 1)
})


test_that("feature_importance with type difference", {
  # type "difference" gives $dropout_loss with _full_model_ subtracted
  result <- feature_importance(x = modelNN, data = test_data, y = test_y, type="difference")
  expect_equal(result$dropout_loss[result$variable=="_full_model_" & result$permutation == 0], 0)
})

test_that("Inverse sorting of bars",{
  result <- feature_importance(x = modelNN, data = test_data, y = test_y, type="difference")

  expect_error(plot(result, desc_sorting = "desc"))
})
