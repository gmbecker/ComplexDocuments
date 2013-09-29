suppressMessages(library(lattice))
suppressMessages(library(rpart))
suppressMessages(library(e1071))
                
findNeighbors = function(distMat, k, newdat = NULL, trainingdat = NULL) {
    if (!is.null(newdat) && !is.null(trainingdat)) 
        distMat = as.matrix(dist(rbind(newdat, trainingdat)))[1:row(newdat), 
            -(1:nrow(newdat))]

    ret = apply(distMat, 1, function(row) order(row)[1:k])
    if (!is.matrix(ret)) 
        ret = matrix(ret, nrow = 1)

    ret
}

doKNN = function(traindat, newdat, distMat = NULL, truthTrain, k = 3) {
    if (is.null(distMat)) {
        distMat = as.matrix(dist(rbind(newdat, traindat)))[1:nrow(newdat), -(1:nrow(newdat))]
    }

    neighbors = findNeighbors(distMat = distMat, k = k)
    apply(neighbors, 2, function(row, truth) {
        neighborClasses = truth[row]
        names(sort(table(neighborClasses), decreasing = TRUE))[1]
    }, truth = truthTrain)
}

getRates = function(preds, true)
{
    by_dig = mapply(function(true, pr) sum(true != pr), 0:9, split(preds, true))
    rel_dig = by_dig / tapply(preds, true, length)
    all = sum(by_dig)/length(true)
    names(by_dig) = names(rel_dig) = unique(true)
    list(abs_dig = by_dig, rel_dig = rel_dig, overall = all)
}

load_digit_data = function(train_per_digit = 50, test_per_digit = 100)
{
    if(!exists("sampleTrain"))
        load("digitsTrain.rda")
    if(!exists("test"))
        load("digitsKnownTest.rda")
    inds = lapply(split(seq(1, nrow(sampleTrain)), sampleTrain$label), sample, size=train_per_digit)
    train = sampleTrain[unlist(inds), ]
    inds2 = lapply(split(seq(1, nrow(test)), test$label), sample, size = test_per_digit)
    testvals = test[unlist(inds2),]
    assign("train", train, .GlobalEnv)
    assign("testvals", testvals, .GlobalEnv)
    invisible(NULL)
}
