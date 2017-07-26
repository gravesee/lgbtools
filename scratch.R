
#### Description
#
# testing ideas for creating a pretty.tree function for lgb
#
#
####

library(lightgbm)
library(onehot)

data(titanic, package="binnr")

s <- sample(nrow(titanic), nrow(titanic)/2)

oh <- onehot(titanic[-1], addNA = TRUE, stringsAsFactors = TRUE, max_levels = 10)
X <- predict(oh, titanic[-1], sparse = TRUE)

train <- lgb.Dataset(data=X[s,], label=titanic$Survived[s], free_raw_data = FALSE)
test <- lgb.Dataset(data=X[-s,], label=titanic$Survived[-s], free_raw_data = FALSE)

valids <- list(train=train, test=test)


bst <- lgb.train(data = train,
  num_leaves = 5,
  learning_rate = .1,
  nrounds = 1000,
  valids = valids,
  nthread = 4,
  objective = "binary")

i <- which.min(bst$record_evals$test$binary_logloss$eval)

lgb.Dataset.save(test, "test.buffer")
p <- predict(bst, X[-s,], rawscore = TRUE, num_iteration = i)

plot(pROC::roc(titanic$Survived[-s], p))


tmp <- tempfile()

m <-

  library(rjson)

m <- fromJSON(bst$dump_model(num_iteration = i))

tree <- m$tree_info[[1]]$tree_structure

l <- tree$right_child$right_child$right_child

###
entry <- function(leaf, i) {
  data.frame(
    node_id = i,
    left = -1,
    right = -1,
    split_index = -1,
    split_feature = -1,
    split_gain = -1,
    threshold = -1,
    decision_type = -1,
    default_value = -1,
    internal_value = leaf$leaf_value,
    internal_count = leaf$leaf_count, stringsAsFactors = F)
}

## recurse the trees and gather info
recurse <- function(tree, i) {

  if (!is.null(tree$leaf_index)) {
    return(entry(tree, i))
  }

  l <- 2 * i + 1
  r <- 2 * i + 2

  res <- data.frame(c(list(node_id=i, left=l, right=r), tree[1:8]), stringsAsFactors = F)

  return(rbind(res, recurse(tree$right_child, l), recurse(tree$left_child, r)))

}


res <- recurse(tree, 0)
res[order(res[,'node_id']),]






lightgbm:::predict.lgb.Booster(bst, X[s,], predleaf = TRUE)

