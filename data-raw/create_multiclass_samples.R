# A 3-class sample: 150 observations, 50 per class, and one score column per
# class. The three classes are separated to different degrees on purpose, so
# that a one-vs-rest evaluation of them has something to show.
set.seed(1)

np <- 50
classes <- c("c1", "c2", "c3")
labels <- rep(classes, each = np)

# Each column scores its own class higher, by a shrinking margin
margins <- c(c1 = 2.5, c2 = 1.2, c3 = 0.3)
scores <- vapply(classes, function(cl) {
  s <- stats::rnorm(np * length(classes), 0, 1)
  s[labels == cl] <- s[labels == cl] + margins[[cl]]
  s
}, numeric(np * length(classes)))
colnames(scores) <- classes

C3N150 <- list(scores = scores, labels = labels)

usethis::use_data(C3N150, overwrite = TRUE)
