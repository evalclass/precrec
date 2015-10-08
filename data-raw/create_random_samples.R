B500 <- sample_rnd5levs(500, 500)
B1000 <- sample_rnd5levs(1000, 1000)
IB500 <- sample_rnd5levs(500, 5000)
IB1000 <- sample_rnd5levs(1000, 10000)

devtools::use_data(B500, B1000, IB500, IB1000, overwrite = TRUE)
