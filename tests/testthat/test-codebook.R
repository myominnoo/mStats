context("codebook")

test_that("data.frame ok", {
    df <- data.frame(x = c(1, 1), y = c(2, 2))
    df <- codebook(df)
    df$Obs_Num
    expect_equal(df$Obs_Num, c("-------", 2, 2, "-------"))
})
