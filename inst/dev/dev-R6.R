

devtools::load_all()





df <- data.frame(x = 1:10) |>
	labelled::set_variable_labels(x = "dummy x")
codebook(df)

df2 <- base::transform(df, x = cut(x, c(5, Inf)))

codebook(df2)

new_var <- "new_x"
dplyr::mutate(df, x = cut(x, c(5, Inf))) |>
	codebook()
dplyr::mutate(df, {{new_var}} := cut(x, c(5, Inf))) |>
	codebook()



df

egen(df, x, c(5, Inf), c("lower", "upper"))
egen(df, x, c(5, Inf), c("lower", "upper"), x2)
