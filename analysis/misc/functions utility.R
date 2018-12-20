##############################
# HANDY DATA/STYLE FUNCTIONS #
##############################
# like plyr::mapvalues, but creates NA value when mapping is not found. Thanks to Julius' answer her: http://stackoverflow.com/questions/18323410/map-values-and-replace
mapvalues2 = function(x, from, to) setNames(to, from)[as.character(x)]
ize = function (df, columns, izer) {  # e.g. factorize like this: df = ize(df, c('col1', 'col2'), as.factor) 
  df[columns] = lapply(df[columns], izer)
  df
}
mapvalues2 = function(x, from, to) setNames(to, from)[as.character(x)]  # like plyr::mapvalues, but creates NA value when mapping is not found. Thanks to Julius' answer her: http://stackoverflow.com/questions/18323410/map-values-and-replace

# instead of having to write df$col = as.numeric(as.character(df$col))
make_numeric = function(x) as.numeric(levels(x))[x]
make_numeric = function(x) as.numeric(as.character(x))

# To avoid doing this for all plots on dots correctness
style_my_plot = function(obj) {
  obj + 
    theme_bw(13) + 
    theme(panel.grid.minor=element_blank()) + 
    scale_colour_manual(values=c('#777777', '#AA3333', '#FF0000', 'black')) + 
    scale_fill_manual(values=c('#777777', '#AA3333', '#FF0000', 'black'))
}
