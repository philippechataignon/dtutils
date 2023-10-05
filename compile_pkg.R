devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::install(dependencies=F)
devtools::test()
