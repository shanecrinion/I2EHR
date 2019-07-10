

# set up individuals CVD data
pData(Ellsworth_final)$title <- as.character(pData(Ellsworth_final)$title)
x <- replace(pData(Ellsworth_final)$title, str_detect(Biobase::pData(Ellsworth_final)$title, 
                                                      "baseline"), "baseline")
x <- replace(pData(Ellsworth_final)$title, str_detect(Biobase::pData(Ellsworth_final)$title, 
                                                      "3month"), "month3")
x <- replace(x, str_detect(Biobase::pData(Ellsworth_final)$title, 
                           "baseline"), "baseline")
x <- replace(x, str_detect(Biobase::pData(Ellsworth_final)$title, 
                           "1year"), "year1")

####### correcting mistakes in the published data
x <- replace(x, str_detect(Biobase::pData(Ellsworth_final)$title, 
                           "basleline"), "baseline")


x <- replace(x, str_detect(Biobase::pData(Ellsworth_final)$title, 
                           "3moths"), "month3")

x <- replace(x, str_detect(Biobase::pData(Ellsworth_final)$title, 
                           "1_year"), "year1")

x <- replace(x, str_detect(Biobase::pData(Ellsworth_final)$title, 
                           "1 year"), "year1")

Biobase::pData(Ellsworth_final)$title <- x
Biobase::pData(Ellsworth_final)$title <- as.factor(Biobase::pData(Ellsworth_final)$title)


# control data


# synthetic expression data


# finish the steps of the differential expression data

