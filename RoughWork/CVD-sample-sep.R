

CVD_Samples[[1]]$Individual <-  rep(1:126,each=3)
head(CVD_Samples[[1]]$Individual)

CVD_Control_SampleID <- sampleNames(CVD_Samples[[1]])[193:378]
CVD_Case_SampleID <- sampleNames(CVD_Samples[[1]])[0:192]

CVD_Cases = CVD_Samples[[1]][, CVD_Case_SampleID]
CVD_Controls = CVD_Samples[[1]][, CVD_Control_SampleID]

# confirmation that samples are coordinated
tail(CVD_Case_SampleID)
head(CVD_Control_SampleID)



head(pData(Ellsworth_final[pData(Ellsworth_final)$title != "3month"]))