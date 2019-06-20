---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
pData(minguez_final)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

```{r}
###### TCFL2 expression

TCFL2_data_CC <- TCFL2_data[TCFL2_data$tissue_T2D == "CC",]
TCFL2_data_nC <- TCFL2_data[TCFL2_data$tissue_T2D == "nC",]

TCFL2_data_CC_median <- median(TCFL2_data_CC$org_value)
TCFL2_data_nC_median <- median(TCFL2_data_nC$org_value)
TCFL2_data_sum <- data.table(org_value = c(TCFL2_data_nC_median,
                                           TCFL2_data_CC_median),
                             tissue_T2D = c("nC", "CC"))
TCFL2_plot <-
  
ggplot(data = TCFL2_data_sum, 
       aes(x = tissue_T2D, 
       y = org_value, 
       color = tissue_T2D,
       group=1)) + 
  geom_line() +
  ggtitle("Median expression for TCFL2")


```


```{r}


tissue_T2D <- tissue[disease == "T2D"]
TCFL2_expr <- Biobase::exprs(minguez_final)["16709333", disease == "T2D"]
TCFL2_data <- as.data.frame(TCFL2_expr)
colnames(TCFL2_data)[1] <- "org_value"
TCFL2_data <- mutate(TCFL2_data, 
                     individual = i_T2D, 
                     tissue_T2D)


TCFL2_data$tissue_T2D <- factor(TCFL2_data$tissue_T2D, 
                                levels = c("CC", "nC"))

TCFL2_EC <-
  
  ggplot(data = TCFL2_data, aes(x = tissue_T2D, y = org_value, 
                             group = tissue_T2D, fill= tissue_T2D)) +
  geom_violin() +
  ggtitle("TCFL2 gene expression dispersal")
```



```{r}
show(pData(phenoData(GSE115313[[1]])))
```



```{r}
##### TCFL2
library(gridExtra)
grid.arrange(TCFL2_plot, TCFL2_EC, nrow = 1)

```


```{r}
THADA_expr <- Biobase::exprs(minguez_final)["16709333", disease == "T2D"]
THADA_data <- as.data.frame(THADA_expr)
colnames(THADA_data)[1] <- "org_value"
THADA_data <- mutate(THADA_data, 
                     individual = i_T2D, 
                     tissue_T2D)


THADA_data$tissue_T2D <- factor(THADA_data$tissue_T2D, 
                                levels = c("CC", "nC"))

THADA_EC <-
  
  ggplot(data = THADA_data, aes(x = tissue_T2D, y = org_value, 
                             group = tissue_T2D, fill= tissue_T2D)) +
  geom_violin() +
  ggtitle("THADA gene expression dispersal")



THADA_data_CC <- THADA_data[THADA_data$tissue_T2D == "CC",]
THADA_data_nC <- THADA_data[THADA_data$tissue_T2D == "nC",]

THADA_data_CC_median <- median(THADA_data_CC$org_value)
THADA_data_nC_median <- median(THADA_data_nC$org_value)
THADA_data_sum <- data.table(org_value = c(THADA_data_nC_median,
                                           THADA_data_CC_median),
                             tissue_T2D = c("nC", "CC"))
THADA_plot <-
  
ggplot(data = THADA_data_sum, 
       aes(x = tissue_T2D, 
       y = org_value, 
       color = tissue_T2D,
       group=1)) + 
  geom_line() +
  ggtitle("Median expression for THADA")

grid.arrange(THADA_plot, THADA_EC, nrow = 1)

```



