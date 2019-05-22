library(ggplot2)

ggplot(as.data.frame(patients.csv$ETHNICITY),
       aes(x=patients.csv$ETHNICITY, color=patients.csv$GENDER)) +
  geom_histogram(fill = "white", alpha = 0.3, position = "identity", stat = "count") +
#  geom_vline(data=as.data.frame(mu), aes(xintercept=mu, colour=patients.csv$GENDER), 
#             linetype="dashed", alpha=0.5) +
  scale_colour_manual(values = c("#0571b0", "#ca0020")) +
  theme_classic() +
  theme(legend.position = "top", 
        axis.text.x = element_text(face = "bold", 
                                   size = 8, 
                                   angle = 90)) 
