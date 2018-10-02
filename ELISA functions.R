analyze <- function(oneFileName) {
  
  standards <- read_excel(oneFileName, sheet = 2)
  OD450data <- read_excel(oneFileName, sheet = 3)
  
  meanStandards <- standards %>%
    group_by(agConc)%>%
    summarize(meanStandard = mean(OD450))
  
  linearFit <- lm(agConc~meanStandard, data = meanStandards)
  
  y_intercept <- linearFit$coefficients[1]
  slope <- linearFit$coefficients[2]
  
  cytokineConc <- OD450data %>%
    group_by(sampleName, ID, dilutionFactor, cytokine) %>%
    summarize(meanValue = mean(OD450)) %>%
    mutate(`concentration` = max(0, (meanValue*slope + y_intercept)*dilutionFactor))
  
  cytokineConc
}

wardHCA <- function(dataSet) {
  y <- dataSet$concentration
  y <- na.omit(y)
  y <- scale(y)
  row.names(y) <- dataSet$sampleName
  
  
  distance <- dist(y)
  hc2 <- agnes(distance, method = "ward")
  dend2 <- as.dendrogram(hc2)
  
  
  pdf("dendrogram.pdf", width = 15, height = 15)
  dendroPlot <- fviz_dend(dend2, k = 3, cex = 1, color_labels_by_k = TRUE,type = "circular")
  
  print(dendroPlot)
  dev.off()
}
