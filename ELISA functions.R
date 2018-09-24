analyze <- function(oneFileName) {
  
  standards <- read_excel(oneFileName, sheet = 2)
  OD450data <- read_excel(oneFileName, sheet = 3)
  
  meanStandards <- standards %>%
    group_by(agConc)%>%
    summarize(meanStandard = mean(OD450))
  
  linearFit <- lm(agConc~meanStandard, data = meanStandards)
  
  y_intercept <- linearFit$coefficients[1]
  slope <- linearFit$coefficients[2]
  
  IL10Conc <- OD450data %>%
    group_by(sample, ID, dilutionFactor) %>%
    summarize(meanValue = mean(OD450)) %>%
    mutate(`concentration` = max(0, (meanValue*slope + y_intercept)*dilutionFactor))
  
  IL10Conc
}
