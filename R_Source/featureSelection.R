#packedge dplyr
#sum - count of 1-class, count - count of elements in group
featuresSelection<-function(data = NULL)
{
  names <- names(data)
  #class in 1-st column
  for (i in 2:ncol(data))
  {
    tmpData <- data.frame(data[1],data[i])
    colnames(tmpData)<-sprintf("x%d", 1:2)
   
     convertionFeatures <- tmpData %>%
       group_by(x2) %>%
       summarise(sum = sum(x1),count = length(x1))
  }
}
