
data <- df
data <- data[, c("q1Clean", "q2Clean") := list(cleanText(question1), cleanText(question2))]

stringdist::stringdistmatrix(data$question1, data$question2, method = "lv")
stringdist::stringdistmatrix("asd asdasdsad asdas", "zxczxczzxczxczxc", method = "lv")

stringdist::stringdist(data$question1, data$question2)
stringdist::stringdist("hola que tal?", "hola como estas?", method = "qgram")


tm::as.DocumentTermMatrix(data)


RecordLinkage::levenshteinSim("how are you?", "how are you today?")
RecordLinkage::levenshteinDist("how are you?", "how are you today?")
RecordLinkage::compare.dedup(select(data, question1:question2), phonetic = c("question1", "question2"))

sapply(data$question1, RecordLinkage::levenshteinSim, data$question2)

df$lv_sim <- apply(data, 1, function(r) {
  RecordLinkage::levenshteinSim(str1 = r["q1Clean"], str2 = r["q2Clean"])
  })
