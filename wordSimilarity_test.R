
df[, c("q1Clean2", "q2Clean2") := list(cleanText(question1, removeExtraWords = tm::stopwords("en"), removeNum = F, lowercase = T, preservePunct = T, encode = F),
                                       cleanText(question2, removeExtraWords = tm::stopwords("en"), removeNum = F, lowercase = T, preservePunct = T, encode = F))]
data <- df
data <- data[, c("q1Clean", "q2Clean") := list(cleanText(question1), cleanText(question2))]

stringdist::stringdistmatrix(data$q1Clean2, data$q2Clean2, method = "lv")
stringdist::stringdistmatrix("asd asdasdsad asdas", "zxczxczzxczxczxc", method = "lv")

stringdist::stringdist(data$q1Clean2, data$q2Clean2)
stringdist::stringdist("hola que tal?", "hola como estas?", method = "qgram")


tm::as.DocumentTermMatrix(data)


RecordLinkage::levenshteinSim("how are you?", "how are you today?")
RecordLinkage::levenshteinDist("how are you?", "how are you today?")
RecordLinkage::compare.dedup(select(data, question1:question2), phonetic = c("question1", "question2"))

sapply(data$question1, RecordLinkage::levenshteinSim, data$question2)

df$lv_sim <- apply(data, 1, function(r) {
  RecordLinkage::levenshteinSim(str1 = r["q1Clean2"], str2 = r["q2Clean2"])
  })
select(df, question1, question2, q1Clean2, q2Clean2, is_duplicate, lv_sim) %>% View

df$is_duplicate <- as.integer(df$is_duplicate)
asd <- glm(is_duplicate ~ lv_sim, data = df)
summary(asd)
