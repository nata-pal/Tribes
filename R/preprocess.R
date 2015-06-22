

# Returns cleaned corpora
cleanDocs <- function(corpora, lang){
  getLibs(c("tm", "SnowballC", "stringi"))
  
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<!--.+?-->", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<style.+?</style>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<script.+?</script>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<head.+?</head>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<.+?>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  
  corpora <- tm_map(corpora, content_transformer(tolower))
  corpora <- tm_map(corpora, removeNumbers)
  corpora <- tm_map(corpora, removePunctuation)
  corpora <- tm_map(corpora, removeWords, stopwords(lang))
  corpora <- tm_map(corpora, stemDocument, language = lang) 
  corpora <- tm_map(corpora, stripWhitespace)
  
}

