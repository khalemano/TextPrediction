
###################################
# Functions for sampling
###################################

# Takes a list of lines and selects
# returns a subset of lines
get_train_logic_vector = function(lines,perc=NULL,num=NULL,seed=123){
  set.seed(seed)
  len = length(lines)
  if (!is.null(num))perc=(num/len)
  sample(c(TRUE,FALSE),size=len,replace=TRUE,prob=c(perc,(1-perc)))
}

###################################
# Functions for generating ngrams 
###################################

#takes a line and outputs a character vector of words
process_input_line=function(line,vocab){
  # Keeps only ASCII characters
  #line = gsub("[^\x20-\x7E]","",line)
  # Adds spaces before and after punctuations
  #line = gsub("([\x21-\x2F\x3A-\x40\x5B-\x60\x7B-\x7E])"," \\1 ",line)
  # Spaces are removed around apostrophes that are part of contractions 
  #line = gsub("([a-zA-Z]) ' ([a-zA-Z])","\\1'\\2",line)
  # Keeps only alphabetical characters and spaces
  line = gsub("[^\x20a-zA-Z]","",line)
  # Splits the line using spaces
  words = unlist(strsplit(line,split=" "))
  # Removes blank words due to double spaces
  words = words[words!=""]
  # Replaces unknown words with <unk>
  words[!tolower(words) %in% vocab] = "<unk>"
  words
}

#takes the keys and increments the resulting ngram by 1
increment_ngram = function(hash,keys){
  hashObject = hash
  i = 1;
  while (i < length(keys)){
    key = keys[i]
    if (has.key(key,hashObject)){
      hashObject = hashObject[[key]]
    } else {
      hashObject[[key]] = hash();
      hashObject = hashObject[[key]]
    }
    i = i + 1
  }
  lastKey = keys[i]
  if (has.key(lastKey,hashObject)){
    hashObject[[lastKey]] = hashObject[[lastKey]] + 1
  } else {
    hashObject[[lastKey]] = 1
  }
}

#takes a chr vector of words and feeds it into an ngram
feed_words_into_ngram = function(hash,words,n){
  for (i in 1:(length(words)-(n-1))){
    increment_ngram(hash,words[i:(i+(n-1))])
  }
}

# takes a chr vector of lines
# returns a hash of ngrams/nextwords
generate_ngrams = function(lines,vocab,unigramReverse,bigramReverse,trigram,progressPercent=10){
  inc = floor(length(lines)/(100/progressPercent))
  lineCounter = 0
  progress = 0
  for (line in lines){
    lineCounter = lineCounter + 1
    if (lineCounter >= inc){
      lineCounter =0
      progress = progress + progressPercent
      print(paste(progress,"percent complete"))
    }
    words = process_input_line(line,vocab)
    feed_words_into_ngram(unigramReverse,rev(words),1)
    words = c("<s>",words,"</s>")
    feed_words_into_ngram(bigramReverse,rev(words),2)
    words = c("<s>",words)
    feed_words_into_ngram(trigram,words,3)
  }
  print(paste("Completed",length(lines),"lines"))
}

######################################
# Functions for processing the ngrams 
######################################

#p-continuation hash
#takes reverse ngram
create_p_continuation = function(ngramReverse){
  totalBigramCount = 0;
  pcont = hash()
  for (key in keys(ngramReverse)){
    pcont[[key]] = length(ngramReverse[[key]])
  }
  pcont
}

#returns discounted count
discount = function(count){
  count - 0.75
}

# takes single depth hash with numeric values and adds lops and total
# also replaces counts with discounted values
add_lops_and_total = function(hash){
  totalCounts = sum(values(hash))
  
  for (key in keys(hash)){
    hash[[key]] = discount(hash[[key]])
  }
  
  totalDiscount = totalCounts - sum(values(hash))
  leftOverProbSpace = totalDiscount/totalCounts

  hash[["<total>"]] = totalCounts
  hash[["<lops>"]] = leftOverProbSpace
}

#applies the add_lops_and_total function to a trigram
add_lops_and_total_to_trigram = function(trigram){
  for (key1 in keys(trigram)){
    for (key2 in keys(trigram[[key1]])){
      add_lops_and_total(trigram[[key1]][[key2]])
    }
  }
}

#collapses trigram into a single depth hash with named vectors as values
collapse_trigram = function(trigram){
  newHash = hash()
  for (key1 in keys(trigram)){
    for (key2 in keys(trigram[[key1]])){
      newHash[[paste(key1,key2)]] = values(trigram[[key1]][[key2]])
    }
  }
  newHash
}

##################################
# Functions for generating output 
##################################

pkn_of_word = function(words,unigramPContinuation,bigramPContinuation,trigram){
  
  word1 = words[1]
  word2 = words[2]
  word3 = words[3]
  
  triP = 0
  triLambda = 1
  triVal = trigram[[paste(word1,word2)]]

  if (!is.null(triVal)){
    if(word3 %in% names(triVal)){
      triP = max(triVal[[word3]],0)/triVal[["<total>"]]      
    }
    triLambda = triVal[["<lops>"]]
  }
  
  biP = max(bigramPContinuation[[word3]],0)/bigramPContinuation[["<total>"]]
  biLambda = bigramPContinuation[["<lops>"]]
  
  uniP = max(unigramPContinuation[[word3]],0)/unigramPContinuation[["<total>"]]
  uniLambda = unigramPContinuation[["<lops>"]]
  
  triP + triLambda*(biP + biLambda*(uniP + uniLambda*(1/10000)))
}

top_candidates = function(words,badwords,unigramPContinuation,bigramPContinuation,trigram){
  candidates = c()
  
  word1 = words[1]
  word2 = words[2]
  
  triVal = trigram[[paste(word1,word2)]]
  
  if (!is.null(triVal)){
    counts = unique(triVal)
    counts = counts[order(counts,decreasing=TRUE)]
    for (count in counts){
      newCandidates = names(triVal)[triVal == count]
      newCandidates = newCandidates[!newCandidates %in% c("<unk>","</s>","<total>","<lops>",badwords)]
      candidates = c(candidates,newCandidates)
      if (length(candidates) >= 5) break;
    }
  }
  
  if(length(candidates) < 5){
    val = values(bigramPContinuation)
    counts = unique(val)
    counts = counts[order(counts,decreasing=TRUE)]
    for (count in counts){
      newCandidates = names(val)[val == count]
      newCandidates = newCandidates[!newCandidates %in% c("<unk>","</s>","<total>","<lops>",badwords)]
      candidates = c(candidates,newCandidates)
      if (length(candidates) >= 5) break;
    }
  }
  
  candidates = unique(candidates)
  
  probs = c()
  for (cand in candidates){
    prob = pkn_of_word(c(words,cand),unigramPContinuation,bigramPContinuation,trigram)
    probs = c(probs,prob)
  }
  
  ord = order(probs,decreasing=TRUE)
  df = data.frame(Candidates = candidates[ord], PercentProbability = 100*probs[ord])
  df$Candidates = levels(df$Candidates)[df$Candidates]
  df
}

text_to_dataframe = function(line,badwords,vocab,unigramPContinuation,bigramPContinuation,trigram){
  words = process_input_line(line,vocab)
  words = c("<s>","<s>",words)
  len = length(words)
  top_candidates(words[(len-1):len],badwords,unigramPContinuation,bigramPContinuation,trigram)
}





