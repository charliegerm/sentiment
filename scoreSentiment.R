score.sentiment = function(sentences)
{
  require(plyr)
  require(stringr)
  
  # .progress = progress  

  # build some sentiment word lists
  pos.words = c('awesome', 'brilliant', 'love', 'fabulous', 'favorite', 'happy', 'great', 'fantastic', 'friendly', 'amazing', 'upgrade', 'better', 'best', 'perfect', 'flawless')
  neg.words = c('bad', 'hate', 'unacceptable', 'poor', 'disappointing', 'cancel', 'irritated', 'aggravated', 'terrible', 'unhappy', 'rude', 'screw', 'worse', 'insufferable', 'worst', 'disgusted', 'awful', 'delay', 'late', 'bumped', 'suck', 'fail', 'miserable', 'horrible', 'unfriendly', 'incompetent')
  
  scores = laply(sentences, function(sentence, pos.words, neg.words)
  {
  # clean up sentences
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
  # convert to lower case
    sentence = tolower(sentence)
  
  # split into words
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
  # compare with positive/negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
  
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
  
  # TRUE/FALSE treated as 1/0 by sum()
    score = sum(pos.matches) - sum(neg.matches)
    return(score)  
  }, pos.words, neg.words)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
  
}
