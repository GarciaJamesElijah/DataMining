scores <- c(75,82,60,95,40)
if(all(scores > 60)){
  print("All students passed")
}else if(any(scores < 60)){
  print("at least one student failed")
}