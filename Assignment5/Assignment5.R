vect<-c(20,55,70)

if(all(vect>50)){
  print("All are above 50")
}else if(any(vect>50)){
  print("Some are above 50")
}else{
  print("None are Above 50")
}