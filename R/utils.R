
file_path <- "./test"
object <-

serialize_robject <- function(file_path, robject){
  outCon <- file(file_path, "w")
  mychars <- rawToChar(serialize(robject, NULL, ascii=T))
  cat(mychars, file=outCon); close(outCon)
}

unserialize_robject <- function(file_path){
  unserialize(charToRaw(readChar(file_path, file.info(file_path)$size)))
}

