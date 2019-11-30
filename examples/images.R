library("jpeg")

folder = "../../faces"


flatten <- function(img) {
  return(c(img))
}

images = c()
for (i in list.files(folder)) {
  img = flatten(readJPEG(paste(c(folder, i), collapse="/")))
  images = rbind(images, img)
}
images