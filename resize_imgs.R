# Resize images
# Uses: {base, magick}

imgs <- base::list.files(path = "www/hex/", pattern = "png$", full.names = TRUE)
for(i in imgs){
  img <- magick::image_read(i)
  img <- magick::image_scale(img, "139x120")
  magick::image_write(image = img, path = i)
}

img <- magick::image_read("www/barebones.png")
img <- magick::image_scale(img, "139x120")
magick::image_write(image = img, path = "www/barebones.png")