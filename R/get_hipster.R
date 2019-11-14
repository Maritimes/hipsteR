#' @title get_hipster
#' @description Plots an image of a hipster.  What else?
#' @examples
#'# Get a hipster
#'get_hipster()
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_hipster<-function(){
# https://worldwideinterweb.com/funny-hipster-pictures/
# https://www.pexels.com/search/hipster/
search <- read_html("https://www.istockphoto.com/ca/photos/beard?sort=mostpopular&mediatype=photography&phrase=beard")
#Grab all <img> tags, get their "src" attribute, a URL to an image
urls <- search %>% html_nodes("img") %>% html_attr("src")
#do some filtering to remove garbage
urls<-urls[!is.na(urls)]
urls <- urls[grepl("http", urls)]
urls <- urls[!grepl("interweb", urls)]
urls <- urls[!grepl("gravatar", urls)]
urls <- urls[!grepl(".svg", urls)]
urls <- urls[!grepl("flags", urls)]
urls <- urls[nchar(urls)>13]
im <- magick::image_read(urls[sample(1:length(urls),1)])
plot(im)
}
