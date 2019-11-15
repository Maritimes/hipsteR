#' @title get_hipster
#' @description Plots an image of a hipster.  What else?
#' @examples
#'# Get a hipster
#'get_hipster()
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_hipster<-function(){
#each query gets 20 results - use the randomizer allow results from any of the first 200 pages of results
randomStart = 20*round(sample(1:200,1)/20)
searchURLs <- c("https://www.google.com/search?q=%22hipster%22&tbm=isch&safe=active&start="
                #, "https://www.bing.com/images/search?q=%22hipster%22&FORM=HDRSC2&safeSearch=Strict&count=20&offset="
                )
this = sample(searchURLs,1)
search <- read_html(paste0(this,randomStart))
#Grab all <img> tags, get their "src" attribute, a URL to an image
urls <- search %>% html_nodes("img") %>% html_attr("src")
#do some filtering to remove garbage
urls<-urls[!is.na(urls)]
urls <- urls[grepl("http", urls)]
urls <- urls[nchar(urls)>13]
im <- magick::image_read(urls[sample(1:length(urls),1)])
plot(im)
}
