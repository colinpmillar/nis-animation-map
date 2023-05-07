# NIS animated map

to run this code, download, open R and set the working directory to the root folder and run

```r
install.packages("icesTAF")
install.deps()

# get data
taf.boot()

# process
source.all()

sourceTAF("shiny")
shiny::runApp("shiny")
```
