getGroupings =
function(h, bw = 3, minInRun = 3, minDelta = 0)    
{
    dens = density(h, bw = bw)
#plot(dens, type = "b")    

      # Look at the change in the density. We are looking for the increasing parts of the curve
      # where there are several points in a row that have increasing y so the difference is positive
      #!!!!XXXX    bad variable d       delta = c(0, diff(d$y))   This caused me a long debug session.
    delta = c(0, diff(dens$y))

    runs = rle(delta >= minDelta)
      # Get an group identifier for each observation in dens$x as to which run it is in.
    g = rep(1:length(runs$length), runs$length)

      # group the dens$x into the groups and then only keep this for which
      # the change is > minDelta and also that the run has at least minInRun
    grps = split(dens$x, g)
    w = runs$values & runs$length >= minInRun
    unname(sapply(grps[w], min))

    #XXX Look at second derivative.
    
}

findLineBreaks =
    #
    # This works reasonably well.
    # However, it fails on a few lines on the first page of Amada-2013.pdf, specifically
    # it returns
    #  "(J.   Arikawa). Corresponding E-mail   addresses: author.   j   arika@med.hokudai.ac.jp Tel.: +81 11 706 6905; ,   yosimatu@med.hokudai.ac.jp fax: +81 11 706 6906. oped spp.   for   detection   of   SEOV   IgG   antibody   in   blood   from   Rattus
    # which combines pieces from 3 lines at the bottom of the first column and one line in the second column.
    # This is because the text in the first column at this point is in a smaller font.
    # If we split by column first, then we would solve this.
    # But since we also detect the columns by resolving the lines first, we have a circularity
    # However, we can return to split by lines within the columns so it would be
    #  lines
    #  columns
    #  lines within columns
function(bbox, bw = 3, minInRun = 3, minDelta = 0, asPositions = TRUE)
{
      # compute the density of the bottom location, but weight the values
      # based on the length of the "word", either as the width or nchar(bbox$text)
      #    h = rep(bbox$top + bbox$height, bbox$width)
    h = rep(bottom(bbox), width(bbox)) 
    starts = getGroupings(h, bw, minInRun, minDelta)

    if(asPositions)
        starts
    else 
        split(bbox, cut(bottom(bbox), c(starts, Inf)))
}


getTextLines =
function(bbox, breaks = findLineBreaks(bbox), drop = TRUE)
{
    bbox = as(bbox, "TextBoundingBox")
    ll = split(bbox, cut(bottom(bbox), c(0, breaks, Inf)))
    if(drop)
        ll[ sapply(ll, nrow) > 0 ]
    else
        ll
}
