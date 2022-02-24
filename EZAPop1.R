# abb 1/20/2022: Download county child populations x year x race from
# https://www.ojjdp.gov/ojstatbb/ezapop/ Hx and notes in /fci/projects/NCHS-OJJDPChildPop201002/,
# integrating here. Highlights: Easier the scrape ezapop than processing NCHS files. No PR (state or
# county) counts. All race alone, can't do in combination, can be combined with ethnicity. No PI race, no reason:
# https://www.ojjdp.gov/ojstatbb/special_topics/qa10102.asp "*Race groups exclude persons of
# Hispanic ethnicity. Hispanic youth can be of any race. The American Indian race category also
# includes Alaskan Native youth. The Asian race category also includes Native Hawaiian and Pacific
# Islander youth."
# manual checks start here: https://www.ojjdp.gov/ojstatbb/ezapop/asp/comparison_selection.asp?selState=1

#source( 'EZAPop1.R', echo=T, max.deparse.length=Inf )
outfileroot='ChildPop_CountyXYear'
colvar='v01' # year
(url1=paste0( 'https://www.ojjdp.gov/ojstatbb/ezapop/asp/comparison_display.asp?selLowerYear=1&selUpperYear=1&selLowerAge=1&selUpperAge=18&col_var=', colvar, '&display_type=counts&export_file=yes&printer_friendly=' ))
#str(x <- readLines( url1 )) #chr [1:61]
#str(x <- read.csv( url1, skip=3, header=T, check.names=F )) # states
x=paste0( 'v05', 1:18 ); (url.age=paste(x, x, sep='=', collapse='&')) # 0-17=1-18
x=paste0( 'v05', 14:18 ); (url.teens=paste(x, x, sep='=', collapse='&')) # 0-17=1-18
subsets <- list( WhNH=paste0('v031=v031&v041=v041&',url.age), NonWh=paste0('v032=v032&v033=v033&v034=v034&',url.age), AI=paste0('v033=v033&',url.age), AA=paste0('v032=v032&',url.age), Wh=paste0('v031=v031&',url.age), Hi=paste0('v042=v042&',url.age), Teens=url.teens )

# custom read.csv for ezapop, basic & brittle for now (in shell I use curl --insecure -L -m 10 --retry 5)
read.ezapop <- function( url, colprefix='', colsuffix='' ) {
#url=paste0(url1, '&selState=13'); colprefix='KidPop'; colsuffix='' # 159 rows?
#url=paste0(url1, '&selState=8'); colprefix='KidPop'; colsuffix='' # Note at end
#url=paste0(url1, '&v031=v031&v041=v041&selState=2'); colprefix='KidPop'; colsuffix='.WhNH' # many Notes
#url=paste0(url1, '&selState=11'); colprefix='KidPop'; colsuffix='' # single row, many Notes
#url=u1; colprefix='KidPop'; colsuffix='' # many Notes
#alexandria city virgina should be 51510, ru13=1?
#url=paste0(url1, '&selState=51'); colprefix='KidPop'; colsuffix='' # 145 rows?
  # skip won't cut it, subsets add lines at top: x1 <- read.csv( url, skip=3, header=T, check.names=F )
  x1 <- readLines( url ) # very brittle, should probably put this in a try/catch for helpful error reporting
  # find first & last rows of table (again, needs descriptive error reporting)
  stopifnot( !is.na(i <- grep('^"?counts"?,', x1))[1] )
  stopifnot( !is.na(i[2] <- grep( '^"?(Suggested |.?Note:)', x1 )[1]) )
  stopifnot( i[2] > i[1] )
  i <- i[1]:(i[2]-1)
  x2 <- read.csv( text=x1[i], header=T, check.names=F ) # skips empties
  rownames(x2) <- x2[,1]
  # drop counts, Total and empty columns
  x2 <- x2[grep('^All ', rownames(x2), invert=T, ignore.case=T), grep('^(counts|Total|)$', colnames(x2), invert=T, ignore.case=T), drop=F]
  # force all numeric
  #drops dims? x2 <- apply( x2, 2, simplify=F, FUN=function(x) suppressWarnings(as.numeric(x)) )
  for( j in colnames(x2) ) x2[[j]] <- suppressWarnings(as.numeric(x2[[j]]))
  colnames(x2) <- paste0( colprefix, colnames(x2), colsuffix )
  #head(x2,2); tail(x2,2)
  return(x2)
}
#head(x <- read.ezapop( url=paste0(url1, '&selState=13'), colprefix='KidPop' ), 3)
#stopifnot( nrow(x) == 159 )

# no FIPS codes, just county names in ezapop, so gotta match the state+county to get FIPS (*need* FIPS)
# also require rural-urban codes, most recent from 2013 (RU13), so FIPS need to be vintage 2013
# I corrected Price=>Prince + FIPS changes: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html
ruc <- read.csv( 'ruralurbancodes2013_corrected.csv', as.is=T )
stopifnot( !duplicated(ruc$FIPS) )
stopifnot( !duplicated(ruc[,c('State','County.Name')]) )
ruc$FIPS <- sprintf( '%05d', as.integer(ruc$FIPS) )
table(nchar(ruc$FIPS <- formatC( ruc$FIPS, width=5, flag='0' )), useNA='always')
table( ruc$RUCC2013, useNA='always' )
#   1    2    3    4    5    6    7    8    9 <NA>
# 472  395  369  217   92  597  434  220  425    0
#ruc[i <- grep('Bedford', ruc$County.Name),] # keep both?

(x=table( ruc$State ))
stopifnot( c(state.abb, 'DC') %in% names(x) ) # PR (78 counties) in there too
#https://www.ojjdp.gov/ojstatbb/ezapop/asp/comparison_selection.asp uses a select list with what look like state FIPS codes
#I could parse the state names from the downloads, less brittle, more complex, keep it simple an assume they're using state FIPS codes in their select list.
i=!duplicated(ruc$State); stcodes <- as.integer(substr( ruc$FIPS[i], 1, 2 )); names(stcodes) <- ruc$State[i]; stcodes

d1=NULL
for( st in setdiff(names(stcodes), 'PR') ) {
#for( st in setdiff(names(stcodes), 'PR')[1:3] ) {
#st='VA' #alexandria city virgina should be 51510, ru13=1. x2[grep('Alex',x2$CountyName),]
  # first fetch pops for all kids:
  u1 <- paste0( url1, '&', url.age, '&selState=', stcodes[st] )
  cat( st, ': Reading ', u1, '...\n', sep='' )
  x1 <- read.ezapop( url=u1, colprefix='KidPop' )
  # link to FIPS using bare county names between x1 & ruc within state
  c1 <- sub( ' (count[yi].*|parish.*|census.*|area.*|borough.*|cit[yi].*|municipalit[yi].*)$', '', rownames(x1), ignore.case=T )
  stopifnot( nchar(c1) >= 3 )
  iruc <- which( ruc$State == st ) # subset ruc indexes to state
  stopifnot( length(iruc) >= 1 )
  c2 <- sub( ' (count[yi].*|parish.*|census.*|area.*|borough.*|cit[yi].*|municipalit[yi].*)$', '', ruc$County.Name[iruc], ignore.case=T )
  stopifnot( length(c2) >= 1 & nchar(c2) >= 3 ) # e.g. Lee County
  stopifnot( !is.na(l12 <- match( gsub('[^a-z]','',tolower(c1)), gsub('[^a-z]','',tolower(c2)) )) ) # match county lower alpha only
  # reorganize columns to match d1 accumulator
  x2 <- data.frame( St=st, CountyName=c1, CountyNameFull=rownames(x1), FIPS=ruc$FIPS[iruc][l12], RU13=ruc$RUCC2013[iruc][l12], x1, row.names=NULL )
  # now add pops by subsets:
  cat( st, ': Reading subsets ... ', sep='' )
  for( ss in names(subsets) ) {
#ss=names(subsets)[1]
    cat( ss, ',', sep='' )
    x3 <- read.ezapop( url=paste0(url1, '&', subsets[[ss]], '&selState=', stcodes[st]), colprefix='KidPop', colsuffix=paste0('.',ss) )
    x2 <- cbind( x2, x3 )
  }
  cat('\n')
  rownames(x2) <- NULL
  # accumulate all that into d1 df
  d1 <- rbind( d1, x2 )
}
summary(d1$RU13)

c1=d1 # save both compressed RData and csv
outfile=paste0(outfileroot,'.RData')
cat( 'Saving to', outfile, '\n' )
save( c1, file=outfile, compress='xz' ) # 1mb

outfile=paste0(outfileroot,'.csv')
cat( 'Saving to', outfile, '\n' )
stopifnot( !grepl(',', d1$CountyName) | !grepl(',', d1$CountyNameFull) )
write.csv( c1, outfile, quote=F, row.names=F ) # 2mb

