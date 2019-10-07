



#' @export
f1 <- function(pcx='SW',n0=5,fnam="C:\\Users\\Giles\\AppData\\Local\\aabb\\aappd\\lr\\pp-complete.csv") {
  j <- c('unique_id', 'price_paid', 'deed_date', 'postcode', 'property_type',
         'new_build', 'estate_type', 'saon', 'paon', 'street', 'locality',
         'town', 'district')
  xx <- fread(fnam,stringsAsFactors = F,sep = ',')[grep( paste0('^',pcx,'[1-9]'),V4)]
  pxlrppd <<- setnames(narep(xx)[,seq_along(j),with=F],j)  %>%
    .[, deed_date := substr(deed_date, 1, 10)]%>%
    .[, price_paid := as.numeric(price_paid)]%>%
    .[, pc := abbrev(postcode, nosp = F)]%>%
    .[, saon := abbrev(saon, nosp =F)]%>%
    .[, paon := abbrev(paon, nosp = F)]%>%
    .[, street := abbrev(street, nosp =F)]%>%
    .[, locality := abbrev(locality, nosp = F)]%>%
    .[postcode != 'UNKNOWN']%>%
    .[property_type!='O']%>%
    .[substr(pc,2,2)!=0]%>% #exclude typos Y0,S0 etc
    .[, unique_id := NULL] %>%
    .[,idlr:=1:.N]
}

#pcrd - postcode lookup
#' @export
f2 <- function() {
  x1 <- pxlrppd[,.(pc=sort(unique(pc)))]
  pxlrpcrcd <<- x1[, rc := unlist(lapply(pc, regpcode))]
}

#join, clean, create id
#' @export
f3 <- function() {
  pxlrppd[, pc := gsub(gsub(x = pc, patt = ' +$', rep = ''),
                       patt = '^ +$',
                       rep = '')]
  x2 <- pxlrpcrcd[pxlrppd, on = c(pc = 'pc')]%>%
    .[, idtmp := paste(rc, saon, paon, street, sep = '-')]%>%
    .[irregpcode(rc) == pc]
  x3 <- x2[, .(
    pc,
    rc,
    a1 = paon,
    a2 = saon,
    a3 = street,
    idtmp,
    deed_date,
    price_paid,
    rc12 = rc,
    rc9 = substr(rc, 1, 9),
    rc6 = substr(rc, 1, 6),
    rc3 = substr(rc, 1, 3)
  )]%>%
    .[grepl('^FLAT$|^STUDIO$|^STUDIO FLAT$|^STUDIO APARTMENT$', a1), a1 :='FLATUNIT']%>% #placeholder for un-numbered flats
    .[, a0 := paste(a1, a2, a3)]
  x4 <- unique(reg1(x3))[, id := paste0(rc, a0)]
  pxlrrppd <<- unique(x4[, .(id, idtmp, rc12, rc9, rc6, rc3)])%>%
    .[x2, on = c(idtmp = 'idtmp')]%>%
    .[, idtmp :=NULL]%>%
    .[, ntr := .N, id]
}


#' @export
f4 <- function(rmin=-1,rmax = 3) {
  x1 <- pxlrrppd[, ntr := .N, id]
  pxlrsegrawd <<-
    x1[1 < ntr]%>%
    .[, year := substr(deed_date, 1, 4)]%>%
    .[, list(
      price_paid,
      year,
      deed_date,
      rc,
      rc9,
      rc6,
      rc3,
      property_type,
      estate_type,
      new_build,
      id,
      ntr
    )]%>%
    .[, startdate := c("", deed_date[-.N]) , by = id]%>%
    .[, startprice := c(NA, price_paid[-.N]) , by = id]%>%
    .[, startnew := c(NA, new_build[-.N]) , by = id]%>%
    .[startdate != '']%>%
    .[deed_date > startdate, r := log(price_paid / startprice)]%>%
    .[, pa := 365.25 * r / as.numeric(as.Date(deed_date) - as.Date(startdate))]%>%
    .[!is.na(r)]%>%
    .[(-1 < pa)&
        (pa < 1) &
        (rmin < r) &
        (r < rmax)]%>%
    .[, .(
      id,
      idtr=paste0(id,deed_date),
      rc,
      rc9,
      rc6,
      rc3,
      property_type,
      estate_type,
      startnew,
      ntr,
      startdate=as.Date(startdate),
      startprice,
      deed_date=as.Date(deed_date),
      price_paid,
      year,
      r,
      pa
    )]%>%
    .[!duplicated(idtr)]
}

#' @export
f5 <- function() {
  pxmodad <<- ceiling_date(seq.Date(from=as.Date('1995-01-05'),to=pxlrsegrawd[,max(deed_date)],by='month'),'month')-days(1)
}

#' @export
f6 <- function() {
  edgeprior <- c(8,2,2,1,1)
  n <- length(pxmodad)
  x1 <- structure(rbind(-sdlslope(n)[1,],sdlcurv(n),sdlslope(n)[n-1,]),dimnames=list(as.character(pxmodad),as.character(pxmodad)))
  n1 <- nrow(x1)+1
  x2 <- x1[-c(1:length(edgeprior),n1-(1:length(edgeprior))),]
  for(i in rev(seq_along(edgeprior))) {
    x2 <- rbind(
      (1+edgeprior[i])*x1[i,],
      -edgeprior[i]   *x1[i,],
      x2,
      (1+edgeprior[i])*x1[n1-i,],
      -edgeprior[i]   *x1[n1-i,]
    )
  }
  pxmotpd <<- x2[0<(apply(abs(x2),1,sum)),]
}

#pxmors1 <- function(x=pxmosld[[1]],pxmodad,pxmotpd,period='month',pstrendt=data.table(area=names(pxmosld),pstren=100),quantilex=.9) {
#' @export
f7 <- function(period='month',pstren=100,quantilex=.9) {
  x <-
    setkey(data.table(
      accrue(
        segd = pxlrsegrawd,
        period = period,
        pdate = newperiods(d1 = pxlrsegrawd[, min(startdate)], d2 = pxlrsegrawd[, max(deed_date)], period = period)
      ),
      keep.rownames = T
    ), rn)
  yy <- setkey(pxlrsegrawd[, list(id = paste0(id, '.', deed_date), r)], id)
  stopifnot(yy[,!any(duplicated(id))]&x[,!any(duplicated(rn))])
  yx <- yy[x][,c(names(yy),names(x)[-1]),with=F]
  colnames(yx)[ncol(yx)] <- as.character(max(pxmodad)) #set final column name to monthend
  x2 <- setcolorder(data.table(pxmotpd*pstren)[,id:=as.character(1:nrow(pxmotpd))][,r:=0.],names(yx))
  jyx <- setdiff(names(yx),'id')
  jna <- setNames(sort(setdiff(jyx,'r')),paste0('a',seq_along(jyx[-1])))
  jra <- c('r',paste0('a',seq_along(jyx[-1])))
  x4 <- as.formula(paste('r~',paste(names(jna),collapse='+'),'-1'))
  setnames(x2,old=jna,new=names(jna))
  setnames(yx,old=jna,new=names(jna))
  x3 <- rbind(x2,yx)[,jra,with=F]
  x5 <- biglm(data=x3,formula=x4)
  x6 <- data.table(r=x3[,r],(x3[,r]-as.matrix(x3[,-1])%*%summary(x5)$mat[,'Coef',drop=F]))%>%
    .[,.(r,.resid=Coef)]%>%
    .[-(1:nrow(pxmotpd))]%>%
    .[,signed.qtile:=rank(abs(.resid))/.N,sign(.resid)]
  iok <- x6[,which(signed.qtile<quantilex)]
  idok <- yx[iok,id]
  x7 <- rbind(x2,yx[iok])[,jra,with=F]
  x8 <- biglm(data=x7,formula=x4)
  x9 <- data.table(summary(x8)$mat)%>%
    .[,date:=jyx[-1]]%>%
    .[,area:=pxlrsegrawd[1,rc]]
  rsq1 <- summary(x5)$rsq
  rsq2 <- summary(x8)$rsq
  x10 <- cbind(yx[,.(id)][,area:=pxlrsegrawd[1,substr(rc,1,3)]],x6[,.(r,.resid,signed.qtile,pass=signed.qtile<quantilex)])
  pxmorsd <<- list(
    augment=x10,
    tidy=x9,
    glance=data.table(rsq1,rsq2,nsam1=nrow(yx),nsam2=length(iok),qtile=quantilex)
  )
}

#display
#' @export
f8 <- function() {
  x1 <- pxmorsd$tidy[,.(date=as.Date(date),ret=Coef*365.25/12)][,idx:=cumsum(ret)]
  ggplot(x1,aes(date,idx))+geom_line()+ylab('cumulative log return')+xlab('')+ggtitle(pxmorsd$augment[1,regpcode(substr(area,1,3))])
}


#-----------------------------------------------library

#' @export
accrue <- function(pdate = newperiods(...),
                   segd = gett('segd'),
                   ...) {
  x1 <-
    structure(outer(pdate, segd[, deed_date], `-`), class = 'numeric')
  x1[] <- pmax(0, x1[])
  x2 <-
    structure(outer(pdate, segd[, startdate], `-`), class = 'numeric')
  x2[] <- pmax(0, x2[])
  structure(cbind(t(x2[1, , drop = F]), t(diff(x2 - x1)))  , dimnames =
              list(segd[, iddate(id, deed_date)], as.character(pdate[])))
}

#' @export
sdlcurv <- function (nn)
{
  x1 <- matrix(0, nn - 2, nn)
  ij <- cbind(1:(nn - 2), 1:(nn - 2))
  x1[ij] <- 1
  ij[, 2] <- ij[, 2] + 1
  x1[ij] <- -2
  ij[, 2] <- ij[, 2] + 1
  x1[ij] <- 1
  x1
}

#' @export
sdlslope <- function (nn)
{
  x1 <- matrix(0, nn - 1, nn)
  ij <- cbind(1:(nn - 1), 1:(nn - 1))
  x1[ij] <- 1
  ij[, 2] <- ij[, 2] + 1
  x1[ij] <- -1
  x1
}

#' @export
reg1 <- function(x1 = ext1d) {
  x <- copy(x1)
  j <- 'a0' #names(x)[grep('^a[0-9]',names(x))]
  patt <- c(
    ' +'        =   ' ',
    #repeated space
    ' - '       =   '-',
    #flat ranges with ' - '
    '^ '        =   '',
    #leading space
    ' $'        =   '',
    #trailing space
    ' '         =   '-',
    #space
    '^FLATS?-'  =   '',
    #remove
    '^APARTMENTS?-' = '',
    #remove
    '^STUDIOS?-' =   '',
    #remove
    '^MAISONETTES?-' = ''  #remove
  )
  for (i in seq_along(j)) {
    cmd <-
      gsub(patt = 'XXX', rep = j[i], x = "XXX:=gsub(patt='[^\\\\w\\\\s\\\\-]',rep='',XXX,perl=T)")
    x[, eval(parse(text = cmd))] #eliminate non-alpha, non-whitespace
    for (i1 in seq_along(patt)) {
      cmd <-
        paste0(j[i],
               ":=gsub(patt='",
               names(patt)[i1],
               "',rep='",
               patt[i1],
               "',x=",
               j[i],
               ",perl=T)")
      x[, eval(parse(text = cmd))]
    }

  }
  x
}

#' @export
regpcode <- function(rawcode = 'W1U 4RL',
                     x = parsepcode(rawcode)) {
  rawcode <- gsub(patt = '  ', rep = ' ', rawcode)
  Reduce(paste0, lapply(x, pad1))
}

#' @export
pad1 <- function(x) {
  n1 <- nchar(x)
  x[n1 == 1] <- paste0(x[n1 == 1], paste(collapse = '', rep('-', 2)))
  x[n1 == 2] <- paste0(x[n1 == 2], '-')
  x
}

#' @export
parsepcode <- function(pc) {
  x <-
    suppressWarnings(lapply(data.frame(Reduce(
      rbind, lapply(lapply(lapply(pc, ppc), data.table), t)
    )), unlist))
  x <- lapply(x, `names<-`, NULL)
  names(x) <- names(ppc(pc[1]))
  x
}

#' @export
ppc <- function(pc = 'EC2R 8AH') {
  if (nchar(pc) < 2)
    return(list(
      area = ifelse(grepl('[A-Z,a-z]', pc), paste0(toupper(pc), '--'), ''),
      district = '',
      sector = '',
      unit = ''
    ))
  chkpcode(pc)
  pc <- toupper(pc)
  gg <- gregexpr(patt = ' ', pc)
  x <- strsplit(pc, split = ' ')
  out <- unlist(lapply(x, '[[', 1))
  nout <- nchar(out)
  inum <- as.numeric(regexpr("[0-9]", out))
  area <- pc
  sector <- unit <- district <- rep('', length(pc))
  area[inum == 2] <- substr(out[inum == 2], 1, 1)
  area[inum == 3] <- substr(out[inum == 3], 1, 2)
  district[inum == 2] <- substring(out[inum == 2], 2)
  district[inum == 3] <- substring(out[inum == 3], 3)
  if (any(lapply(x, length) > 1)) {
    #inbound code exists
    stopifnot(all(lapply(x, length) == 2)) #exists for all
    inb <- unlist(lapply(x, '[[', 2))
    nin <- nchar(inb)
    sector <- substr(inb, 1, 1)
    unit <- substring(inb, 2, nin)
  }
  list(
    area = area,
    district = district,
    sector = sector,
    unit = unit
  )
}


#' @export
irregpcode <- function(x) {
  x1 <- substr(x, 1, pmin(6, nchar(x)))
  x2 <- substr(x, pmin(7, nchar(x)), nchar(x))
  gsub(patt = ' $',
       rep = '',
       x = paste(
         gsub(patt = '\\-', rep = '', x = x1),
         gsub(patt = '\\-', rep = '', x = x2)
       ))
}

#' @export
narep <- function(x, nasub = '') {
  jn <- colnames(x)
  for (j in seq_along(jn)) {
    i <- which(is.na(x[, j, with = FALSE]))
    x[i, (j) := nasub]
  }
  x[]
}

#' @export
abbrev <- function (x,
                    len = 30,
                    rep = "",
                    patt = list("\\.",
                                "/", "&", "\\*", ":", ","),
                    nospace = TRUE)
{
  if (nospace)
    patt <- union(patt, " ")
  x <- abbreviate(x, minl = len)
  x <- gsub(x = x,
            patt = grepstring(patt, caret = F),
            rep = rep)
  x
}

#' @export
grepstring <- function(x = regpcode(metro()),
                       dollar = F,
                       caret = T) {
  if (caret)
    x <- paste0('^', x)
  if (dollar)
    x <- paste0(x, '$')
  paste(x, collapse = '|')
}

#' @export
chkpcode <- function(pc = 'EC2R 8AH') {
  #composed of correct chars
  #grepl(patt='[^a-zA-Z0-9]/',x=pc,perl=TRUE)
  #right length
  nch <- sapply(pc, nchar)
  stopifnot(all(nch <= 8))
  #max one space
  stopifnot(unlist(lapply(gregexpr(patt = ' ', pc), length)) == 1)
  #is either 1-part or 2-part
  x <- strsplit(pc, split = ' ')
  #1-part always starts with alpha cap
  if (length(x[[1]]) == 1) {
    stopifnot(all(unlist(gregexpr(pc, patt = '^[A-Z,a-z]')) == 1))
  }
  #2-part always starts with number
  if (length(x[[1]]) == 2) {
    pcin <- lapply(x, '[[', 2)
    if (!all(unlist(gregexpr(pcin, patt = '^[0-9]')) == 1))
      browser()
    stopifnot(all(unlist(gregexpr(pcin, patt = '^[0-9]')) == 1))
  }
  TRUE
}

#' @export
newperiods <- function(d1, d2, ...) {
  x1 <-
    ceiling_date(seq.Date(from = pxlrsegrawd[, min(startdate)], to = pxlrsegrawd[, max(deed_date)], by =
                            'month'),
                 'month') - days(1)
  x2 <-
    structure(x1, names = as.character(seq_along(x1)))#[-length(x1)]
  x2
}

#' @export
iddate <- function(id, deed_date) {
  paste0(id, '.', deed_date)
}
