



#' @export
f1 <- function(pcx='NW',n0=5,fnam="C:\\Users\\Giles\\AppData\\Local\\aabb\\aappd\\lr\\pp-complete.csv") {
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

#populates y, x; adds dummy 'shrinkage' rows, trims residuals, applies k-fold cross-validation across pstren*10^pinc
#' @export
f7 <- function(quantilex=.9,pstren=100,pinc=c(-10,0,2)) {
  x <-
    setkey(data.table(
      accrue(
        segd = pxlrsegrawd,
        period = period,
        pdate = newperiods(d1 = pxlrsegrawd[, min(startdate)], d2 = pxlrsegrawd[, max(deed_date)])
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
  x3 <- as.formula(paste('r~',paste(names(jna),collapse='+'),'-1'))
  setnames(yx,old=jna,new=names(jna))
  setnames(x2,old=jna,new=names(jna))
  
  x4 <- rbind(x2,yx)[,jra,with=F]
  
  x5 <- biglm(data=x4,formula=x3)
  x6 <- data.table(r=x4[,r],(x4[,r]-as.matrix(x4[,-1])%*%summary(x5)$mat[,'Coef',drop=F]))%>%
    .[,.(r,.resid=Coef)]%>%
    .[-(1:nrow(pxmotpd))]%>%
    .[,signed.qtile:=rank(abs(.resid))/.N,sign(.resid)]
  iok <- x6[,which(signed.qtile<quantilex)]
  
  x8 <- biglm(data=rbind(x2,yx[iok])[,jra,with=F],formula=x3) #solve trimmed data
  x8lo <- biglm(data=rbind(x2[,jra,with=F]*10^min(pinc),yx[iok,jra,with=F]),formula=x3) #solve low shrink
  x8hi <- biglm(data=rbind(x2[,jra,with=F]*10^max(pinc),yx[iok,jra,with=F]),formula=x3) #solve high shrink
  
  x9 <- data.table(pstren=pinc) %>% #test prior strength by x-val, returns sse(pstren*10^pinc)
    .[,fxv(
      yx=yx[iok,jra,with=F],
      xshrink=x2[,jra,with=F],
      frml=x3,
      kxv=5,
      pstren=as.numeric(unlist(.BY)))
      ,by=pstren]
  x10 <- data.table(summary(x8)$mat)%>%
    .[,date:=jyx[-1]]%>%
    .[,area:=pxlrsegrawd[1,rc]]%>%
    .[,hishrink:=summary(x8hi)$mat[,'Coef']]%>% #cheeky add of extreme solutions
    .[,loshrink:=summary(x8lo)$mat[,'Coef']]
  rsq1 <- summary(x5)$rsq
  rsq2 <- summary(x8)$rsq
  x11 <- cbind(yx[,.(id)][,area:=pxlrsegrawd[1,substr(rc,1,3)]],x6[,.(r,.resid,signed.qtile,pass=signed.qtile<quantilex)])
  
  pxmorsd <<- list(
    augment=x11,
    tidy=x10,
    glance=data.table(rsq1,rsq2,nsam1=nrow(yx),nsam2=length(iok),qtile=quantilex),
    xval=x9
  )
}

#display - ggplot
#' @export
f8 <- function() {
  x1 <- pxmorsd$tidy[,.(date=as.Date(date),tuned=Coef*365.25/12,low.shrink=loshrink*365.25/12,high.shrink=hishrink*365.25/12)]
  x2 <- melt(x1,id.vars='date')[,y:=cumsum(value),variable]
  g1 <- ggplot(x2,aes(date,y,color=variable))+
    geom_line()+
    ylab('cumulative log return')+
    xlab('')+
    ggtitle(paste0('Area postcode: ',pxmorsd$augment[1,irregpcode(substr(area,1,3))])) +
    theme(legend.position=c(.99,.12),legend.justification='right',legend.title = element_blank())+
    labs(caption='Land Registry | Anest')+
    theme(panel.background = element_rect(fill = "#EEF8FF"))
  # ggplotly(g1)
  g1
}

#display - ggplotly
#' @export
f9 <- function() {
  x1 <- pxmorsd$tidy[,.(date=as.Date(date),tuned=Coef*365.25/12,low.shrink=loshrink*365.25/12,high.shrink=hishrink*365.25/12)]
  x2 <- melt(x1,id.vars='date')[,y:=cumsum(value),variable]
  x3 <- data.table(a=c('low.shrink','tuned','high.shrink'),n=1:3)[x2,on=c(a='variable')][,xx:=reorder(a,n)][,.(shrink=xx,date,y)]
  g1 <- ggplot(x3,aes(date,y,frame=shrink))+
    geom_line()+
    ylab('cumulative log return')+
    xlab('')+
    ggtitle(paste0('Area postcode: ',pxmorsd$augment[1,irregpcode(substr(area,1,3))])) +
    theme(legend.position=c(.99,.12),legend.justification='right',legend.title = element_blank())+
    labs(caption='Land Registry | Anest')+
    theme(panel.background = element_rect(fill = "#EEF8FF"))
  ggplotly(g1)
}

#' @export
f10 <- function() {
  x1 <- melt(copy(pxmorsd$xval)[,pstren:=0:2],id.var='pstren')
  g1 <- ggplot(x1,aes(pstren,value,color=variable)) +
    geom_line()+
    geom_point()+
    ylab('sum square error')+
    xlab('shrinkage prior strength -->') +
    theme(legend.position=c(.99,.12),legend.justification='right',legend.title = element_blank())+
    labs(caption='Land Registry | Anest')+
    theme(panel.background = element_rect(fill = "#EEF8FF"))
  ggplotly(g1)
}

#-----------------------------------------------library

#test prior strength with k-fold cross-validation
#' @export
fxv <- function(yx,xshrink,frml,kxv=5,pstren=0) {
  x7 <- rbind(10^pstren*xshrink,yx)
  i1 <- 1:nrow(yx)
  insse <- outsse <- 0*NA
  for(k in 1:kxv) {
    iin <- which((i1%%kxv+1)!=k)+nrow(xshrink) #excluding k
    iout <- which((i1%%kxv+1)==k)+nrow(xshrink) #kth
    i3 <- c(1:nrow(xshrink),iin) #for estimation
    x7r <- biglm(data=x7[i3],formula=frml)
    outsse[k] <- sum((ho=x7[iout,r]-(as.matrix(x7[iout,-1])%*%summary(x7r)$mat[,'Coef',drop=F]))**2)
    insse[k] <- sum((ho=x7[iin,r]-(as.matrix(x7[iin,-1])%*%summary(x7r)$mat[,'Coef',drop=F]))**2)
  }
  data.table(insample=sum(insse)/(kxv-1),outsample=sum(outsse))
}

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
