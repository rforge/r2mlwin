MacroScript1 <- function(indata,dtafile,oldsyntax=FALSE,resp, levID, expl, rp, D='Normal', nonlinear=c(0,1), categ=NULL,notation=NULL, nonfp=NA, clre, Meth=1, extra=F,reset,rcon,fcon,maxiter,convtol,
                         BUGO=NULL,mem.init="default", optimat=F, weighting=NULL,modelfile=modelfile,initfile=initfile,datafile=datafile,
                         macrofile=macrofile,IGLSfile=IGLSfile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,debugmode=debugmode,startval=startval){
  
  nlev=length(levID)
  
  nrp=length(rp)
  if (nrp>0){
    rp.names=names(rp)
    if (D[1]=='Multinomial'||D[1]=='Multivariate Normal'||D[1]=='Mixed'){
      for (i in 1:nrp){
        temp=rp.names[i]
        rp.names[i]=paste("rp",as.integer(sub("rp","",temp))+1,sep="")
      }
    }
    names(rp)=rp.names
  }
  num.expl.init=function(p,nonfp,categ){
    num_vars=0
    if (is.na(nonfp[1])){
      if (is.null(categ)|| sum(p==categ["var",])==0){
        num_vars=num_vars+1
      }else{
        if (is.na(categ["ref",which(p==categ["var",])])){
          num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])
        }else{
          num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])-1
        }
      }
    }else{
      if (sum(p==nonfp)==0){
        if (is.null(categ)|| sum(p==categ["var",])==0){
          num_vars=num_vars+1
        }else{
          if (is.na(categ["ref",which(p==categ["var",])])){
            num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])
          }else{
            num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])-1
          }
        }
      }
    }
    num_vars
  }
  
  if(is.list(expl)){
    sep.coeff=expl$sep.coeff
    if (is.list(nonfp)){
      nonfp.sep=nonfp$nonfp.sep
      nonfp.common=nonfp$nonfp.common
    }
    if (!is.na(sep.coeff[1])){
      num_vars=sum(sapply(sep.coeff, function(x) num.expl.init(x,nonfp.sep,categ)))
    }else{
      num_vars=0
    }
    
    if (D[1]=='Multinomial'){
      nresp=length(levels(indata[,resp]))-1
      ## it is true if adding each expl variables separately
      num_vars=num_vars *nresp
    }
    if (D[1]=='Multivariate Normal'){
      nresp=length(resp)
      num_vars=num_vars *nresp
    }
    
    common.coeff=expl$common.coeff
    num_vars=num_vars+sum(sapply(common.coeff, function(x) num.expl.init(x,nonfp$nonfp.common,categ)))
    common.coeff.id=expl$common.coeff.id
    
  }else{
    num_vars=sum(sapply(expl, function(x) num.expl.init(x,nonfp,categ)))
    if (D[1]=='Multinomial'){
      nresp=length(levels(indata[,resp]))-1
      num_vars=num_vars *nresp
    }
    if (D[1]=='Multivariate Normal'){
      nresp=length(resp)
      num_vars=num_vars *nresp
    }
  }
  
  if (nrp>0){
    for(ii in 1:nrp){
      rpx=rp[[rp.names[ii]]]
      nrpx=length(rpx)
      if (nrpx==1) num_vars=num_vars+1
      if (nrpx>=2) num_vars=num_vars+ nrpx*(nrpx-1)/2+nrpx
    }
  }
  
  ## Write into macro file
  wrt =function(x) write(x,macrofile, append = T)
  
  cat(file=macrofile)
  wrt("ECHO    0")
  wrt("NOTE    *****************************************************************")
  wrt("NOTE      MLwiN macro created by rtomlwin command")
  wrt("NOTE    *****************************************************************")
  wrt("")
  
  wrt("NOTE    Initialise MLwiN storage")
  if(mem.init[1]=="default"){
    wrt(paste("INIT    ",nlev+1," 6000 2500 ",num_vars+10," 30", sep=""))
  }else{
    wrt(paste("INIT    ",mem.init[1],mem.init[2],mem.init[3],mem.init[4],mem.init[5]))
  }
  
  wrt("NOTE     Limit the maximum matrix size")
  if (optimat){
    wrt("OPTS   1")
  }else{
    wrt("OPTS   0")
  }
  
  wrt("MONI    0")
  wrt("NOTE    Import the R data into MLwiN")
  wrt(paste("RSTA    '",dtafile,"'",sep=""))
  
  if (notation=='class'){
    wrt("INDE 1")
  }
  if (!(D[1]=="Multinomial"||D[1]=="Multivariate Normal"||D[1]=="Mixed")){
    wrt("NOTE   Specify the response variable")
    for (p in resp) wrt(paste("RESP    '",p,"'",sep=""))
    wrt("")
  }
  
  if (D[1] == 'Binomial') {
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:nlev){
      aa=nlev:1
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("")
    
    wrt('RDISt 1 0')
    wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
    if (D[2] == 'logit') {wrt('LFUN 0');DD2=0}
    if (D[2] == 'probit') {wrt('LFUN 1');DD2=1}
    if (D[2] == 'cloglog') {wrt('LFUN 2');DD2=2}
    
    interpos=grep("\\:",expl)
    if (oldsyntax==FALSE || length(interpos)==0){
      for (p in expl){
        if (is.null(categ)){
          wrt(paste("ADDT    '",p,"'",sep=""))
        }else{
          if (sum(p==categ["var",])!=0) {
            if(is.na(categ["ref",which(p==categ["var",])])){
              wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
            }else{
              wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
            }
          }else{
            wrt(paste("ADDT    '",p,"'",sep=""))
          }
        }
      }
    }else{
      exply=expl[interpos]
      explx=expl[-interpos]
      if (length(explx)>0){
        for (p in explx){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }
      for (i in 1:length(exply)){
        TT=""
        interx=unlist(strsplit(exply[i],"\\:"))
        for (j in 1:length(interx)){
          TT=paste(TT,"'",interx[j],"' ",sep="")
        }
        wrt(paste("ADDT    ",TT,sep=""))
      }
      expl <- c(explx, exply)
    }
    wrt("")
  }
  
  if(D[1]=='Mixed'){
    nresp=length(resp)
    for(ii in 1:nresp) wrt(paste("MVAR 1   '", resp[ii],"'",sep=""))
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:nlev){
      aa=nlev:2
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("IDEN 1 'resp_indicator'")
    jj=1
    for(ii in 2:length(D)){
      if(D[[ii]][1]=="Binomial"){
        wrt(paste("RDISt ",jj," 0",sep=""))
        wrt(paste("DOFFs ",jj," '",D[[ii]][3],"'",sep=""))
        if (D[[ii]][2] == 'logit') {wrt('LFUN 0');DD2=0}
        if (D[[ii]][2] == 'probit') {wrt('LFUN 1');DD2=1}
        if (D[[ii]][2] == 'cloglog') {wrt('LFUN 2');DD2=2}
      }
      if(D[[ii]][1]=="Poisson"){
        wrt(paste("RDISt ",jj," 1",sep=""))
        wrt('LFUN 3');DD2=3
        DD2=3
        if (!is.na(D[[ii]][3])) {
          wrt(paste("DOFFs 1 '",D[[ii]][3],"'",sep=""))
        }
      }
      jj=jj+1
    }
    wrt("")
    if(is.list(expl)){
      if (!is.na(sep.coeff[1])){
        interpos1=grep("\\:",sep.coeff)
        if (oldsyntax==FALSE || length(interpos1)==0){
          for (x in 1:length(sep.coeff)){
            p=sep.coeff[x]
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }
        }else{
          exply=sep.coeff[interpos1]
          explx=sep.coeff[-interpos1]
          if (length(explx)>0){
            for (p in explx){
              if (is.null(categ)){
                wrt(paste("ADDT    '",p,"'",sep=""))
              }else{
                if (sum(p==categ["var",])!=0) {
                  if(is.na(categ["ref",which(p==categ["var",])])){
                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                  }else{
                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                  }
                }else{
                  wrt(paste("ADDT    '",p,"'",sep=""))
                }
              }
            }
          }
          for (i in 1:length(exply)){
            TT=""
            interx=unlist(strsplit(exply[i],"\\:"))
            for (j in 1:length(interx)){
              TT=paste(TT,"'",interx[j],"' ",sep="")
            }
            wrt(paste("ADDT    ",TT,sep=""))
          }
          sep.coeff <- c(explx, exply)
        }
      }
      interpos2=grep("\\:",common.coeff)
      if (oldsyntax==FALSE || length(interpos2)==0){
        for (y in 1:length(common.coeff)){
          p=common.coeff[y]
          len.common.id=length(common.coeff.id[y,])
          tt="RPAT    "
          aa=1:len.common.id
          partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
          bb=""
          for (ii in aa) bb=paste(bb,partname[ii],sep="")
          for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
          wrt(tt)
          
          p=common.coeff[y]
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }else{
        for (y in 1:length(common.coeff)){
          p=common.coeff[y]
          len.common.id=length(common.coeff.id[y,])
          tt="RPAT    "
          aa=1:len.common.id
          partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
          bb=""
          for (ii in aa) bb=paste(bb,partname[ii],sep="")
          for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
          wrt(tt)
          p=common.coeff[y]
          if (!(y%in%interpos2)){
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }else{
            TT=""
            interx=unlist(strsplit(p,"\\:"))
            for (j in 1:length(interx)){
              TT=paste(TT,"'",interx[j],"' ",sep="")
            }
            wrt(paste("ADDT    ",TT,sep=""))
          }
          wrt("RPAT")
          common.coeff[y]=paste(p,".",bb,sep="")
        }
      }
    }else{
      interpos=grep("\\:",expl)
      if (oldsyntax==FALSE || length(interpos)==0){
        for (p in expl){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }else{
        exply=expl[interpos]
        explx=expl[-interpos]
        if (length(explx)>0){
          for (p in explx){
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }
        }
        for (i in 1:length(exply)){
          TT=""
          interx=unlist(strsplit(exply[i],"\\:"))
          for (j in 1:length(interx)){
            TT=paste(TT,"'",interx[j],"' ",sep="")
          }
          wrt(paste("ADDT    ",TT,sep=""))
        }
        expl <- c(explx, exply)
      }
    }
    wrt("")
  }
  
  if (D[1]=='Multivariate Normal'){
    nresp=length(resp)
    for (ii in 1:nresp) wrt(paste("MVAR 1   '", resp[ii],"'",sep=""))
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:nlev){
      aa=nlev:2
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("IDEN 1 'resp_indicator'")
    wrt('LFUN 0')
    wrt("")
    if(is.list(expl)){
      if (!is.na(sep.coeff[1])){
        interpos1=grep("\\:",sep.coeff)
        if (oldsyntax==FALSE || length(interpos1)==0){
          for (x in 1:length(sep.coeff)){
            p=sep.coeff[x]
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }
        }else{
          exply=sep.coeff[interpos1]
          explx=sep.coeff[-interpos1]
          if (length(explx)>0){
            for (p in explx){
              if (is.null(categ)){
                wrt(paste("ADDT    '",p,"'",sep=""))
              }else{
                if (sum(p==categ["var",])!=0) {
                  if(is.na(categ["ref",which(p==categ["var",])])){
                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                  }else{
                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                  }
                }else{
                  wrt(paste("ADDT    '",p,"'",sep=""))
                }
              }
            }
          }
          for (i in 1:length(exply)){
            TT=""
            interx=unlist(strsplit(exply[i],"\\:"))
            for (j in 1:length(interx)){
              TT=paste(TT,"'",interx[j],"' ",sep="")
            }
            wrt(paste("ADDT    ",TT,sep=""))
          }
          sep.coeff <- c(explx, exply)
        }
      }
      interpos2=grep("\\:",common.coeff)
      if (oldsyntax==FALSE || length(interpos2)==0){
        for (y in 1:length(common.coeff)){
          p=common.coeff[y]
          len.common.id=length(common.coeff.id[y,])
          tt="RPAT    "
          aa=1:len.common.id
          partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
          bb=""
          for (ii in aa) bb=paste(bb,partname[ii],sep="")
          for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
          wrt(tt)
          
          p=common.coeff[y]
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }else{
        for (y in 1:length(common.coeff)){
          p=common.coeff[y]
          len.common.id=length(common.coeff.id[y,])
          tt="RPAT    "
          aa=1:len.common.id
          partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
          bb=""
          for (ii in aa) bb=paste(bb,partname[ii],sep="")
          for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
          wrt(tt)
          p=common.coeff[y]
          if (!(y%in%interpos2)){
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }else{
            TT=""
            interx=unlist(strsplit(p,"\\:"))
            for (j in 1:length(interx)){
              TT=paste(TT,"'",interx[j],"' ",sep="")
            }
            wrt(paste("ADDT    ",TT,sep=""))
          }
          wrt("RPAT")
          common.coeff[y]=paste(p,".",bb,sep="")
        }
      }
    }else{
      interpos=grep("\\:",expl)
      if (oldsyntax==FALSE || length(interpos)==0){
        for (p in expl){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }else{
        exply=expl[interpos]
        explx=expl[-interpos]
        if (length(explx)>0){
          for (p in explx){
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }
        }
        for (i in 1:length(exply)){
          TT=""
          interx=unlist(strsplit(exply[i],"\\:"))
          for (j in 1:length(interx)){
            TT=paste(TT,"'",interx[j],"' ",sep="")
          }
          wrt(paste("ADDT    ",TT,sep=""))
        }
        expl <- c(explx, exply)
      }
    }
    wrt("")
  }
  
  if (D[1]== 'Multinomial'){
    wrt("LINK 2 G21")
    wrt("NAME G21[1] 'resp' G21[2] 'resp_indicator'")
    wrt("LINK 0 G21")
    wrt(paste('MNOM ',as.numeric(D[4]), " '",resp,"' ",  "'resp' 'resp_indicator' ",as.numeric(D[5]),sep=""))
    wrt("RESP   'resp'")
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:c(nlev-1)){
      aa=nlev:1
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("IDEN 1 'resp_indicator'")
    wrt("")
    
    if (as.numeric(D[4])==0) wrt('RDISt 1 4') else wrt('RDISt 1 5')
    wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
    if (D[2] == 'logit') {wrt('LFUN 0');DD2=0}
    if (D[2] == 'probit') {wrt('LFUN 1');DD2=1}
    if (D[2] == 'cloglog') {wrt('LFUN 2');DD2=2}
    
    wrt("")
    if(is.list(expl)){
      if (!is.na(sep.coeff[1])){
        interpos1=grep("\\:",sep.coeff)
        if (oldsyntax==FALSE || length(interpos1)==0){
          for (x in 1:length(sep.coeff)){
            p=sep.coeff[x]
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }
        }else{
          exply=sep.coeff[interpos1]
          explx=sep.coeff[-interpos1]
          if (length(explx)>0){
            for (p in explx){
              if (is.null(categ)){
                wrt(paste("ADDT    '",p,"'",sep=""))
              }else{
                if (sum(p==categ["var",])!=0) {
                  if(is.na(categ["ref",which(p==categ["var",])])){
                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                  }else{
                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                  }
                }else{
                  wrt(paste("ADDT    '",p,"'",sep=""))
                }
              }
            }
          }
          for (i in 1:length(exply)){
            TT=""
            interx=unlist(strsplit(exply[i],"\\:"))
            for (j in 1:length(interx)){
              TT=paste(TT,"'",interx[j],"' ",sep="")
            }
            wrt(paste("ADDT    ",TT,sep=""))
          }
          sep.coeff <- c(explx, exply)
        }
      }
      interpos2=grep("\\:",common.coeff)
      if (oldsyntax==FALSE || length(interpos2)==0){
        for (y in 1:length(common.coeff)){
          p=common.coeff[y]
          len.common.id=length(common.coeff.id[y,])
          tt="RPAT    "
          aa=1:len.common.id
          partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
          bb=""
          for (ii in aa) bb=paste(bb,partname[ii],sep="")
          for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
          wrt(tt)
          
          p=common.coeff[y]
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }else{
        for (y in 1:length(common.coeff)){
          p=common.coeff[y]
          len.common.id=length(common.coeff.id[y,])
          tt="RPAT    "
          aa=1:len.common.id
          partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
          bb=""
          for (ii in aa) bb=paste(bb,partname[ii],sep="")
          for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
          wrt(tt)
          p=common.coeff[y]
          if (!(y%in%interpos2)){
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }else{
            TT=""
            interx=unlist(strsplit(p,"\\:"))
            for (j in 1:length(interx)){
              TT=paste(TT,"'",interx[j],"' ",sep="")
            }
            wrt(paste("ADDT    ",TT,sep=""))
          }
          wrt("RPAT")
          common.coeff[y]=paste(p,".",bb,sep="")
        }
      }
    }else{
      interpos=grep("\\:",expl)
      if (oldsyntax==FALSE || length(interpos)==0){
        for (p in expl){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }else{
        exply=expl[interpos]
        explx=expl[-interpos]
        if (length(explx)>0){
          for (p in explx){
            if (is.null(categ)){
              wrt(paste("ADDT    '",p,"'",sep=""))
            }else{
              if (sum(p==categ["var",])!=0) {
                if(is.na(categ["ref",which(p==categ["var",])])){
                  wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                }else{
                  wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                }
              }else{
                wrt(paste("ADDT    '",p,"'",sep=""))
              }
            }
          }
        }
        for (i in 1:length(exply)){
          TT=""
          interx=unlist(strsplit(exply[i],"\\:"))
          for (j in 1:length(interx)){
            TT=paste(TT,"'",interx[j],"' ",sep="")
          }
          wrt(paste("ADDT    ",TT,sep=""))
        }
        expl <- c(explx, exply)
      }
    }
    wrt("")
  }
  
  if (D[1] == 'Normal') {
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:nlev){
      aa=nlev:1
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("")
    
    wrt("NOTE   Specify covariate(s) used anywhere in the model")
    interpos=grep("\\:",expl)
    if (oldsyntax==FALSE || length(interpos)==0){
      for (p in expl){
        if (is.null(categ)){
          wrt(paste("ADDT    '",p,"'",sep=""))
        }else{
          if (sum(p==categ["var",])!=0) {
            if(is.na(categ["ref",which(p==categ["var",])])){
              wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
            }else{
              wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
            }
          }else{
            wrt(paste("ADDT    '",p,"'",sep=""))
          }
        }
      }
    }else{
      exply=expl[interpos]
      explx=expl[-interpos]
      if (length(explx)>0){
        for (p in explx){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }
      for (i in 1:length(exply)){
        TT=""
        interx=unlist(strsplit(exply[i],"\\:"))
        for (j in 1:length(interx)){
          TT=paste(TT,"'",interx[j],"' ",sep="")
        }
        wrt(paste("ADDT    ",TT,sep=""))
      }
      expl <- c(explx, exply)
    }
    wrt("")
  }
  if (D[1] == 'Poisson') {
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:nlev){
      aa=nlev:1
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("")
    
    wrt('RDISt 1 1')
    wrt('LFUN 3')
    DD2=3
    if (!is.na(D[3])) {
      wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
    }
    interpos=grep("\\:",expl)
    if (oldsyntax==FALSE || length(interpos)==0){
      for (p in expl){
        if (is.null(categ)){
          wrt(paste("ADDT    '",p,"'",sep=""))
        }else{
          if (sum(p==categ["var",])!=0) {
            if(is.na(categ["ref",which(p==categ["var",])])){
              wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
            }else{
              wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
            }
          }else{
            wrt(paste("ADDT    '",p,"'",sep=""))
          }
        }
      }
    }else{
      exply=expl[interpos]
      explx=expl[-interpos]
      if (length(explx)>0){
        for (p in explx){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }
      for (i in 1:length(exply)){
        TT=""
        interx=unlist(strsplit(exply[i],"\\:"))
        for (j in 1:length(interx)){
          TT=paste(TT,"'",interx[j],"' ",sep="")
        }
        wrt(paste("ADDT    ",TT,sep=""))
      }
      expl <- c(explx, exply)
    }
    wrt("")
  }
  
  
  if (D[1] == 'Negbinom') {
    wrt("NOTE   Specify the level identifier(s)")
    for (ii in 1:nlev){
      aa=nlev:1
      if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
    }
    wrt("")
    
    wrt('RDISt 1 2')
    wrt('LFUN 3')
    DD2=3
    if (as.logical(D[2])) {
      wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
    }
    interpos=grep("\\:",expl)
    if (oldsyntax==FALSE || length(interpos)==0){
      for (p in expl){
        if (is.null(categ)){
          wrt(paste("ADDT    '",p,"'",sep=""))
        }else{
          if (sum(p==categ["var",])!=0) {
            if(is.na(categ["ref",which(p==categ["var",])])){
              wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
            }else{
              wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
            }
          }else{
            wrt(paste("ADDT    '",p,"'",sep=""))
          }
        }
      }
    }else{
      exply=expl[interpos]
      explx=expl[-interpos]
      if (length(explx)>0){
        for (p in explx){
          if (is.null(categ)){
            wrt(paste("ADDT    '",p,"'",sep=""))
          }else{
            if (sum(p==categ["var",])!=0) {
              if(is.na(categ["ref",which(p==categ["var",])])){
                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
              }else{
                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
              }
            }else{
              wrt(paste("ADDT    '",p,"'",sep=""))
            }
          }
        }
      }
      for (i in 1:length(exply)){
        TT=""
        interx=unlist(strsplit(exply[i],"\\:"))
        for (j in 1:length(interx)){
          TT=paste(TT,"'",interx[j],"' ",sep="")
        }
        wrt(paste("ADDT    ",TT,sep=""))
      }
      expl <- c(explx, exply)
    }
    wrt("")
  }
  
  if (is.list(nonfp)){
    wrt("NOTE Turn off the fixed part of the explotary varible(s)")
    nonfp.sep=nonfp$nonfp.sep
    nonfp.common=nonfp$nonfp.common
    if (!is.na(nonfp.sep[1])){
      interpos=grep("\\:",nonfp.sep)
      if (oldsyntax==FALSE || length(interpos)==0){
        for (p in nonfp.sep) wrt(paste("FPAR 0  '",p,"'",sep=""))
      }else{
        for (i in 1:length(nonfp.sep)){
          if (i %in% interpos){
            wrt(paste("FPAR 0  '",gsub("\\:","\\.",nonfp.sep[i]),"'",sep=""))
          }else{
            wrt(paste("FPAR 0  '",nonfp.sep[i],"'",sep=""))
          }
        }
      }
    }
    if (!is.na(nonfp.common[1])){
      interpos=grep("\\:",nonfp.common)
      if (oldsyntax==FALSE || length(interpos)==0){
        for (p in nonfp.common) wrt(paste("FPAR 0  '",p,"'",sep=""))
      }else{
        for (i in 1:length(nonfp.common)){
          if (i %in% interpos){
            wrt(paste("FPAR 0  '",gsub("\\:","\\.",nonfp.common[i]),"'",sep=""))
          }else{
            wrt(paste("FPAR 0  '",nonfp.common[i],"'",sep=""))
          }
        }
      }
    }
  }else{
    if (!is.na(nonfp[1])){
      wrt("NOTE Turn off the fixed part of the explotary varible(s)")
      for (p in nonfp) wrt(paste("FPAR 0  '",gsub("\\:","\\.",p),"'",sep=""))
    }
  }
  
  wrt("")
  wrt("NOTE   Specify random part covariate(s)")
  if (nrp>0){
    for (ii in 1:nrp){
      for (p in rp[[ii]]) wrt(paste("SETV  ",as.numeric(sub("rp","",rp.names[ii])),"   '",gsub("\\:","\\.",p),"'",sep=""))
    }
  }
  if (!is.null(clre)){
    nclre=ncol(clre)
    for (ii in 1:nclre){
      wrt(paste("CLRE  ",as.numeric(clre[1,ii])," '",gsub("\\:","\\.",clre[2,ii]),"' '",gsub("\\:","\\.",clre[3,ii]),"'",sep=""))
    }
  }
  
  nexpl=length(expl)
  wrt("")

  if (!is.null(fcon)) {
    wrt("FCON b1001")
    wrt(paste("JOIN cb1001", paste(as.vector(fcon), collapse=" "), "cb1001"))
  }

  if (!is.null(rcon)) {
    wrt("RCON b1002")
    wrt(paste("JOIN cb1002", paste(as.vector(rcon), collapse=" "), "cb1002"))
  }
  
  wrt("NOTE   Set estimation method")
  if (Meth!=2){
    wrt(paste("METH",Meth))
  }

  for (ii in 1:nlev){
    wrt(paste("RESEt",ii,reset[ii]))
  }

  wrt(paste("LINE ",nonlinear[1],nonlinear[2]))
  if (extra==TRUE) {
    wrt("EXTRa 1")
  }
  wrt("")
  if (!is.null(weighting)){
    if (is.null(weighting$FSDE)) weighting$FSDE=2
    if (is.null(weighting$RSDE)) weighting$RSDE=2
    if(length(weighting$levels)==length(weighting$weights)){
      for (i in 1:length(weighting$weights)){
        if (!is.na(weighting$weights[i])){
          wrt(paste("NOTE   Specify sampling weights at level", weighting$levels[i]))
          wrt(paste("WEIG ", weighting$levels[i]," ", 1, " '",weighting$weights[i],"'",sep=""))
          wrt("")
        }else{
          wrt(paste("NOTE   Specify equal weights at level", weighting$levels[i]))
          wrt(paste("WEIG ", weighting$levels[i]," ", 1,sep=""))
          wrt("")
        }
        if (as.integer(weighting$mode)==2){
          wrt("NOTE   Standardised weighting")
          wrt("LINK 1 G21")
          wrt(paste("WEIG ", weighting$levels[i]," ", 2, " G21[1]", sep=""))
          wrt("FILL G21")
          wrt("LINK 0 G21")
          wrt("WEIG 2")
          wrt("")
        }
        if (as.integer(weighting$mode)==1){
          wrt("NOTE   Raw weighting")
          wrt("LINK 1 G21")
          wrt(paste("WEIG ", weighting$levels[i]," ", 2, " G21[1]", sep=""))
          wrt("FILL G21")
          wrt("LINK 0 G21")
          wrt("WEIG 1")
          wrt("")
        }
      }
      if (as.integer(weighting$mode)>0){
        wrt("NOTE   Create the standardised weights")
        wrt("WEIG")
        wrt("")
        if ( weighting$FSDE==2){
          wrt("NOTE   Turn on sandwich estimators for the fixed part parameter standard errors")
          wrt("FSDE 2")
          wrt("")
        }else{
          wrt("FSDE 0")
          wrt("")
        }
        if ( weighting$RSDE==2){
          wrt("NOTE   Turn on sandwich estimators for the random part parameter standard errors")
          wrt("RSDE 2")
          wrt("")
        }else{
          wrt("RSDE 0")
          wrt("")
        }
      }else{
        wrt("NOTE   Create the equal weights")
        wrt("WEIG")
        wrt("WEIG   0")
        wrt("")
      }
    }else{
      stop("The length of levels does not match with the length of weights.")
    }
  }
  if (D[1]=="Normal"){
    wrt("PREF   0")
    wrt("POST   0")
  }
  
  wrt("NOTE   Fit the model")
  wrt("ECHO 1")
  wrt("BATC 1")
  wrt("MAXI 2")
  wrt("STAR")

  wrt("NAME   c1098 '_FP_b'")
  wrt("NAME   c1099 '_FP_v'")
  wrt("NAME   c1096 '_RP_b'")
  wrt("NAME   c1097 '_RP_v'")

  if (!is.null(startval)){
    if (!is.null(startval$FP.b)){
      wrt(paste("JOIN ",paste(startval$FP.b, collapse=" "), " '_FP_b'",sep=""))
    }
    if (!is.null(startval$FP.v)){
      wrt(paste("JOIN ",paste(startval$FP.v[!upper.tri(startval$FP.v)], collapse=" "), " '_FP_v'",sep=""))
    }
    if (!is.null(startval$RP.b)){
      wrt(paste("JOIN ",paste(startval$RP.b, collapse=" "), " '_RP_b'",sep=""))
    }
    if (!is.null(startval$RP.v)){
      wrt(paste("JOIN ",paste(startval$RP.v[!upper.tri(startval$RP.v)], collapse=" "), " '_RP_v'",sep=""))
    }
  }

  if (debugmode){
    wrt("WSET 15 1")
    wrt("EXPA 3")
    wrt("ESTM 2")
    wrt("PAUS")
  }

  wrt(paste("TOLE", convtol))
  wrt(paste("MAXI", maxiter))
  wrt("NEXT")
  wrt("ECHO 0")
  wrt("MONI 1")
  wrt("ITNU 0 b21")
  wrt("CONV b22")
  wrt("")
  
  
  
  if (debugmode){
    wrt("NOTE   Open the equations window")
    wrt("WSET 15 1")
    wrt("EXPA 3")
    wrt("ESTM 2")
    wrt("PAUS")
  }
  wrt("NOTE    *****************************************************************")
  wrt("")
  
  
  wrt("NOTE    *****************************************************************")
  wrt("NOTE       Export the model results to R")
  wrt("NOTE    *****************************************************************")
  wrt("LINK 1 G21")
  wrt("NAME   G21[1] '_Stats'")
  if (D[1]=="Multivariate Normal" || D[1]=="Multinomial") {
    wrt("NOBS 2 b31 b32")
  } else {
    wrt("NOBS 1 b31 b32")
  }
  wrt("EDIT 1 '_Stats' b31")
  wrt("EDIT 2 '_Stats' b32")
  if (D[1]=="Normal" || D[1]=="Multivariate Normal"){
    wrt("LIKE   b100")
  }
  wrt("EDIT 3 '_Stats' b100")
  wrt("EDIT 7 '_Stats' b21")
  wrt("EDIT 8 '_Stats' b22")
  wrt("NAME   c1098 '_FP_b'")
  wrt("NAME   c1099 '_FP_v'")
  wrt("NAME   c1096 '_RP_b'")
  wrt("NAME   c1097 '_RP_v'")
  wrt("NAME   c1094 '_esample'")
  wrt("SUM '_esample' b1")
  wrt("EDIT 9 '_Stats' b1")
  wrt(paste("PSTA '",IGLSfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ", "'_Stats'",sep=""))
  
  calcresiduals = function(level, displevel, rpx, resioptions, clre=clre){
    wrt("")
    if (!("norecode"%in%resioptions)){
      wrt("MISR 0")
    }
    
    len.rpx = length(rpx)
    if (level == 2 & D[[1]][1]=="Mixed"){
      for (i in 2:length(D)){
        if (D[[i]][1]=="Binomial" | D[[i]][1]=="Poisson"){
          len.rpx = len.rpx + 1
        }
      }
    }

    wrt(paste("LINK", len.rpx, "G21"))
    for (k in 1:len.rpx){
      wrt(paste0("NAME G21[", k, "] ", paste0("'lev_",displevel,"_resi_est_",rpx[k],"'")))
      wrt(paste0("DESC G21[", k, "] ", "'residual estimates'"))
    }
    
    wrt(paste("LINK", len.rpx, "G22"))
    if ("variance" %in% resioptions){
      for (k in 1:len.rpx){
        wrt(paste0("NAME G22[",k,"] ", paste0("'lev_",displevel,"_resi_variance_",rpx[k],"'")))
        wrt(paste0("DESC G22[",k,"] ", "'residual variance'"))
      }
    }else{
      for (k in 1:len.rpx){
        wrt(paste0("NAME G22[",k,"] ", paste0("'lev_",displevel,"_resi_se_",rpx[k],"'")))
        wrt(paste0("DESC G22[",k,"] ", "'residual standard error'"))
      }
    }
    wrt("RFUN")
    wrt("ROUT G21 G22")
    wrt("")
    
    wrt(paste("RLEV   ",level,sep=""))
    wrt("RCOV   1")

    outgroups <- c("G21","G22")

    if ("standardised"%in%resioptions||"deletion"%in%resioptions||"leverage"%in%resioptions){
      wrt(paste("LINK", len.rpx, "G23"))
      for (k in 1:len.rpx){
        wrt(paste0("NAME G23[",k,"] ", paste0("'lev_",displevel,"_std_resi_est_",rpx[k],"'")))
        wrt(paste0("DESC G23[",k,"] ", "'std standardised residual'"))
      }
      
      wrt("RTYP   0")
      wrt("RESI")
      if (!("variance" %in% resioptions)){
        for (k in 1:len.rpx){
          wrt(paste0("CALC G23[",k,"]=G21[",k,"]/sqrt(G22[",k,"])",sep=""))
        }
      }
      outgroups <- c(outgroups, "G23")
    }
    
    #NOTE leverage depends on residuals with rtype 0
    if ("leverage"%in%resioptions||"influence"%in%resioptions){
      #influence requires leverage to be calculated
      wrt(paste("LINK", len.rpx, "G24"))
      for (k in 1:len.rpx){
        wrt(paste0("NAME G24[",k,"] ", paste0("'lev_",displevel,"_resi_leverage_",rpx[k],"'")))
        wrt(paste0("DESC G24[",k,"] ", "'leverage residual'"))
      }
      outgroups <- c(outgroups, "G24")
      
      for (k in 1:len.rpx){
        wrt("LINK 1 G25")
        wrt(paste0("OMEGa ",level, " '",rpx[k], "' G25[1]"))# retrieve variance for corresponding random parameter
        wrt("PICK 1 G25[1] b50")
        wrt("ERASe G25[1]")
        wrt("LINK 0 G25")
        wrt(paste0("CALC G24[",k,"]=1-sqrt(G22[",k,"])/sqrt(b50)"))
      }
      
      if (!("standardised"%in%resioptions)&&!("deletion"%in%resioptions)){
        wrt("ERAS G23")
        wrt("LINK 0 G23")
        outgroups <- outgroups[outgroups!="G23"]
      }
    }
    
    wrt("RTYP   1")# Compute comparative variances
    wrt("RESI")

    if (!("variance"%in%resioptions)){
      for (k in 1:len.rpx){
        wrt(paste0("CALC G22[",k,"]=sqrt(G22[",k,"])"))# Convert the variances to standard errors
      }
    }
    
    if ("deletion"%in%resioptions||"influence"%in%resioptions){
      wrt(paste("LINK", len.rpx, "G25"))
      for (k in 1:len.rpx){
        wrt(paste0("NAME G25[",k,"] ", paste0("'lev_",displevel,"_resi_deletion_",rpx[k],"'")))
        wrt(paste0("DESC G25[",k,"] ", "'deletion residual'"))
      }
      outgroups <- c(outgroups, "G25")
      
      wrt(paste("NOBS ",level,"b31 b32"))
      for (k in 1:len.rpx){
        wrt(paste0("CALC G25[",k,"]=G23[",k,"]/ sqrt((b31 - 1 - G23[",k,"]^2)/(b31 - 2))"))
      }
      
      if (!("standardised"%in%resioptions)&&!("leverage"%in%resioptions)){
        wrt("ERASe G23")
        wrt("LINK 0 G23")
        outgroups <- outgroups[outgroups!="G23"]
      }
    }
    
    if ("influence"%in%resioptions){
      wrt(paste("LINK", len.rpx, "G26"))
      for (k in 1:len.rpx){
        wrt(paste0("NAME G26[",k,"] ", paste0("'lev_",displevel,"_resi_influence_",rpx[k],"'")))
        wrt(paste0("DESC G26[",k,"] ", "'influence residual'"))
      }
      outgroups <- c(outgroups, "G26")

      for (k in 1:len.rpx){
        wrt(paste0("SUM    G24[",k,"]", " b50"))
        wrt(paste0("CALC   G26[",k,"]=G24[",k,"]/b50"))
        wrt(paste0("CALC   G26[",k,"]=sqrt(G26[",k,"]/(1-G26[",k,"]))*abso(G25[",k,"])"))
      }      
      
      if (!("deletion"%in%resioptions)){
        wrt("ERASE G25")
        wrt("LINK 0 G25")
        outgroups <- outgroups[outgroups!="G25"]
      }
      if(!("leverage"%in%resioptions)){
        wrt("ERASE G26")
        wrt("LINK 0 G26")
        outgroups <- outgroups[outgroups!="G26"]
      }
    }
    
    if ("sampling"%in%resioptions){
      numele <- (len.rpx*(len.rpx+1)) / 2
      wrt(paste0("LINK ", numele," G26"))
      k <- 1
      for (i in 1:len.rpx){
        for (j in 1:i){
          if (i==j){
            wrt(paste0("NAME G26[",k,"] ", paste0("'lev_",displevel,"_resi_var_",rpx[i],"'")))
            wrt(paste0("DESC G26[",k,"] ", "'sampling variance'"))
          }else{
            wrt(paste0("NAME G26[",k,"] ", paste0("'lev_",displevel,"_resi_cov_",rpx[i],"_",rpx[j],"'")))
            wrt(paste0("DESC G26[",k,"] ", "'sampling covariance'"))
          }
          k <- k + 1
        }
      }
      outgroups <- c(outgroups, "G26")

      

      wrt(paste0("LINK ", len.rpx," G27"))
      wrt(paste0("LINK ", 1," G28"))
      wrt(paste0("LINK ", 1," G29"))

      wrt(paste0("NAME G29[1] ", paste0("'lev_",displevel,"_resi_cov'")))
      wrt(paste0("DESC G29[1] ", paste0("'sampling var(cov)'")))
      outgroups <- c(outgroups, "G29")

      
      wrt("RFUN")
      wrt("ROUT G27 G29")
      wrt("RCOV 2")
      wrt("RESI")
      wrt("")
      
      #NOTE: This is square rooted, as the residual covariances are sometimes negative
      wrt(paste0("NOBS ",level, " b31 b32"))
      wrt(paste0("CODE ",numele, " 1 b31 G28[1]"))
      wrt("SPLIt G29 G28 G26")
      wrt("ERAS G28 G29")
      wrt("LINK 0 G28")
      wrt("LINK 0 G29")
      wrt("ERAS G27")
      wrt("LINK 0 G27")
      outgroups <- outgroups[outgroups!="G29"]
    }
    
    wrt("")
    wrt(paste0("NOBS ", level, " b30 b31"))
    wrt("LINK 1 G29")
    wrt("GENE 1 b30 1 G29[1]")
    wrt(paste0("NAME G29[1] 'lev_",level,"_residualid'"))
    outgroups <- c(outgroups, "G29")
    if (!("norecode"%in%resioptions)){
      wrt("MISR 1")
    }
    wrt("")

    for (i in 1:length(outgroups)) {
      wrt(paste("GSET 2 G30", outgroups[i], "G30"))
      wrt(paste("LINK 0", outgroups[i]))
    }
  }
  
  
  if (resi.store&& nrp>0){
    wrt("LINK 0 G30")
    for (j in nrp:1){
      rpx=rp[[j]]
      len.rpx=length(rp[[j]])
      wrt(paste("NOTE Calculate level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
      levtt=as.numeric(sub("rp","",rp.names[j]))
      displevel = levtt
      if (D[1] == "Multivariate Normal" || D[1] == "Mixed" || D[1] == "Multinomial") {
        displevel <- displevel - 1
      }
      calcresiduals(levtt, displevel, rpx, resioptions, clre=clre)
      wrt(paste("PSTA '", resifile, "' G30",sep=""))
    }
    wrt("ERAS G30")
    wrt("LINK 0 G30")

  }
  
  if ((!is.null(BUGO))&&!(D[1]=="Mixed")&&nrp>0){
    if(D[1]=="Normal") DD=1
    if(D[1]=="Binomial") DD=2
    if(D[1]=="Poisson") DD=3
    if(D[1]=='Multivariate Normal') DD=4
    if(D[1]=="Multinomial") {if (as.numeric(D[4])==0) DD=6 else DD=7}
    
    for (j in nrp:1){
      rpx=rp[[j]]
      len.rpx=length(rp[[j]])
      wrt(paste("NOTE Calculate MCMC starting values for level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
      wrt(paste("RLEV   ",as.numeric(sub("rp","",rp.names[j])),sep=""))
      wrt("RFUN")
      wrt("RCOV 2")
      wrt("LINK 2 G21")
      wrt(paste("LINK", len.rpx, "G22"))
      wrt(paste("LINK", len.rpx, "G23"))
      wrt("ROUT G22 G23")
      wrt("MISR 0")
      wrt("RESI")
      wrt("MISR 1")
      wrt("JOIN G21[1] G22 G21[1]")
      wrt("JOIN G21[2] G23 G21[2]")
      wrt("ERAS G22 G23")
      wrt("LINK 0 G22")
      wrt("LINK 0 G23")
    }
    
    version=as.numeric(BUGO["version"])
    if(D[1]=='Normal'||D[1]=='Multivariate Normal') DD2=0
    wrt(paste("BUGO 6 ",DD," ",DD2, " G21[1] ","'",modelfile,"' ","'",initfile,"' ","'",datafile,"'",sep=""))
    wrt("ERAS G21")
    wrt("LINK 0 G21")
  }
  
  if (!debugmode) wrt("EXIT")
}
