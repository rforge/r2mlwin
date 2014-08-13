MacroScript2 <- function(indata,dtafile,resp, levID, expl, rp, D,nonlinear, categ,notation,nonfp,clre,Meth,merr,carcentre,maxiter,convtol,
                         seed,iterations,burnin,scale,thinning,priorParam,refresh,fixM,residM,Lev1VarM, 
                         OtherVarM,adaption,priorcode,rate, tol,lclo,mcmcOptions,fact,xclass=NULL,BUGO=NULL,mem.init,optimat=F,
                         nopause,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,IGLSfile=IGLSfile,MCMCfile=MCMCfile,
                         chainfile=chainfile,MIfile=MIfile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,resichains=resichains,
                         FACTchainfile=FACTchainfile,resi.store.levs=resi.store.levs,debugmode=debugmode, startval=startval, dami=dami){
  
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
  
  if(nrp>0){
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
    wrt(paste("INIT    ",nlev+1," 6000 2500 ",num_vars+10," 30",sep=""))
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
    if (length(interpos)==0){
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
        if (!is.na(D[[jj]][3])) {
          wrt(paste("DOFFs 1 '",D[[jj]][3],"'",sep=""))
        }
      }
      jj=jj+1
    }
    wrt("")
    if(is.list(expl)){
      if (!is.na(sep.coeff[1])){
        interpos1=grep("\\:",sep.coeff)
        if (length(interpos1)==0){
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
      if (length(interpos2)==0){
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
      if (length(interpos)==0){
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
        if (length(interpos1)==0){
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
      if (length(interpos2)==0){
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
      if (length(interpos)==0){
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
        if (length(interpos1)==0){
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
      if (length(interpos2)==0){
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
      if (length(interpos)==0){
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
    if (length(interpos)==0){
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
    if (length(interpos)==0){
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
    if (length(interpos)==0){
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
    wrt("NOTE Turn off the fixed part of the explanatary variable(s)")
    nonfp.sep=nonfp$nonfp.sep
    nonfp.common=nonfp$nonfp.common
    if (!is.na(nonfp.sep[1])){
      interpos=grep("\\:",nonfp.sep)
      if (length(interpos)==0){
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
      if (length(interpos)==0){
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
  
  wrt("NOTE   Set estimation method")
  if (Meth!=2){
    wrt(paste("METH", Meth))
  }
  wrt(paste("LINE ",nonlinear[1],nonlinear[2]))
  wrt("")
  
  if (!is.null(fact)){
    TT=NULL
    for (i in 1:fact$nfact){
      TT=c(TT, fact$lev.fact[i]+1, matrix(rbind(fact$loading[i,],fact$constr[i,]),nrow=1))
    }
    if (fact$nfactcor>0) TT=c(TT,fact$factcor)
    FACT=as.vector(c(length(resp),fact$nfact, fact$nfactcor,TT))
    rm(TT)
    TT=""
    for (i in 1:length(FACT)){
      TT=paste(TT,FACT[i])
    }
    wrt(paste("FACT ",TT))
    wrt("LINK 2 G21")
    wrt("SMFA 1 G21[1]")
    wrt("SMFA 2 G21[2]")
    
  }
  if (D[1]=="Normal"){
    wrt("PREF   0")
    wrt("POST   0")
  }
  
  wrt("NOTE   Fit the model")
  wrt("ECHO 1")
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
      wrt(paste("JOIN ",paste(startval$RP.v[!upper.tri(startval$RP.v)], collapse=" "), " '_FP_v'",sep=""))
    }
    if (!is.null(startval$RP.b)){
      wrt(paste("JOIN ",paste(startval$RP.b, collapse=" "), " '_RP_b'",sep=""))
    }
    if (!is.null(startval$RP.v)){
      wrt(paste("JOIN ",paste(startval$RP.v[!upper.tri(startval$RP.v)], collapse=" "), " '_RP_v'",sep=""))
    }
  } else {
    wrt(paste("TOLE", convtol))
    wrt(paste("MAXI", maxiter))
    wrt("BATC 1")
    wrt("NEXT")
  }
  wrt("ECHO 0")
  wrt("MONI 1")
  wrt("ITNU 0 b21")
  wrt("CONV b22")
  wrt("")
  
  wrt("NOTE    *****************************************************************")
  wrt("")
  
  wrt("NOTE    *****************************************************************")
  wrt("NOTE       Export the model results to R")
  wrt("NOTE    *****************************************************************")

  wrt("LINK 1 G30")
  wrt("NAME   G30[1] '_Stats'")
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
  wrt("NAME   c1094 '_esample'")
  wrt("SUM '_esample' b1")
  wrt("EDIT 9 '_Stats' b1")
  wrt(paste("PSTA '",IGLSfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ", "'_Stats'",sep=""))
  wrt("LINK 0 G30")

  wrt("NOTE    *****************************************************************")
  wrt("NOTE Set estimation method to MCMC")
  wrt("NOTE    *****************************************************************")
  wrt("EMODe  3")
   
  if (!is.null(resi.store.levs)){
    wrt(paste0("LINK ", length(resi.store.levs), " G22"))
    for (i in 1:length(resi.store.levs)){
      wrt(paste("SMRE ", resi.store.levs[i], " G22[",i,"]",sep=""))
      wrt(paste("NAME ", " G22[",i,"] 'resi_lev",resi.store.levs[i],"'",sep=""))
    }
    
  }
  if (!is.null(merr)){
    nmerr=as.numeric(merr[1])
    tt=paste("MERR  ",nmerr)
    j=1
    for (ii in 1:nmerr){
      tt=paste(tt," '",merr[j+1],"' ",as.numeric(merr[j+2]),sep="")
      j=j+2
    }
    wrt(tt)
  }
  wrt(paste("ORTH",mcmcOptions$orth))
  if (mcmcOptions$hcen==0){
    wrt(paste("HCEN",mcmcOptions$hcen))
  }else{
    wrt(paste("HCEN",1, mcmcOptions$hcen))
  }
  wrt(paste("SMCM",mcmcOptions$smcm))
  wrt(paste("SMVN",mcmcOptions$smvn))
  if(is.matrix(mcmcOptions$paex)){
    apply(mcmcOptions$paex,1,function(x) wrt(paste("PAEX",x[1],x[2])))
  }else{
    wrt(paste("PAEX",mcmcOptions$paex[1],mcmcOptions$paex[2]))
  }
  
  if (D[1]=="Multivariate Normal") wrt(paste("MCCO ",mcmcOptions$mcco))
  if (!is.null(carcentre) && carcentre == TRUE) {
    wrt("CARC 1")
  }
  if (!is.null(xclass)){
    for(ii in 1:length(as.numeric(xclass$class))){
      if (as.numeric(xclass$N1[ii])==1){
        wrt(paste("MULM",as.numeric(xclass$class[ii])," 1"))
        wrt(paste("CARP",as.numeric(xclass$class[ii])," 0"))
      }
      if (xclass$N1[ii]>1){
        if (length(xclass)==4){
          carflag=F
        }else{
          carflag=xclass$car[ii]
        }
        if (is.na(xclass$id[ii])){
          wrt(paste("MULM ",as.numeric(xclass$class[ii])," ", as.numeric(xclass$N1[ii])," '",xclass$weight[ii],"'",sep=""))
          if (carflag){
            wrt(paste("CARP",as.numeric(xclass$class[ii])," 1"))
          }else{
            wrt(paste("CARP",as.numeric(xclass$class[ii])," 0"))
          }
        }else{
          wrt(paste("MULM ",as.numeric(xclass$class[ii])," ", as.numeric(xclass$N1[ii])," '",xclass$weight[ii],"'"," '",xclass$id[ii],"'",sep=""))
          if (carflag){
            wrt(paste("CARP",as.numeric(xclass$class[ii])," 1"))
          }else{
            wrt(paste("CARP",as.numeric(xclass$class[ii])," 0"))
          }
        }
      }
    }
    wrt("XCLA 1")
  }
  
  wrt("")
  
  wrt("NOTE Set MCMC seed")
  wrt(paste("MCRS ",seed,sep=""))
  wrt("")
  
  wrt("NOTE Set prior distribution parameters")
  if (priorParam[1]!="default"){
    wrt("PRIOR  c1092")
    lenpp=length(priorParam)
    tempt=" "
    for (i in 1:lenpp)  tempt=paste(tempt,priorParam[i])
    wrt(paste("JOIN",tempt," c1092",sep=""))
    wrt("")
  }else{
    wrt("")
  }
  
  if (D[1]=="Normal"){
    wrt("PREF   0")
    wrt("POST   0")
  }
  
  if (is.null(xclass)){
    wrt("MISR   0")
    wrt("LINK 2 G30")
    if (D[1]=="Multinomial"&& as.numeric(D[4])==0){
      len.rpx=2
      wrt(paste0("LINK ", len.rpx, " G27"))
      wrt("LINK 1 G28")
      wrt("NOTE Calculate MCMC starting values for level 2 residuals")
      wrt("RLEV 2")
      wrt("RFUN")
      wrt("RCOV 2")
      wrt("ROUT G27 G28")
      wrt("RESI")
      wrt("JOIN G30[1] G27 G30[1]")
      wrt("JOIN G30[2] G28 G30[2]")
      wrt("ERAS G27")
      wrt("ERAS G28")
      wrt("LINK 0 G27")
      wrt("LINK 0 G28")
    }
    if (nrp>0){
      for (j in nrp:1){
        if (as.numeric(sub("rp","",rp.names[j]))!=1){
          rpx=rp[[j]]
          len.rpx = length(rpx)
          wrt(paste0("LINK ", len.rpx, " G27"))
          wrt("LINK 1 G28")
          wrt(paste("NOTE Calculate MCMC starting values for level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
          wrt(paste("RLEV   ",as.numeric(sub("rp","",rp.names[j])),sep=""))
          wrt("RFUN")
          wrt("RCOV   2")
          wrt("ROUT G27 G28")
          wrt("RESI")
          wrt("JOIN G30[1] G27 G30[1]")
          wrt("JOIN G30[2] G28 G30[2]")
          wrt("ERAS G27")
          wrt("ERAS G28")
          wrt("LINK 0 G27")
          wrt("LINK 0 G28")
        }
      }
    }
    wrt("MISR   1")
  }
  
  if(D[1]=="Normal") DD=1
  if(D[1]=="Binomial") DD=2
  if(D[1]=="Mixed") DD=5
  if(D[1]=="Poisson") DD=3
  if(D[1]=="Multivariate Normal") DD=4
  if(D[1]=="Multinomial") {if (as.numeric(D[4])==0) DD=6 else DD=7; wrt("CLRV 2")}

  wrt(paste("LCLO   ",lclo,sep=""))
  
  if (debugmode){
    wrt("NOTE   Open the equations window")
    wrt("WSET 15 1")
    wrt("EXPA 3")
    wrt("ESTM 2")
    wrt("PAUS")
  }

  priorcol <- ""
  if (priorParam[1]!="default") {
    priorcol <- "c1092"
  }

  if ((!is.null(BUGO))&&!(D[1]=="Mixed")&&nrp>0){
    version=as.numeric(BUGO["version"])
    if(D[1]=='Normal'||D[1]=='Multivariate Normal') DD2=0
    if (is.null(xclass)){
      wrt(paste("BUGO 6 ",DD," ",DD2, " G30[1] ", priorcol," '",modelfile,"' ","'",initfile,"' ","'",datafile,"'",sep=""))
      wrt("ERAS   G30")
      wrt("LINK 0 G30")
    } else {
      wrt(paste("BUGO 6 ",DD," ",DD2, " ","'",modelfile,"' ","'",initfile,"' ","'",datafile,"'",sep=""))
    }
  }else{
    wrt("NOTE   fit the model in MCMC")
    wrt(paste("MTOT   ",iterations,sep=""))
    wrt("ECHO 1")

    residcols <- ""
    if (is.null(xclass)){
      residcols <- "G30[1] G30[2]"
    }
    wrt(paste("MCMC   0", burnin, adaption, scale, rate, tol, residcols, priorcol, fixM, residM, Lev1VarM, OtherVarM, priorcode, DD))
 
    if (is.null(xclass)){
      wrt("ERAS G30")
      wrt("LINK 0 G30")
    }
    wrt("ERAS  c1090 c1091")
    wrt("")
    if (!is.null(dami)&&dami[1]==0&&length(dami)>1){
      ndami=length(dami)
      mvnames=rep(NA,ndami-1)
      wrt(paste("LINK",ndami,"G23"))
      for (i in 2:ndami){
        wrt(paste("MCMC 1 ",dami[i]-dami[i-1]," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
        wrt("PUPN c1003 c1004")
        wrt("AVER c1091 b99 b100")
        wrt(paste0("DAMI 0 G23[",i-1,"]"))
        mvnames[i-1]=paste0("'_est_",dami[i],"'")
        wrt(paste0("NAME G23[",i-1, "] ", mvnames[i-1]))
        wrt("PAUS 1")
      }
      wrt("LINK 0 G23")
      if (dami[ndami]<iterations){
        wrt(paste("MCMC 1 ",(iterations/thinning)-dami[ndami]," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
        wrt("PUPN c1003 c1004")
        wrt("AVER c1091 b99 b100")
        wrt("PAUS 1")
      }
      wrt("LINK 1 G24")
      wrt("NAME G24[1] '_MissingInd'")
      wrt("CALC   '_MissingInd'=abso('_esample'-1)")
      wrt(paste("PSTA '",MIfile, "' ",paste(mvnames,collapse=" ")," '_MissingInd'",sep=""))
      wrt("LINK 0 G24")
      wrt(paste("ERAS  ",paste(mvnames,collapse=" "),sep=""))
    }else{
      if (debugmode&&(!nopause)){
        for (i in 1:floor((iterations/thinning)/refresh)){
          wrt(paste("MCMC 1 ",refresh," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
          wrt("PUPN c1003 c1004")
          wrt("AVER c1091 b99 b100")
          wrt("PAUS 1")
        }
        is.wholenumber <-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
        if(!is.wholenumber(iterations/refresh)){
          wrt(paste("MCMC 1 ",(iterations/thinning)%%refresh," ", thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
          wrt("PUPN c1003 c1004")
          wrt("AVER c1091 b99 b100")
          wrt("PAUS 1")
        }
      }else{
        wrt(paste("MCMC 1 ",iterations/thinning," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
        wrt("PUPN c1003 c1004")
        wrt("AVER c1091 b99 b100")
      }
    }
    wrt("ECHO 0")

    if (debugmode){
      wrt("WSET 15 1")
      wrt("EXPA 3")
      wrt("ESTM 2")
      wrt("PAUS")
    }
    
    wrt("NOTE    *****************************************************************")
    wrt("NOTE       Export the model results to R")
    wrt("NOTE    *****************************************************************")
    if (D[1]=="Multivariate Normal" || D[1]=="Multinomial") {
      wrt("NOBS 2 b31 b32")
    } else {
      wrt("NOBS 1 b31 b32")
    }
    wrt("EDIT 1 '_Stats' b31")
    wrt("EDIT 2 '_Stats' b32")
    if (!(D[1]=="Mixed")&&is.null(merr)&&is.null(fact)){
      wrt("BDIC b1 b2 b3 b4")
      wrt("EDIT 3 '_Stats' b1")
      wrt("EDIT 4 '_Stats' b2")
      wrt("EDIT 5 '_Stats' b3")
      wrt("EDIT 6 '_Stats' b4")
    }
    wrt("EDIT 7 '_Stats' b21")
    wrt("EDIT 8 '_Stats' b22")
    wrt("NAME   c1098 '_FP_b'")
    wrt("NAME   c1099 '_FP_v'")
    wrt("NAME   c1096 '_RP_b'")
    wrt("NAME   c1097 '_RP_v'")
    wrt("NAME   c1094 '_esample'")
    wrt("SUM '_esample' b1")
    wrt("EDIT 9 '_Stats' b1")
    if(is.null(fact)){
      wrt(paste("PSTA '",MCMCfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ","'_Stats'",sep=""))
    }else{
      wrt("LINK 6 G25")
      wrt("DAFA G25[1] G25[2]")
      wrt("DAFL G25[3] G25[4]")
      wrt("DAFV G25[5] G25[6]")
      wrt("NAME G21[1] '_FACT_load_b_chain'")
      wrt("NAME G21[2] '_FACT_load_v_chain'")
      wrt("NAME G25[1] '_FACT_value_b'")
      wrt("NAME G25[2] '_FACT_value_v'")
      wrt("NAME   G25[3]  '_FACT_load_b'")
      wrt("NAME   G25[4]  '_FACT_load_v'")
      wrt("NAME   G25[5]  '_FACT_var_b'")
      wrt("NAME   G25[6]  '_FACT_var_v'")
      wrt(paste("PSTA '",FACTchainfile,"' ","'_FACT_load_b_chain' ","'_FACT_load_v_chain' ","'_FACT_value_b' ","'_FACT_value_v' ",sep=""))
      wrt(paste("PSTA '",MCMCfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ","'_FACT_load_b' ","'_FACT_load_v' ","'_FACT_var_b' ", "'_FACT_var_v' ","'_Stats'",sep=""))
      wrt("ERAS G21")
      wrt("LINK 0 G21")
      wrt("ERAS G25")
      wrt("LINK 0 G25")
    }
    wrt("ERAS '_Stats'")
    wrt("")
    
    if (!is.null(dami)&&length(dami)==1){
      wrt("LINK 3 G26")
      wrt("NOTE generate example if there a missing values")
      wrt("SWIT b1")
      wrt("CASE 0:")
      wrt("LEAVE")
      wrt("CASE:")
      wrt("NAME G26[1] '_MissingInd'")
      wrt("CALC   '_MissingInd'=abso('_esample'-1)")
      if (dami==0){
        wrt("DAMI 0 G26[2]")
        wrt("NAME G26[2] '_est'")
        wrt(paste("PSTA '",MIfile, "' '_est' '_esample' ",sep=""))
        wrt("ERAS  '_est'")
      }
      if (dami==1){
        wrt("DAMI 1 G26[2]")
        wrt("NAME G26[2] '_est'")
        wrt(paste("PSTA '",MIfile, "' '_est' '_esample' ",sep=""))
        wrt("ERAS  '_est'")
      }
      if (dami==2){
        wrt("DAMI 2 G26[2] G26[3]")
        wrt("NAME G26[2] '_est'")
        wrt("NAME G26[3] '_SDs'")
        wrt(paste("PSTA '",MIfile, "' '_est' '_SDs' '_esample' ",sep=""))
        wrt("ERAS  '_est' '_SDs'")
      }
      wrt("ENDS")
      wrt("")
      wrt("LINK 0 G26")
    }
    
    wrt("NOTE export parameter chain")
    wrt("NAME   c1091 'deviance'")
    wrt("NAME   c1090 'mcmcchains'")
    
    wrt("LINK 0 G25")
    wrt("LINK 0 G26")
    
    if (D[1]=='Multinomial'){
      nresp=length(levels(indata[,resp]))-1
      resp.names=levels(indata[,resp])[-as.numeric(D[5])]
      
      if (is.list(expl)){
        nonfp.s=nonfp.sep
        for (i in 1:length(resp.names)){
          if (D['mode']==0){
            nonfp.s=gsub(paste(".",resp.names[i],sep=""),"", nonfp.s)
          }
          if (D['mode']==1){
            nonfp.s=gsub(paste(".(>=",resp.names[i],")",sep=""),"", nonfp.s)
          }
        }
        nonfp.s=unique(nonfp.s)
        if (is.na(sep.coeff[1])) sep.coeff <- character(0)
        for (p in sep.coeff){
          if (is.na(nonfp.sep[1])||sum(p==nonfp.s)==0){
            if (is.null(categ)|| sum(p==categ["var",])==0){
              for (j in 1:nresp){
                wrt("LINK 1 G25")
                wrt(paste("NAME G25[1] 'FP_",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                wrt(paste("DESC G25[1] 'FP:",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                wrt("GSET 2 G26 G25 G26")
                wrt("LINK 0 G25")
              }
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (j in 1:nresp){
                  for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                    wrt("LINK 1 G25")
                    wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt("GSET 2 G26 G25 G26")
                    wrt("LINK 0 G25")
                  }
                }
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (j in 1:nresp){
                  for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                    wrt("LINK 1 G25")
                    wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt("GSET 2 G26 G25 G26")
                    wrt("LINK 0 G25")
                  }
                }
              }
            }
          }
        }
        #svec.common <- character(0)
        kk <- 1
        tempid=1:(nresp+1)
        tempid=tempid[-as.numeric(D["ref.cat"])]
        for (p in common.coeff){
          newp <- paste(p, paste(tempid[as.logical(common.coeff.id[kk,])], collapse=""), sep=".")
          kk <- kk + 1
          nonfp.c=nonfp.common
          if (is.na(nonfp.common[1])||sum(newp==nonfp.c)==0){
            if (is.null(categ)|| sum(p==categ["var",])==0){
              wrt("LINK 1 G25")
              wrt(paste("NAME G25[1] 'FP_",chartr(".","_",newp),"'",sep=""))
              wrt(paste("DESC G25[1] 'FP:",chartr(".","_",newp),"'",sep=""))
              wrt("GSET 2 G26 G25 G26")
              wrt("LINK 0 G25")
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                  wrt("LINK 1 G25")
                  wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt("GSET 2 G26 G25 G26")
                  wrt("LINK 0 G25")
                }
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                  wrt("LINK 1 G25")
                  wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt("GSET 2 G26 G25 G26")
                  wrt("LINK 0 G25")
                }
              }
            }
          }
        }
      }else{
        nonfp.s=nonfp
        for (i in 1:length(resp.names)){
          if (D['mode']==0){
            nonfp.s=gsub(paste(".",resp.names[i],sep=""),"", nonfp.s)
          }
          if (D['mode']==1){
            nonfp.s=gsub(paste(".(>=",resp.names[i],")",sep=""),"", nonfp.s)
          }
        }
        nonfp.s=unique(nonfp.s)
        expla=expl
        for (p in expla){
          if (is.na(nonfp[1])||sum(p==nonfp.s)==0){
            if (is.null(categ)|| sum(p==categ["var",])==0){
              for (j in 1:nresp){
                wrt("LINK 1 G25")
                wrt(paste("NAME G25[1] 'FP_",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                wrt(paste("DESC G25[1] 'FP:",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                wrt("GSET 2 G26 G25 G26")
                wrt("LINK 0 G25")
              }
              
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (j in 1:nresp){
                  for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                    wrt("LINK 1 G25")
                    wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt("GSET 2 G26 G25 G26")
                    wrt("LINK 0 G25")
                  }
                }
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (j in 1:nresp){
                  for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                    wrt("LINK 1 G25")
                    wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                    wrt("GSET 2 G26 G25 G26")
                    wrt("LINK 0 G25")
                  }
                }
              }
            }
          }
        }
      }
    }else{
      if (D[1]=="Multivariate Normal"||D[1]=="Mixed"){
        nresp=length(resp)
        
        if (is.list(expl)){
          nonfp.s=nonfp.sep
          for (i in 1:length(resp)){
            nonfp.s=gsub(paste(".",resp[i],sep=""),"", nonfp.s)
          }
          nonfp.s=unique(nonfp.s)
          if (is.na(sep.coeff[1])) sep.coeff <- character(0)
          for (p in sep.coeff){
            if (is.na(nonfp.sep[1])||sum(p==nonfp.s)==0){
              if (is.null(categ)|| sum(p==categ["var",])==0){
                for (j in 1:nresp){
                  wrt("LINK 1 G25")
                  wrt(paste("NAME G25[1] 'FP_",chartr(".","_",p),"_",resp[j],"'",sep=""))
                  wrt(paste("DESC G25[1] 'FP:",chartr(".","_",p),"_",resp[j],"'",sep=""))
                  wrt("GSET 2 G26 G25 G26")
                  wrt("LINK 0 G25")
                }
              }else{
                if (is.na(categ["ref",which(p==categ["var",])])){
                  categ.names=levels(indata[[p]])
                  for (j in 1:nresp){
                    for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                      wrt("LINK 1 G25")
                      wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt("GSET 2 G26 G25 G26")
                      wrt("LINK 0 G25")
                    }
                  }
                }else{
                  categ.names=levels(indata[[p]])
                  refx=categ["ref",which(p==categ["var",])]
                  categ.names=categ.names[-which(refx==categ.names)]
                  for (j in 1:nresp){
                    for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                      wrt("LINK 1 G25")
                      wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt("GSET 2 G26 G25 G26")
                      wrt("LINK 0 G25")
                    }
                  }
                }
              }
            }
          }
          kk <- 1
          for (p in common.coeff){
            newp <- paste(p, paste(which(as.logical(common.coeff.id[kk,])), collapse=""), sep=".")
            kk <- kk + 1
            print(newp)
            nonfp.c=nonfp.common
            if (is.na(nonfp.common[1])||sum(newp==nonfp.c)==0){
              if (is.null(categ)|| sum(p==categ["var",])==0){
                wrt("LINK 1 G25")
                wrt(paste("NAME G25[1] 'FP_",chartr(".","_",newp),"'",sep=""))
                wrt(paste("DESC G25[1] 'FP:",chartr(".","_",newp),"'",sep=""))
                wrt("GSET 2 G26 G25 G26")
                wrt("LINK 0 G25")
              }else{
                if (is.na(categ["ref",which(p==categ["var",])])){
                  categ.names=levels(indata[[p]])
                  for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                    wrt("LINK 1 G25")
                    wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                    wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                    wrt("GSET 2 G26 G25 G26")
                    wrt("LINK 0 G25")
                  }
                }else{
                  categ.names=levels(indata[[p]])
                  refx=categ["ref",which(p==categ["var",])]
                  categ.names=categ.names[-which(refx==categ.names)]
                  for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                    wrt("LINK 1 G25")
                    wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                    wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                    wrt("GSET 2 G26 G25 G26")
                    wrt("LINK 0 G25")
                  }
                }
              }
            }
          }
        }else{
          nonfp.s=nonfp
          for (i in 1:length(resp)){
            nonfp.s=gsub(paste(".",resp[i],sep=""),"", nonfp.s)
          }
          nonfp.s=unique(nonfp.s)
          expla=expl
          for (p in expla){
            if (is.na(nonfp[1])||sum(p==nonfp.s)==0){
              if (is.null(categ)|| sum(p==categ["var",])==0){
                for (j in 1:nresp){
                  wrt("LINK 1 G25")
                  wrt(paste("NAME G25[1] 'FP_",chartr(".","_",p),"_",resp[j],"'",sep=""))
                  wrt(paste("DESC G25[1] 'FP:",chartr(".","_",p),"_",resp[j],"'",sep=""))
                  wrt("GSET 2 G26 G25 G26")
                  wrt("LINK 0 G25")
                }
              }else{
                if (is.na(categ["ref",which(p==categ["var",])])){
                  categ.names=levels(indata[[p]])
                  for (j in 1:nresp){
                    for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                      wrt("LINK 1 G25")
                      wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt("GSET 2 G26 G25 G26")
                      wrt("LINK 0 G25")
                    }
                  }
                }else{
                  categ.names=levels(indata[[p]])
                  refx=categ["ref",which(p==categ["var",])]
                  categ.names=categ.names[-which(refx==categ.names)]
                  for (j in 1:nresp){
                    for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                      wrt("LINK 1 G25")
                      wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                      wrt("GSET 2 G26 G25 G26")
                      wrt("LINK 0 G25")
                    }
                  }
                }
              }
            }
          }
        }
      }else{
        expla=expl
        for (p in expla){
          if (is.na(nonfp[1])||sum(p==nonfp)==0){
            if (is.null(categ)|| sum(p==categ["var",])==0){
              wrt("LINK 1 G25")
              wrt(paste("NAME G25[1] 'FP_",p,"'",sep=""))
              wrt(paste("DESC G25[1] 'FP:",p,"'",sep=""))
              wrt("GSET 2 G26 G25 G26")
              wrt("LINK 0 G25")
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                  wrt("LINK 1 G25")
                  wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt("GSET 2 G26 G25 G26")
                  wrt("LINK 0 G25")
                }
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                  wrt("LINK 1 G25")
                  wrt(paste("NAME G25[1] 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt(paste("DESC G25[1] 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                  wrt("GSET 2 G26 G25 G26")
                  wrt("LINK 0 G25")
                }
              }
            }
          }
        }
      }
    }
    
    wrt.resid=function(rpx, resid.lev){
      nrpx=length(rpx)
      for (j in 1: nrpx){
        for (i in 1:j){
          if (i==j){
            wrt("LINK 1 G25")
            wrt(paste("NAME G25[1] 'RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),"'",sep=""))
            wrt(paste("DESC G25[1] 'RP",resid.lev,":var(",chartr(".", "_", rpx[i]),")'",sep=""))
            wrt("GSET 2 G26 G25 G26")
            wrt("LINK 0 G25")
          }else{
            wrt("LINK 1 G25")
            wrt(paste("NAME G25[1] 'RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),"'",sep=""))
            wrt(paste("DESC G25[1] 'RP",resid.lev,":cov(",chartr(".", "_", rpx[i]),",",chartr(".", "_", rpx[j]),")'",sep=""))
            wrt("GSET 2 G26 G25 G26")
            wrt("LINK 0 G25")
          }
        }
      }
    }
    
    wrt.resid2=function(rpx, resid.lev, clre){
      nrpx=length(rpx)
      nclre=ncol(clre)
      k=1
      for (j in 1: nrpx){
        for (i in 1:j){
          if (i==j){
            if (resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[i]==clre[3,k]){
              if (k<ncol(clre)) k=k+1
            }else{
              wrt("LINK 1 G25")
              wrt(paste("NAME G25[1] 'RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),"'",sep=""))
              wrt(paste("DESC G25[1] 'RP",resid.lev,":var(",chartr(".", "_", rpx[i]),")'",sep=""))
              wrt("GSET 2 G26 G25 G26")
              wrt("LINK 0 G25")
            }
          }else{
            if ((resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[j]==clre[3,k])||
                  (resid.lev==as.numeric(clre[1,k])&&rpx[j]==clre[2,k]&&rpx[i]==clre[3,k])){
              if (k<ncol(clre)) k=k+1
            }else{
              wrt("LINK 1 G25")
              wrt(paste("NAME G25[1] 'RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),"'",sep=""))
              wrt(paste("DESC G25[1] 'RP",resid.lev,":cov(",chartr(".", "_", rpx[i]),",",chartr(".", "_", rpx[j]),")'",sep=""))
              wrt("GSET 2 G26 G25 G26")
              wrt("LINK 0 G25")
            }
          }
        }
      }
    }
    
    wrt.resid3=function(rpx, resid.lev){
      nrpx=length(rpx)
      for (j in 1: nrpx){
        for (i in 1:j){
          if (i==j){
            wrt("LINK 1 G25")
            wrt(paste("NAME G25[1] 'RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),"'",sep=""))
            wrt(paste("DESC G25[1] 'RP",resid.lev,":var(",chartr(".", "_", rpx[i]),")'",sep=""))
            wrt("GSET 2 G26 G25 G26")
            wrt("LINK 0 G25")
          }
        }
      }
    }
    
    if (nrp>0){
      for (ii in 1:nrp){
        if(!is.null(fact)){
          wrt.resid3(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])))
        }else{
          if (is.null(clre)){
            wrt.resid(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])))
          }else{
            wrt.resid2(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])), clre)
          }
        }
      }
    }
    # Add in extra parameters ect.
    if (D[1]=='Multinomial'&&as.numeric(D["mode"])==0){
      wrt("LINK 1 G25")
      wrt(paste("NAME G25[1] 'RP1_bcons_1'",sep=""))
      wrt(paste("DESC G25[1] 'RP1:bcons_1'",sep=""))
      wrt("GSET 2 G26 G25 G26")
      wrt("LINK 0 G25")
    }
    
    wrt("GSIZ G26 b1000")
    wrt("LINK 3 G27")
    wrt(paste("CODE ",iterations/thinning, "b1000", 1, "G27[1]"))
    wrt(paste0("CALC G27[1] = G27[1] * ", thinning))
    wrt("NAME   G27[1] 'itnum'")
    wrt(paste("CODE b1000", 1,iterations/thinning, "G27[2]"))
    wrt("NAME   G27[2] 'parnum'")
    wrt("NAME   G27[3] 'iteration'")
    wrt("DESC   G27[3] '\\Iteration'")
    
    wrt("UNVE b1000 'parnum' 'itnum' 'mcmcchains' 'iteration' G26")
    if(D[1]=='Multinomial'&&as.numeric(D["mode"])==1){
      wrt("LINK 1 G25")
      wrt(paste("PUT ",iterations/thinning, 1, "G25[1]"))
      wrt("NAME G25[1] 'RP1_bcons_1'")
      wrt("DESC G25[1] 'RP1:bcons_1'")
      wrt("GSET 2 G26 G25 G26")
      wrt("LINK 0 G25")
    }
    wrt(paste0("PSTA '",chainfile, "' ","'iteration' 'deviance' G26"))
    wrt("ERAS 'itnum' 'parnum' 'iteration' G26")
    wrt("LINK 0 G27")
    wrt("LINK 0 G26")
    
    calcresiduals = function(level, displevel, rpx, resioptions, clre=clre){
      wrt("")
      if (!("norecode"%in%resioptions)){
        wrt("MISR 0")
      }
      
      len.rpx = length(rpx)
      
      ii=1
      wrt(paste("LINK", len.rpx, "G26"))
      for (k in 1:len.rpx){
        if (!is.null(clre)){
          if (!(level==as.numeric(clre[1,ii])&&rpx[k]==clre[2,ii]&&rpx[k]==clre[3,ii])){
            wrt(paste0("NAME G26[", k, "] ", paste0("'lev_",displevel,"_resi_est_",rpx[k],"'")))
            wrt(paste0("DESC G26[", k, "] ", "'residual estimates'"))
          }else{
            if (ii<ncol(clre)) ii = ii+1
          }
        }else{
          wrt(paste0("NAME G26[", k, "] ", paste0("'lev_",displevel,"_resi_est_",rpx[k],"'")))
          wrt(paste0("DESC G26[", k, "] ", "'residual estimates'"))
        }
      }

      wrt(paste("LINK", len.rpx, "G27"))
      if ("variance" %in% resioptions){
        ii=1
        for (k in 1:len.rpx){
          if (!is.null(clre)){
            if (!(level==as.numeric(clre[1,ii])&&rpx[k]==clre[2,ii]&&rpx[k]==clre[3,ii])){
              wrt(paste0("NAME G27[",k,"] ", paste0("'lev_",displevel,"_resi_variance_",rpx[k],"'")))
              wrt(paste0("DESC G27[",k,"] ", "'residual variance'"))
            }else{
              if (ii<ncol(clre)) ii = ii+1
            }
          }else{
            wrt(paste0("NAME G27[",k,"] ", paste0("'lev_",displevel,"_resi_variance_",rpx[k],"'")))
            wrt(paste0("DESC G27[",k,"] ", "'residual variance'"))
          }
        }
      }else{
        residual_se=NULL
        ii=1
        for (k in 1:len.rpx){
          if (!is.null(clre)){
            if (!(level==as.numeric(clre[1,ii])&&rpx[k]==clre[2,ii]&&rpx[k]==clre[3,ii])){
              wrt(paste0("NAME G27[",k,"] ", paste0("'lev_",displevel,"_resi_se_",rpx[k],"'")))
              wrt(paste0("DESC G27[",k,"] ", "'residual standard error'"))
            }else{
              if (ii<ncol(clre)) ii = ii+1
            }
          }else{
            wrt(paste0("NAME G27[",k,"] ", paste0("'lev_",displevel,"_resi_se_",rpx[k],"'")))
            wrt(paste0("DESC G27[",k,"] ", "'residual standard error'"))
          }
        }
      }
      wrt("RFUN")
      wrt("ROUT G26 G27")
      wrt("")
      
      wrt(paste("RLEV   ",level,sep=""))
      wrt("RCOV   1")

      outgroups <- c("G26","G27")

      if ("standardised"%in%resioptions){
        wrt(paste("LINK", len.rpx, "G28"))
        ii=1
        for (k in 1:len.rpx){
          if (!is.null(clre)){
            if (!(level==as.numeric(clre[1,ii])&&rpx[k]==clre[2,ii]&&rpx[k]==clre[3,ii])){
              wrt(paste0("NAME G28[",k,"] ", paste0("'lev_",displevel,"_std_resi_est_",rpx[k],"'")))
              wrt(paste0("DESC G28[",k,"] ", "'std standardised residual'"))
            }else{
              if (ii<ncol(clre)) ii = ii+1
            }
          }else{
            wrt(paste0("NAME G28[",k,"] ", paste0("'lev_",displevel,"_std_resi_est_",rpx[k],"'")))
            wrt(paste0("DESC G28[",k,"] ", "'std standardised residual'"))
          }
        }
        
        wrt("RTYP   0")
        wrt("MCRE")

        ccount =1
        if (!("variance" %in% resioptions)){
          for (k in 1:len.rpx){
            wrt(paste0("CALC G28[",k,"]=G28[",k,"]/sqrt(G27[",k,"])",sep=""))
          }
        }
        outgroups <- c(outgroups, "G28")
      }
            
      wrt("RTYP   1")# Compute comparative variances
      wrt("MCRE")
      
      if (!("variance"%in%resioptions)){
        for (k in 1:len.rpx){
          wrt(paste0("CALC G28[",k,"]=sqrt(G28[",k,"])"))# Convert the variances to standard errors
        }
      }
      
      wrt("")
      wrt(paste0("NOBS ", level, " b30 b31"))
      wrt("LINK 1 G29")
      wrt("GENE 1 b30 1 G29[1]")
      wrt(paste0("NAME G29[1] 'lev_",displevel,"_residualid'"))
      outgroups <- c(outgroups, "G29")
      if (!("norecode"%in%resioptions)){
        wrt("MISR 1")
      }
      wrt("")
    }
    
    if (resi.store&&nrp>0){
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
      
      if (!is.null(resi.store.levs)){
        resiname=rep(NA,length(resi.store.levs))
        for (i in 1:length(resi.store.levs)){
          resiname[i]=paste("'resi_lev",resi.store.levs[i],"'",sep="")
        }
        wrt(paste("PSTA '",resichains,"' ",paste(resiname,collapse=" "),sep=""))
      }
    }
  }
  if (!debugmode) wrt("EXIT")
}
