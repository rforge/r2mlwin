runMLwiN <- function(Formula, levID, D="Normal", data=NULL, estoptions=list(EstM=0), BUGO=NULL, MLwiNPath=NULL,stdout="",stderr="",workdir=tempdir(),checkversion=TRUE, indata=NULL) {
  if (!is.null(indata) && !is.null(data)) {
    stop("Only one of data and indata can be specified")
  }
  if (!is.null(data)) {
    indata <- data
  }
  if (is.null(indata)) {
    stop("Using the currently attached data is not yet implemented")
  }

  if (length(D) == 1) {
    if (!(D %in% c("Normal", "Binomial", "Poisson", "Negbinom", "Multivariate Normal", "Ordered Multinomial", "Unordered Multinomial"))) {
      stop("Invalid distribution specified")
    }
  } else {
    if (D[1] != "Mixed") {
      stop("Invalid distribution specified")
    } else {
      for (i in 2:length(D)) {
        if (estoptions$EstM == 0) {
          if (!(D[i] %in% c("Normal", "Binomial", "Poisson"))) {
            stop("Invalid distribution specified")
          }
        } else {
          if (!(D[i] %in% c("Normal", "Binomial"))) {
            stop("Invalid distribution specified")
          }
        }
      }
    }
  }
  
  # Check MLwiNPath is usable and set command/args
  debugmode=estoptions$debugmode
  if(is.null(debugmode)) debugmode=F
  
  x64=estoptions$x64
  if(is.null(x64)) {
    if (.Machine$sizeof.pointer == 8) {
      x64=TRUE
    } else {
      x64=FALSE
    }
  }
  
  if (is.null(MLwiNPath)) {
    MLwiNPath <- getOption("MLwiN_path")
  }
  
  pathinfo <- file.info(MLwiNPath)
  if (is.na(pathinfo$isdir)) {
    stop(paste0(MLwiNPath, " does not exist"))
  }
  
  if (pathinfo$isdir == FALSE) {
    if (file.access(MLwiNPath, mode=1) == 0) {
      cmd <- MLwiNPath
    } else {
      stop(paste0(MLwiNPath, " is not executable"))
    }
  }
  
  if (pathinfo$isdir == TRUE) {
    if (debugmode) {
      cmd <- paste0(MLwiNPath, "/i386/mlwin.exe")
      if (file.access(cmd, mode=1) != 0) {
        cmd <- paste0(MLwiNPath, "/mlwin.exe")
      } else {
        if (file.access(cmd, mode=1) != 0) {
          cmd <- paste0(MLwiNPath, "/i386/mlnscript.exe")
          if (file.access(cmd, mode=1) != 0) {
            stop("Cannot find valid MLwiN executable")
          }
        }
      }
    } else {
      if (x64) {
        cmd <- paste0(MLwiNPath, "/x64/mlnscript.exe")
        if (file.access(cmd, mode=1) != 0) {
          cmd <- paste0(MLwiNPath, "/i386/mlnscript.exe")
          if (file.access(cmd, mode=1) != 0) {
            cmd <- paste0(MLwiNPath, "/mlwin.exe")
            if (file.access(cmd, mode=1) != 0) {
              stop("Cannot find valid MLwiN executable")
            }
          }
        }
      } else {
        cmd <- paste0(MLwiNPath, "/i386/mlnscript.exe")
        if (file.access(cmd, mode=1) != 0) {
          cmd <- paste0(MLwiNPath, "/mlwin.exe")
          if (file.access(cmd, mode=1) != 0) {
            stop("Cannot find valid MLwiN executable")
          }
        }
      }
    }
  }  
  
  
  versioninfostr <- "
version:date:md5:filename:x64:trial
1.10:Jun 2001:44e840796f3c43113c45ac8fe7e0633a:mlwin.exe:FALSE:FALSE
1.10:Jul 2001:21e3a2d85e6f9c4bb8d926658981a020:mlwin.exe:FALSE:FALSE
2.00:May 2004:4ddb67c6426112bafc70bddca38cd63a:mlwin.exe:FALSE:FALSE
2.00:Jun 2004:bf9aff9fa66d8eadc8b3a170e616ab58:mlwin.exe:FALSE:FALSE
2.00:Jul 2004:f78c7d6d3a0dc82e7ea7e391a70ebb02:mlwin.exe:FALSE:FALSE
2.00:Nov 2004:e9fbafdc5715921dcadec601e3cec593:mlwin.exe:FALSE:FALSE
2.01:Dec 2004:359dbe8d728b2841b948504b5c272392:mlwin.exe:FALSE:FALSE
2.01:Feb 2004:f4a164b199b37b33158b194402034756:mlwin.exe:FALSE:FALSE
2.02:Jun 2005:cfb2ba2aea080ad69189709e87613ef7:mlwin.exe:FALSE:FALSE
2.10:Feb 2009:d96fdc4d9876206837d5c720bf37c8e1:mlwin.exe:FALSE:FALSE
2.11:Apr 2009:1da1348d7a65a3a7ae1f310d63520429:mlwin.exe:FALSE:FALSE
2.12:Jul 2009:7f44b98b0ca60ea6b34ee56f962869c7:mlwin.exe:FALSE:FALSE
2.13:Aug 2009:b435c8137676da09412ee6a57d7426cc:mlwin.exe:FALSE:FALSE
2.14:Sep 2009:2e493aa7cdf221caed82c0cdc4facb17:mlwin.exe:FALSE:FALSE
2.15:Oct 2009:3ca55fe4c04f546040fc4937f0ac1a9f:mlwin.exe:FALSE:FALSE
2.16:Nov 2009:afd80cecbe7e1164957f4530b07ea5ec:mlwin.exe:FALSE:FALSE
2.16:Nov 2009:a91958a92ee4e44bf80a58cb8c5a319a:mlwin.exe:FALSE:TRUE
2.17:Jan 2010:28e92f9aba0431d2a53ab4ea0c1471e6:mlwin.exe:FALSE:FALSE
2.18:Mar 2010:d0e7c52a33024ffe3bb176fac6fdd724:mlwin.exe:FALSE:FALSE
2.19:May 2010:46e40433d3f22f947ffc539bfddff58a:mlwin.exe:FALSE:FALSE
2.19:May 2010:2cd0159a1580452c43358511f763fb78:mlwin.exe:FALSE:TRUE
2.20:Jun 2010:cf9ba18ef770d1d5e761b99bd74cfb48:mlwin.exe:FALSE:FALSE
2.21:Oct 2010:71f36aecbbef624f70251d273547bae5:mlwin.exe:FALSE:FALSE
2.22:Dec 2010:d372e2ea4d3dd8202bddc0fc3e3be445:mlwin.exe:FALSE:FALSE
2.23:Apr 2011:0a150498818a6e519e1fb5f4c96863df:mlwin.exe:FALSE:FALSE
2.23:Apr 2011:dd6304ed39b0fd769e1e40e4b85e5b8f:mlwin.exe:FALSE:TRUE
2.24:Sep 2011:8b8a5d06d4440de87fa97359d06da8d6:mlwin.exe:FALSE:FALSE
2.24:Sep 2011:005a73f0c8af424520147151d504fffb:mlwin.exe:FALSE:TRUE
2.25:Feb 2012:a5a8e56a2da1faa75a0bf5a9c260f79b:mlwin.exe:FALSE:FALSE
2.25:Feb 2012:ce92f10b5146c3d4fd10c1cad78d01d3:mlwin.exe:FALSE:TRUE
2.26:Sep 2012:f915c285f8409fe66bf8ac0a90256fe7:mlwin.exe:FALSE:FALSE
2.26:Sep 2012:8bdcb5ef1b4a1c10b7a5f2e1c359fae4:mlwin.exe:FALSE:TRUE
2.26:Sep 2012:d4b3b6a97e0d413bf185debd18a7c388:mlnscript.exe:FALSE:FALSE
2.26:Sep 2012:6d06f90db77a9f4d3bf973cfd4be6aad:mlnscript.exe:TRUE:FALSE
2.27:Feb 2013:e25a7fb9431c024e2f09222434d9fc55:mlwin.exe:FALSE:FALSE
2.27:Feb 2013:d0330c49c3234474b0e7d79fcd83117d:mlwin.exe:FALSE:TRUE
2.27:Feb 2013:5bc7e8fade28bd8fb9f9ef110ec56afc:mlnscript.exe:FALSE:FALSE
2.27:Feb 2013:63fa77b06439f295231dd4795e4ed99e:mlnscript.exe:TRUE:FALSE
2.28:Jul 2013:6bdadad3615c49ca418cc63cb952d37f:mlwin.exe:FALSE:FALSE
2.28:Jul 2013:6a1f0d366ffa622e4a052695a6013e2c:mlwin.exe:FALSE:TRUE
2.28:Jul 2013:5fcbc7d0dd0a900c99ec411018ccdaa5:mlnscript.exe:FALSE:FALSE
2.28:Jul 2013:d299e1156e5f7ef909a182abf637bb0d:mlnscript.exe:TRUE:FALSE
2.29:Dec 2013:5f0a87e6cb7198d796f9664a05d5031a:mlwin.exe:FALSE:FALSE
2.29:Dec 2013:5afdf13c0406202aaf308b569052dd20:mlwin.exe:FALSE:TRUE
2.29:Dec 2013:47fbc35bf375d56d2291a3f85d2d838c:mlnscript.exe:FALSE:FALSE
2.29:Dev 2013:4d39f330c201e7614df17150f8aab74f:mlnscript.exe:TRUE:FALSE
2.30:Feb 2014:869c73b95daf1ec92c2b22277bd94724:mlwin.exe:FALSE:FALSE
2.30:Feb 2014:022ba981c2bf8751dad35c041f5f7db3:mlwin.exe:FALSE:TRUE
2.30:Feb 2014:b0f739262853e594242a6d4dad296eb6:mlnscript.exe:FALSE:FALSE
2.30:Feb 2014:c964df5ff4011eae94419c2f815a9450:mlnscript.exe:TRUE:FALSE
"
  versioninfo <- read.delim(textConnection(versioninfostr), header=TRUE, sep=":",strip.white=TRUE)
  if (checkversion == TRUE) { # Allow disabling the version check if it is slowing things down (e.g. in a simulation study)
    currentver = versioninfo[versioninfo$md5==digest(cmd, algo="md5", file=TRUE),]  
    if (nrow(currentver) == 0) {
      versiontext = "MLwiN version unknown or >2.30"
    } else {
      if (currentver$version < 2.28) { # Block versions >year older than current release
        stop("The current version of MLwiN is too old, please update it from http://www.bris.ac.uk/cmm/software/mlwin/download/upgrades.html")
      }
      versiontext = paste0("MLwiN version ", currentver$version)
    }
  } else {
    versiontext = "MLwiN version unchecked"
  }
  
  # the current function call
  cl <- match.call()

  invars = Formula.translate(Formula, levID, D, indata)

  EstM=estoptions$EstM
  if (is.null(EstM)) EstM=0
  if (EstM != 0 && EstM != 1) {
    stop("Invalid EstM option (can be zero or one)")
  }

  resp = invars$resp
  expl = invars$expl

  D = invars$D
  if (EstM == 0) {
    if (!is.element(D[1], c("Normal", "Binomial", "Poisson", "Negbinom", "Multivariate Normal", "Mixed", "Multinomial"))) {
      stop(cat("Invalid distribution specified", D[1]))
    }
    if (D[1] == "Binomial") {
      if (!is.element(D[2], c("logit", "probit", "cloglog"))) {
        stop(cat("Invalid link function specified", D[2]))
      }
      if (is.na(D[3])) {
        stop("A denominator must be specified for a Binomial response")
      }
    }
    if (D[1] == "Mixed") {
      for (i in 2:length(D)) {
        if (!is.element(D[[i]][[1]], c("Normal", "Binomial", "Poisson"))) {
          stop(cat("Invalid distribution specified", D[[i]][[1]]))
        }
        if (D[[i]][[1]] == "Binomial") {
          if (!is.element(D[[i]][[2]], c("logit", "probit", "cloglog"))) {
            stop(cat("Invalid link function specified", D[[i]][[2]]))
          }
          if (is.na(D[[i]][[3]])) {
            stop("A denominator must be specified for Binomial responses")
          }
        }
      }
    }
    if (D[1] == "Multinomial") {
      if (D[4] == 1) { # Unordered
        if (D[2] != "logit") {
          stop("Invalid link function specified")
        }
      }
      if (D[4] == 0) { # Ordered
        if (!is.element(D[2], c("logit", "probit", "cloglog"))) {
          stop(cat("Invalid link function specified", D[2]))
        }
      }
    }
  } else {
    if (!is.element(D[1], c("Normal", "Binomial", "Poisson", "Multivariate Normal", "Mixed", "Multinomial"))) {
      stop(cat("Invalid distribution specified", D[1]))
    }
    if (D[1] == "Binomial") {
      if (!is.element(D[2], c("logit", "probit", "cloglog"))) {
          stop(cat("Invalid link function specified", D[2]))
      }
    }
    if (D[1] == "Mixed") {
      for (i in 2:length(D)) {
        if (!is.element(D[[i]][[1]], c("Normal", "Binomial"))) {
          stop(cat("Invalid distribution specified", D[[i]][[1]]))
        }
        if (D[[i]][[1]] == "Binomial") {
          if (!D[[i]][[2]] == "probit") {
            stop(cat("Invalid link function specified", D[[i]][[2]]))
          }
        }
      }
    }
    if (D[1] == "Multinomial") {
      if (D[4] == 1) { # Unordered
        if (D[2] != "logit") {
          stop("Invalid link function specified")
        }
      }
      if (D[4] == 0) { # Ordered
        if (!is.element(D[2], c("logit", "probit", "cloglog"))) {
          stop(cat("Invalid link function specified", D[2]))
        }
      }
    }
  }

  rp = invars$rp
  if (D[1] != "Normal" || D[1] != "Mixed") {
    if (D[1] == "Binomial") {
      if (any(rp$rp1 != "bcons.1")) stop("Variables cannot be made random at level one in Binomial models")
    }
    if (D[1] == "Poisson") {
      if (any(rp$rp1 != "bcons.1")) stop("Variables cannot be made random at level one in Poisson models")
    }
    if (D[1] == "Negbinom") {
      if (suppressWarnings(any(rp$rp1 != c("bcons.1", "bcons2.1")))) stop("Variables cannot be made random at level one in Negative-binomial models")
    }
    if (D[1] == "Mixed") {
      for (i in 2:length(D)) {
        if (D[[i]][[1]] == "Binomial") {
          rp1 <- rp$rp1[names(rp$rp1) == resp[i-1]]
          if (length(rp1) > 0) stop("Variables cannot be made random at level one for Binomial responses")
        }
        if (D[[i]][[1]] == "Poisson") {
          rp1 <- rp$rp1[names(rp$rp1) == resp[i-1]]
          if (length(rp1) > 0) stop("Variables cannot be made random at level one for Poisson responses")
        }
      }
    }
    if (D[1] == "Multinomial") {
      if (!is.null(rp$rp1)) {
        stop("Variables cannot be made random at level one in multinomial models")
      }
    }
  }
  nonfp = invars$nonfp
  if(is.null(nonfp)) {
    if(is.list(expl)){
      nonfp = list("nonfp.sep"=NA,"nonfp.common"=NA)
    }else{
      nonfp = NA
    }
  }
  categ = invars$categ
  if(is.null(categ)){
    categ=NULL
  }else{
    if (is.null(rownames(categ))){
      rownames(categ)=c("var","ref","ncateg")
    }
  }
  
  if (D[1]=='Multinomial'||D[1]=='Multivariate Normal'||D[1]=='Mixed'){
    levID=c(levID,NA)
  }
   
  show.file=estoptions$show.file
  if (is.null(show.file)) show.file = F
  
  resi.store=estoptions$resi.store
  if (is.null(resi.store)) resi.store=F
  
  resioptions=estoptions$resioptions
  if (is.null(resioptions)){
    if (EstM==0){
      resioptions = c("variance", "sampling")
    }
    else{
      resioptions = c("variance")
    }
  }
  if ("variance"%in%resioptions&&("standardised"%in%resioptions||"deletion"%in%resioptions||"leverage"%in%resioptions)){
    stop("variance will not be calculated together with standardised or deletion or leverage. Please remove variance in resioptions, and then standard error will be calculated instead.")
  }

  if (EstM == 1 && ("sampling" %in% resioptions || "leverage" %in% resioptions || "infleuence" %in% resioptions || "deletion" %in% resioptions)) {
    stop("Invalid residual option specified for MCMC estimation")
  }
  
  resi.store.levs = estoptions$resi.store.levs
  if (EstM == 0 && !is.null(resi.store.levs)) {
    stop("resi.store.levs option is not valid for (R)IGLS estimation")
  }
  
  weighting = estoptions$weighting
  if (EstM == 1 && !is.null(weighting)) {
    stop("weighting option is not valid for MCMC estimation")
  }
  if (!is.null(weighting) && (!is.element(D[1], c("Normal", "Poisson", "Binomial", "Negbinom")))) {
    stop("weighting can only be used in univariate models")
  }
  
  centring = estoptions$centring
  
  clean.files=estoptions$clean.files
  if (is.null(clean.files)) clean.files=T
   
  clre=estoptions$clre
  smat=estoptions$smat

  if (!is.null(smat)) {
    if (!is.matrix(smat)) {
      smat <- as.matrix(smat)
    }
    for (i in 1:ncol(smat)) {
      if (smat[2, i] == 1) {
        lev <- smat[1, i]
        for (j in 1:length(rp[[paste0("rp", lev)]])) {
          for (k in 1:j) {
            if (j != k) clre <- cbind(clre, c(lev, rp[[paste0("rp", lev)]][j], rp[[paste0("rp", lev)]][k]))
          }
        }
      }
    }
    smat <- NULL
  }

  notation=estoptions$notation
  if(is.null(notation)) notation="level"
  
  mem.init=estoptions$mem.init
  if(is.null(mem.init)) mem.init="default"
  
  optimat=estoptions$optimat
  if(is.null(optimat)) optimat=F

  maxiter=estoptions$maxiter
  if(is.null(maxiter)) maxiter=20

  convtol=estoptions$tol
  if(is.null(convtol)) convtol=2

  extra=estoptions$extra
  if(is.null(extra)) extra=F
  if (extra == TRUE) {
    if (EstM != 0) {
      stop("extra can only be specified for (R)IGLS models")
    }
    if (D[1] != "Poisson" && D[1] != "Binomial" && D[1] != "Negbinom" && D[1] != "Multinomial") {
      stop("extra can only be specified for discrete outcome models")
    }
  }
  
  nonlinear=estoptions$nonlinear
  if (!is.null(nonlinear)) {
    if (D[1] == "Normal" || D[1] == "Multivariate Normal") {
      stop("Nonlinear options are not valid for normal models")
    }
  }
  if (is.null(nonlinear)) nonlinear=c(0,1)
  if (length(na.omit(levID)) == 1 && any(nonlinear != c(0,1))) {
    stop("Only MQL1 is valid for one-level discrete models")
  }

  if (nonlinear[1] < 0 || nonlinear[1] > 1) {
    stop("Invalid nonlinear option")
  }

  if (nonlinear[2] < 1 || nonlinear[2] > 2) {
    stop("Invalid nonlinear option")
  }
  
  Meth=estoptions$Meth
  if(is.null(Meth)) Meth=1
  
  reset=estoptions$reset
  if (is.null(reset)) {
    if (EstM == 0) {
      reset=rep(0, length(levID))
      reset[1] <- 2
    }
  }
  if (!is.null(reset)) {
    if (EstM == 1) {
      stop("reset is only available for (R)IGLS models")
    } else {
      if (length(reset) != length(levID)) {
        stop("reset vector is wrong length")
      }
      if (any(reset<0 || reset>2)) {
        stop("Invalid reset value")
      }
    }
  }

  fcon <- NULL
  rcon <- NULL
  if (!is.null(estoptions$constraints)) {
    if (EstM == 1) {
      stop("constraints are not available for MCMC estimation")
    } else {
      if (!is.null(estoptions$constraints$fixed.ui) || !is.null(estoptions$constraints$fixed.ci)) {
        if (is.null(estoptions$constraints$fixed.ui) || is.null(estoptions$constraints$fixed.ci)) {
          stop("fixed.ui and fixed.ci must be specified for fixed constaints")
        }
        if (ncol(estoptions$constraints$fixed.ui) != length(estoptions$constraints$fixed.ci)) {
          stop("number of columns in fixed.ui and fixed.ci must be equal for fixed constaints")
        }

        if (!is.list(expl)) {
          numfp <- length(setdiff(expl, nonfp))
        } else {
          numfp <- length(setdiff(expl$sep.coeff, nonfp$sep))*length(resp) + setdiff(expl$common.coeff, nonfp.common)
        }

        if (!is.vector(estoptions$constraints$fixed.ci)) {
          stop("fixed.ci should be a vector")
        }

        if (nrow(estoptions$constraints$fixed.ui) != numfp) {
          stop(paste("number of rows for fixed.ci must equal the number of fixed parameters:", numfp))
        }

        fcon <- rbind(estoptions$constraints$fixed.ui, estoptions$constraints$fixed.ci)
      }
      if (!is.null(estoptions$constraints$random.ui) || !is.null(estoptions$constraints$random.ci)) {
        if (is.null(estoptions$constraints$random.ui) || is.null(estoptions$constraints$random.ci)) {
          stop("random.ui and random.ci must be specified for fixed constaints")
        }
        if (ncol(estoptions$constraints$random.ui) != length(estoptions$constraints$random.ci)) {
          stop("number of columns in random.ui and random.ci must be equal for random constaints")
        }

        numrp <- 0
        for (ii in 1:length(rp)) {
          numrp <- numrp + (length(rp[[ii]])*(length(rp[[ii]])+1)/2)
        }

        if (!is.null(clre)){
          numrp <- numrp - ncol(clre)
        }

        if (!is.vector(estoptions$constraints$random.ci)) {
          stop("random.ci should be a vector")
        }

        if (nrow(estoptions$constraints$random.ui) != numrp) {
          stop(paste("number of rows for random.ui must equal the number of random parameters", numrp))
        }

        rcon <- rbind(estoptions$constraints$random.ui, estoptions$constraints$random.ci)
      }
    }
  }

  fact=estoptions$fact
  if (EstM == 0 && !is.null(fact)) {
    stop("Factor models not available with (R)IGLS estimation")
  }
  if (!is.null(fact) && D[1] != "Multivariate Normal") {
    stop("Factor models can only be defined for multivariate models")
  }

  if (!is.null(fact)) {
    for (i in 1:fact$nfact) {
      for (j in 1:length(rp[[paste0("rp", fact$lev.fact[i])]])) {
        for (k in 1:j) {
          if (j != k) clre <- cbind(clre, c(fact$lev.fact[i], rp[[paste0("rp", fact$lev.fact[i])]][j], rp[[paste0("rp", fact$lev.fact[i])]][k]))
        }
      }
    }
  }

  xclass=estoptions$xclass
  if (EstM == 0 && !is.null(xclass)) {
    stop("Cross-classification is not available with (R)IGLS estimation")
  }

  if (!is.null(xclass)) {
    for (i in 1:length(as.numeric(xclass$class))) {
      lev <- as.numeric(xclass$class[i])
      num <- as.numeric(xclass$N1[i])
      weightcol <- xclass$weight[i]
      idcol <- xclass$id[i]
      if (is.null(idcol) || is.na(idcol)) {
        idcol <- rev(na.omit(levID))[lev]
      }
      idstart = which(colnames(indata) == idcol)
      idend = idstart+(num-1)
      idmat <- indata[, colnames(indata)[idstart:idend]]
        
      if (!is.null(weightcol)) {
        weightstart = which(colnames(indata) == weightcol)
        weightend = weightstart+(num-1)
        weightmat <- indata[, colnames(indata)[weightstart:weightend]]

        # NOTE: These checks could probably be vectorised
        for (i in 1:nrow(idmat)) {
          for (j in 1:ncol(idmat)) {
            if (idmat[i, j] != 0 && weightmat[i, j] == 0) {
              stop(paste("The MM ID variable", j, "for observation", i, "is present but a zero MM weight has been specified for it"))
            }
            if (idmat[i, j] == 0 && weightmat[i, j] != 0) {
              stop(paste("The MM ID variable", j, "for observation", i, "is absent but a positive MM weight has been specified for it"))
            }
            for (k in 1:j) {
              if (k != j) {
                if (idmat[i, j] == idmat[i, k] && idmat[i, j] != 0) {
                  stop(paste("The MM ID variable", j, "for observation", i, "is duplicated in ID variable", k))
                }
              }
            }
          }
        }
      }
    }
  }

  carcentre=estoptions$carcentre
  if (is.null(carcentre)) {
    carcentre <- FALSE
  }
  carcount <- 0
  if (is.null(xclass)) {
    for(i in 1:length(as.numeric(xclass$class))) {
      if (length(xclass) > 4){
        if (xclass$car[i] == TRUE) {
          carcount <- carcount + 1
        }
        if (carcount > 1 && xclass$car[i] == TRUE && carcentre != TRUE) {
          stop("CAR can only apply to one level unless CAR centring is turned on")
        }
      }
    }
  }

  merr=estoptions$merr
  if (EstM == 0 && !is.null(merr)) {
    stop("Measurement error is not available with (R)IGLS estimation")
  }
  if (!is.null(merr) && (!is.element(D[1], c("Normal", "Poisson", "Binomial", "Negbinom")))) {
    stop("measurement error can only be used in univariate models")
  }
  mcmcMeth=estoptions$mcmcMeth
  if (EstM == 0 && !is.null(merr)) {
    stop("MCMC method options cannot be specified with (R)IGLS estimation")
  }


  FP.names=NULL
  
  if (D[1]=='Multinomial'){
    nresp=length(levels(indata[,resp]))-1
    resp.names=levels(indata[,resp])[-as.numeric(D[5])]
    
    if (is.list(expl)){
      nonfp.sep=nonfp$nonfp.sep
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
      for (p in expl$sep.coeff){
        if (is.na(nonfp.sep[1])||sum(p==nonfp.s)==0){
          if (is.null(categ)|| sum(p==categ["var",])==0){
            for (j in 1:nresp){
              FP.names=c(FP.names, paste("FP_",chartr(".","_",p),"_",resp.names[j],sep=""))
            }
          }else{
            if (is.na(categ["ref",which(p==categ["var",])])){
              categ.names=levels(indata[[p]])
              for (j in 1:nresp){
                for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                  FP.names=c(FP.names,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                }
              }
            }else{
              categ.names=levels(indata[[p]])
              refx=categ["ref",which(p==categ["var",])]
              categ.names=categ.names[-which(refx==categ.names)]
              for (j in 1:nresp){
                for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                  FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                }
              }
            }
          }
        }
      }
      for (p in expl$common.coeff){
        nonfp.common=nonfp$nonfp.common
        nonfp.c=nonfp.common
        for (i in 1:length(nonfp.c)){
          nonfp.c[i]=gsub("\\.*[[:digit:]]","",nonfp.c[i])
        }
        if (is.na(nonfp.common[1])||sum(p==nonfp.c)==0){
          if (is.null(categ)|| sum(p==categ["var",])==0){
            FP.names=c(FP.names, paste("FP_",chartr(".","_",p),sep=""))
          }else{
            if (is.na(categ["ref",which(p==categ["var",])])){
              categ.names=levels(indata[[p]])
              for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
              }
            }else{
              categ.names=levels(indata[[p]])
              refx=categ["ref",which(p==categ["var",])]
              categ.names=categ.names[-which(refx==categ.names)]
              for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
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
              FP.names=c(FP.names, paste("FP_",chartr(".","_",p),"_",resp.names[j],sep=""))
            }
          }else{
            if (is.na(categ["ref",which(p==categ["var",])])){
              categ.names=levels(indata[[p]])
              for (j in 1:nresp){
                for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                  FP.names=c(FP.names,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                }
              }
            }else{
              categ.names=levels(indata[[p]])
              refx=categ["ref",which(p==categ["var",])]
              categ.names=categ.names[-which(refx==categ.names)]
              for (j in 1:nresp){
                for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                  FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
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
        nonfp.sep=nonfp$nonfp.sep
        nonfp.s=nonfp.sep
        for (i in 1:length(resp)){
          nonfp.s=gsub(paste(".",resp[i],sep=""),"", nonfp.s)
        }
        nonfp.s=unique(nonfp.s)
        for (p in expl$sep.coeff){
          if (is.na(nonfp.sep[1])||sum(p==nonfp.s)==0){
            if (is.null(categ)|| sum(p==categ["var",])==0){
              for (j in 1:nresp){
                FP.names=c(FP.names, paste("FP_",chartr(".","_",p),"_",resp[j],sep=""))
              }
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (j in 1:nresp){
                  for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                    FP.names=c(FP.names,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                  }
                }
                
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (j in 1:nresp){
                  for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                    FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                  }
                }
              }
            }
          }
        }
        for (p in expl$common.coeff){
          nonfp.common=nonfp$nonfp.common
          nonfp.c=nonfp.common
          for (i in 1:length(nonfp.c)){
            nonfp.c[i]=gsub("\\.*[[:digit:]]","",nonfp.c[i])
          }
          if (is.na(nonfp.common[1])||sum(p==nonfp.c)==0){
            if (is.null(categ)|| sum(p==categ["var",])==0){
              FP.names=c(FP.names, paste("FP_",chartr(".","_",p),sep=""))
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                  FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
                }
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                  FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
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
                FP.names=c(FP.names, paste("FP_",chartr(".","_",p),"_",resp[j],sep=""))
              }
              
            }else{
              if (is.na(categ["ref",which(p==categ["var",])])){
                categ.names=levels(indata[[p]])
                for (j in 1:nresp){
                  for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                    FP.names=c(FP.names,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                  }
                }
              }else{
                categ.names=levels(indata[[p]])
                refx=categ["ref",which(p==categ["var",])]
                categ.names=categ.names[-which(refx==categ.names)]
                for (j in 1:nresp){
                  for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                    FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
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
            FP.names=c(FP.names, paste("FP_",chartr(".","_",p),sep=""))
          }else{
            if (is.na(categ["ref",which(p==categ["var",])])){
              categ.names=levels(indata[[p]])
              for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
              }
            }else{
              categ.names=levels(indata[[p]])
              refx=categ["ref",which(p==categ["var",])]
              categ.names=categ.names[-which(refx==categ.names)]
              for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                FP.names=c(FP.names, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
              }
            }
          }
        }
      }
    }
  }

  resid.names=function(rpx, resid.lev,RP){
    nrpx=length(rpx)
    for (j in 1: nrpx){
      for (i in 1:j){
        if (i==j){
          RP=c(RP, paste("RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),sep=""))
        }else{
          RP=c(RP, paste("RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),sep=""))
        }
      }
    }
    RP
  }
  
  resid2.names=function(rpx, resid.lev, clre,RP){
    nrpx=length(rpx)
    nclre=ncol(clre)
    k=1
    for (j in 1: nrpx){
      for (i in 1:j){
        if (!any(as.numeric(clre[1,])==resid.lev & ((clre[2,]==rpx[i] & clre[3,]==rpx[j]) | (clre[2,]==rpx[j] & clre[3,]==rpx[i])))) {
          if (i==j) {
            RP=c(RP, paste("RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),sep=""))
          }else{
            RP=c(RP, paste("RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),sep=""))
          }
        }
      }
    }
    RP
  }
  
  RP.names =NULL
  if (length(rp)>0){
    for (ii in 1:length(rp)){
      if (is.null(clre)){
        RP.names=resid.names(rp[[ii]],as.numeric(sub("rp","",names(rp)[ii])),RP.names)
      }else{
        RP.names=resid2.names(rp[[ii]],as.numeric(sub("rp","",names(rp)[ii])), clre,RP.names)
      }
    }
  }


  if (D[1]=='Multinomial') {
    RP.names=c(RP.names, "RP1_bcons_1")
    if (EstM == 0 && as.numeric(D[4])==0) {
      RP.names=c(RP.names, "RP1_bcons_2")
    }
  }


  levID0=levID
  #if (is.na(levID0[length(levID0)])){
  #  tmp.RP.names=gsub("RP","",RP.names)
  #  for (i in 1:length(RP.names)){
  #    tmpstrlist=unlist(strsplit(tmp.RP.names[i],"\\_"))
  #    tmpno=as.integer(tmpstrlist[1])-1
  #    RP.names[i]=paste0("RP",tmpno,"_",paste(tmpstrlist[-1],collapse="_"))
  #  }
  #}

  FP <- rep(0, length(FP.names))
  names(FP) <- FP.names

  FP.cov <- matrix(0,length(FP.names),length(FP.names))
  colnames(FP.cov) <- FP.names
  rownames(FP.cov) <- FP.names

  RP <- rep(0, length(RP.names))
  names(RP) <- RP.names

  RP.cov <- matrix(0,length(RP.names),length(RP.names))
  colnames(RP.cov) <- RP.names
  rownames(RP.cov) <- RP.names

  sval <- estoptions$startval
  if (EstM == 1) {
    if (!is.null(mcmcMeth$startval)) {
      warning("startval is now specified directly within estoptions")
      sval=mcmcMeth$startval
    }
  }

  if (!is.null(sval)) {
    if (!is.null(sval$FP.b) && is.null(names(sval$FP.b))) {
      names(sval$FP.b) <- FP.names
    }

    if (!is.null(sval$FP.v) && (is.null(rownames(sval$FP.v)) || is.null(colnames(sval$FP.v)))) {
      rownames(sval$FP.v) <- FP.names
      colnames(sval$FP.v) <- FP.names
    }

    if (!is.null(sval$RP.b) && is.null(names(sval$RP.b))) {
      names(sval$RP.b) <- RP.names
    }

    if (!is.null(sval$RP.v) && (is.null(rownames(sval$RP.v)) || is.null(colnames(sval$RP.v)))) {
      rownames(sval$RP.v) <- RP.names
      colnames(sval$RP.v) <- RP.names
    }

    sharedFP <- intersect(FP.names, names(sval$FP.b))
    if (!is.null(sval$FP.b) && !is.null(sharedFP)) {
      FP[sharedFP] <- sval$FP.b[sharedFP]
    }
    if (!is.null(sval$FP.v) && !is.null(sharedFP)) {
      FP.cov[sharedFP, sharedFP] <- sval$FP.v[sharedFP, sharedFP]
    }
    sharedRP <- intersect(RP.names, names(sval$RP.b))
    if (!is.null(sval$RP.b) && !is.null(sharedRP)) {
      RP[sharedRP] <- sval$RP.b[sharedRP]
    }
    if (!is.null(sval$RP.v) && !is.null(sharedRP)) {
      RP.cov[sharedRP, sharedRP] <- sval$RP.v[sharedRP, sharedRP]
    }
    startval <- list(FP.b=FP, FP.v=FP.cov, RP.b=RP, RP.v=RP.cov)
  } else {
    startval <- NULL
  }

  if (EstM == 1) {
    seed=mcmcMeth$seed
    if(is.null(seed)) seed=1
    iterations=mcmcMeth$iterations
    if(is.null(iterations)) iterations=5000
    burnin=mcmcMeth$burnin
    if(is.null(burnin)) burnin=500
    thinning=mcmcMeth$thinning
    if(is.null(thinning)) thinning=1
    priorParam=mcmcMeth$priorParam
    if(is.list(priorParam)) priorParam=prior2macro(priorParam, Formula, levID, D, indata)
    if(is.null(priorParam)) priorParam="default"
    scale =mcmcMeth$scale
    if(is.null(scale)) scale=5.8
    refresh=mcmcMeth$refresh
    if(is.null(refresh)) refresh=50
    fixM=mcmcMeth$fixM
    if(is.null(fixM)) {
      if(D[1]=="Poisson"||D[1]=="Multinomial"||D[1]=="Binomial"||D[1]=="Mixed"){
        fixM=2
      }else{
        fixM=1
      }
    }
    residM=mcmcMeth$residM
    if(is.null(residM)){
      if(D[1]=="Poisson"||D[1]=="Multinomial"||D[1]=="Binomial"||D[1]=="Mixed"){
        residM=2
      }else{
        residM=1
      }
    }
    Lev1VarM=mcmcMeth$Lev1VarM
    if(is.null(Lev1VarM)){
      if(D[1]=="Poisson"||D[1]=="Multinomial"||D[1]=="Binomial"||D[1]=="Mixed"){
        Lev1VarM=2
      }else{
        Lev1VarM=1
      }
    }
    OtherVarM=mcmcMeth$OtherVarM
    if(is.null(OtherVarM)) OtherVarM=1
    adaption=mcmcMeth$adaption
    if(is.null(adaption)) adaption=1
    priorcode=mcmcMeth$priorcode
    if (is.null(priorcode)) priorcode=1
    rate=mcmcMeth$rate
    if (is.null(rate)) rate=50
    tol=mcmcMeth$tol
    if (is.null(tol)) tol=10
    lclo=mcmcMeth$lclo
    if (is.null(lclo)) lclo =0
    nopause=mcmcMeth$nopause
    if (is.null(nopause)) nopause=F
    dami=mcmcMeth$dami
  }

  mcmcOptions=estoptions$mcmcOptions
  if (EstM == 0 && !is.null(merr)) {
    stop("MCMC options cannot be specified with (R)IGLS estimation")
  }

  if (EstM == 1) {
    if(!is.null(mcmcOptions)) {
      if (D[1]=='Multivariate Normal'){
        mcmcOptions2=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0),mcco=0)
        for (ii in names(mcmcOptions)) mcmcOptions2[[ii]]=mcmcOptions[[ii]]
        mcmcOptions=mcmcOptions2
      }else{
        mcmcOptions2=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0))
        for (ii in names(mcmcOptions)) mcmcOptions2[[ii]]=mcmcOptions[[ii]]
        mcmcOptions=mcmcOptions2
      }
    }else{
      if (D[1]=='Multivariate Normal'){
        mcmcOptions=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0),mcco=0)
      }else{
        mcmcOptions=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0))
      }
    }
    if (mcmcOptions$hcen > 0) {
      if (mcmcOptions$hcen < 2 || mcmcOptions$hcen > length(na.omit(levID))) {
        stop("Invalid level for hierarchical centring")
      }
    }
    if (is.matrix(mcmcOptions$paex)) {
      for (i in 1:nrow(mcmcOptions$paex)) {
        if (mcmcOptions$paex[i, 2] == 1) {
          pelev <- mcmcOptions$paex[i, 1]
          if (pelev < 2 || pelev > length(na.omit(levID))) {
            stop("Invalid level for parameter expansion")
          }
        }
      }
    } else {
      if (mcmcOptions$paex[2] == 1) {
        pelev <- mcmcOptions$paex[1]
        if (pelev < 2 || pelev > length(na.omit(levID))) {
          stop("Invalid level for parameter expansion")
        }
      }
    }
  }

  if (EstM == 0 && !is.null(BUGO)) {
    stop("BUGO requires MCMC estimation to be selected")
  }

  drop.data=estoptions$drop.data
  if (is.null(drop.data)) drop.data=T
  
  sort.force=estoptions$sort.force
  if (is.null(sort.force)) sort.force = FALSE
  
  sort.ignore=estoptions$sort.ignore
  if (is.null(sort.ignore)) sort.ignore = FALSE
  
  if (!is.null(centring)){
    for (p in names(centring)){
      if(as.integer(centring[[p]][1])==1){
        indata[[p]]=indata[[p]]-mean(indata[[p]])
      }
      if(as.integer(centring[[p]][1])==2){
        indata[[p]]=indata[[p]]-mean(indata[[p]][as.logical(indata[[centring[[p]][2]]])])
      }
      if(as.integer(centring[[p]][1])==3){
        indata[[p]]=indata[[p]]-as.integer(centring[[p]][2])
      }
    }
  }
  
  if (D[1] == "Binomial") {
    if (!all(indata[[resp]] >= 0 && indata[[resp]] <= 1)) {
      stop("All values for a binomial response must lie between zero and one")
    }
  }

  if (D[1] == "Poisson") {
    if (!all(indata[[resp]] >= 0) && all(as.integer(indata[[resp]]) == indata[[resp]])) {
      stop("All values for a Poisson response must be positive integers")
    }
  }

  if (D[1] == "Mixed") {
    mixlink <- NULL
    discreteresp <- NULL
    for (i in 2:length(D)) {
      if (D[[i]][1] == "Binomial") {
        if (!all(indata[[resp[i-1]]] >= 0 && indata[[resp[i-1]]] <= 1)) {
          stop("All values for a binomial response must lie between zero and one")
        }
        discreteresp <- union(discreteresp, D[[i]][1])
        mixlink <- union(mixlink, D[[i]][2])
      }

      if (D[[i]][1] == "Poisson") {
        if (!all(indata[[resp[i-1]]] >= 0) && all(as.integer(indata[[resp[i-1]]]) == indata[[resp]])) {
          stop("All values for a Poisson response must be positive integers")
        }
        discreteresp <- union(discreteresp, D[[i]][1])
        mixlink <- union(mixlink, D[[i]][2])
      }
    }
    if (length(discreteresp) > 1) {
      stop("Mixed response models cannot contain both Binomial and Poisson responses")
    }
    if (length(mixlink) > 1) {
      stop("Only one link type can be specified for mixed models")
    }
  }

  if (D[1] == "Multinomial") {
    if (length(unique(indata[[resp]])) < 2) {
      stop("Responses must have at least two categories for multinomial models")
    }
    if (!is.factor(indata[[resp]])) {
      indata[[resp]] <- as.factor(indata[[resp]])
    }
    if (is.na(D[5])) {
      stop("Invalid reference category")
    }
    if (D[4] == 1) { # Ordered multinomial
      if (D[5] != min(levels(indata[[resp]])) && D[5] != max(levels(indata[[resp]]))) {
        stop("Invalid reference category")
      }
    }
  }

  needsint <- FALSE
  if (is.list(expl)){
    if ("1" %in% expl$sep.coeff || "1" %in% nonfp$common.coeff) {
      needsint <- TRUE
      expl$sep.coeff[expl$sep.coeff == "1"] <- "Intercept"
      expl$common.coeff[expl$common.coeff == "1"] <- "Intercept"
    }
  } else { 
    if ("1" %in% expl) {
      needsint <- TRUE
      expl[expl == "1"] <- "Intercept"
    }
  }

  if (is.list(nonfp)){
    if ("1" %in% nonfp$nonfp.sep || "1" %in% nonfp$nonfp.common) {
      needsint <- TRUE
      nonfp$nonfp.sep[nonfp$nonfp.sep == "1"] <- "Intercept"
      nonfp$nonfp.common[nonfp$nonfp.common == "1"] <- "Intercept"
    }
  } else { 
    if ("1" %in% nonfp) {
      needsint <- TRUE
      nonfp[nonfp == "1"] <- "Intercept"
    }
  }

  for (ii in 1:length(rp)) {
    if ("1" %in% rp[[ii]]) {
      needsint <- TRUE
      rp[[ii]][rp[[ii]] == "1"] <- "Intercept"
    }
  }

  if (needsint == TRUE) {
    indata[["Intercept"]] <- rep(1, nrow(indata))
  }  

  if (drop.data) {
    outvars <- c(resp)
    
    # Multiple membership IDs if applicable
    if (!is.null(xclass)) {
      for (i in 1:length(as.numeric(xclass$class))) {
        lev <- as.numeric(xclass$class[i])
        num <- as.numeric(xclass$N1[i])
        weightcol <- xclass$weight[i]
        idcol <- xclass$id[i]
        if (is.null(idcol) || is.na(idcol)) {
          idcol <- rev(na.omit(levID))[lev]
        }
        idstart = which(colnames(indata) == idcol)
        idend = idstart+(num-1)
        outvars <- union(outvars, colnames(indata)[idstart:idend])
        
        if (!is.null(weightcol)) {
          weightstart = which(colnames(indata) == weightcol)
          weightend = weightstart+(num-1)
          outvars <- union(outvars, colnames(indata)[weightstart:weightend])
        }
      }
    }
    outvars <- union(outvars, na.omit(levID))
    if(is.list(expl)) {
      xvars <- c(expl$sep.coeff, expl$common.coeff)
    } else {
      xvars <- expl
    }
    interpos=grep("\\:",xvars)
    if (length(interpos) == 0) {
      outvars <- union(outvars, xvars)
    } else {
      explx <- xvars[-interpos]
      outvars <- c(outvars, explx)
      # This intersect is a bit of a hack, but avoids variables generated by MLwiN being referenced
      interx <- intersect(unlist(mapply(strsplit, expl[interpos], "\\:"), use.names=FALSE), colnames(indata))
      outvars <- union(outvars, interx)
    }
    
    # Denominators/Offsets if applicable
    if (D[1] == 'Binomial' || D[1] == 'Poisson' || D[1] == 'Multinomial' || D[1] == 'Negbinom') {
      outvars <- union(outvars, D[[3]])
    }
    if (D[1] == 'Mixed') {
      for (i in 2:length(D)) {
        if (D[[i]][1] == 'Binomial' || D[[i]][1] == 'Poisson')
          outvars <- union(outvars, D[[i]][[3]])
      }
    }
    
    # (R)IGLS Weights if applicable
    if (!is.null(weighting)) {
      for (w in weighting$weights) {
        if (!is.na(w)) {
          outvars <- union(outvars, w)
        }
      }
    }
    
    outdata <- indata[, outvars]
  } else {
    outdata <- indata
  }
  
  if (sort.ignore == FALSE) {
    # Don't enforce sorting on level-1 in cases where it isn't used
    if (D[1] == 'Normal' || D[1] == 'Binomial' || D[1] == 'Poisson' || D[1] == 'Negbinom') {
      outdata[["_sortindex"]] <- seq(1, nrow(outdata)) # replace with sequence to keep sorting stable
      l1id <- levID[length(levID)]
      levID[length(levID)] <- "_sortindex"
    }
    
    # Check/sort data as approriate
    if (sort.force == TRUE) {
      outdata <- outdata[do.call(order, outdata[na.omit(levID)]), ]
    } else {
      if (is.null(estoptions$xclass) && all(do.call(order, outdata[na.omit(levID)]) == seq(1, nrow(outdata))) == FALSE) {
        stop("The input data are not sorted according to the model hierarchy")
      }
    }
    
    # Restore original level ID and drop temporary variable
    if (D[1] == 'Normal' || D[1] == 'Binomial' || D[1] == 'Poisson' || D[1] == 'Negbinom') {
      levID[length(levID)] <- l1id
      outdata[["_sortindex"]] <- NULL
    }
  }

  hierarchy <- NULL
  shortID <- na.omit(rev(levID))
  if (length(shortID) > 1) {
    for (lev in length(shortID):2) {
      if (!is.null(xclass)) {
        groupsize <- by(outdata, outdata[,shortID[lev]], nrow)
      } else {
        groupsize <- na.omit(as.vector(by(outdata, outdata[,shortID[lev:length(shortID)]], nrow)))
      }
      groupinfo <- cbind(length(groupsize), min(groupsize), mean(groupsize), max(groupsize))
      colnames(groupinfo) <- c("N", "min", "mean", "max")
      rownames(groupinfo) <- shortID[lev]
      hierarchy <- rbind(hierarchy, groupinfo)
    }
  }

  if (!file.access(workdir)==0) dir.create(workdir)
  
  dtafile = gsub("\\", "/", tempfile("dtafile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
  macrofile = gsub("\\", "/", tempfile("macrofile_",tmpdir =workdir, fileext=".txt"), fixed=TRUE)
  
  IGLSfile = gsub("\\", "/", tempfile("IGLSfile_", tmpdir=workdir, fileext=".dta"), fixed=TRUE)
  if (EstM==1) {
    MCMCfile = gsub("\\", "/", tempfile("MCMCfile_", tmpdir=workdir, fileext=".dta"), fixed=TRUE)
    chainfile = gsub("\\", "/", tempfile("chainfile_", tmpdir=workdir, fileext=".dta"), fixed=TRUE)
    if (!is.null(dami)){
      MIfile=gsub("\\", "/", tempfile("MIfile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
    }else{
      dami=MIfile=NULL
    }
    if (!is.null(fact)){
      FACTchainfile=gsub("\\", "/", tempfile("factchainfile_",tmpdir =workdir,fileext=".dta"), fixed=TRUE)
    }else{
      FACTchainfile=NULL
    }
  }
  if (!is.null(BUGO)) {
    modelfile = gsub("\\", "/", tempfile("modelfile_", tmpdir=workdir, fileext=".txt"), fixed=TRUE)
    initfile = gsub("\\", "/", tempfile("initfile_", tmpdir=workdir,fileext=".txt"), fixed=TRUE)
    datafile = gsub("\\", "/", tempfile("datafile_", tmpdir=workdir,fileext=".txt"), fixed=TRUE)
    scriptfile = gsub("\\", "/", tempfile("scriptfile_", tmpdir=workdir,fileext=".txt"), fixed=TRUE)
    bugEst = gsub("\\", "/", tempfile("bugEst_", tmpdir=workdir, fileext=".txt"), fixed=TRUE)
  } else {
    modelfile=NULL
    initfile=NULL
    datafile=NULL
  }
  if (resi.store) resifile = gsub("\\", "/", tempfile("resifile_", tmpdir =workdir,fileext=".dta"), fixed=TRUE)
  if (!is.null(resi.store.levs)) resichains = gsub("\\", "/", tempfile("resichains_",tmpdir =workdir,fileext=".dta"), fixed=TRUE)

  if ((D[1]=="Multivariate Normal"||D[1]=="Mixed"||D[1]=="Multinomial") && !is.null(clre)) {
    clre[1,] <- as.numeric(clre[1,]) + 1
  }

  args <- paste0("/run ", "\"" , macrofile, "\"")
  if (!debugmode){
    args <- paste0("/nogui ", args)
  }
  
  write.dta(outdata, dtafile, version = 10)
  
  finalClean <- function(clean.files){
    if (clean.files){
      file.remove(dtafile)
      file.remove(macrofile)
      file.remove(IGLSfile)
      if (EstM==1 && is.null(BUGO)) file.remove(MCMCfile)
      if (EstM==1 && is.null(BUGO)) file.remove(chainfile)
      if (!is.null(BUGO))  file.remove(modelfile)
      if (resi.store&& is.null(BUGO)) file.remove(resifile)
      if (EstM==1 && is.null(BUGO)){
        if (!is.null(resi.store.levs)) file.remove(resichains)
        if (!is.null(fact)) file.remove(FACTchainfile)
        if (!is.null(dami)) file.remove(MIfile)
      }
    } 
  }
  if (EstM==0){
    MacroScript1(outdata, dtafile,resp, levID, expl, rp, D, nonlinear, categ,notation, nonfp, clre,Meth,extra,reset,rcon,fcon,maxiter,convtol,
                 BUGO,mem.init, optimat, weighting,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,
                 IGLSfile=IGLSfile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,debugmode=debugmode,startval=startval)
    iterations=estoptions$mcmcMeth$iterations
    if(is.null(iterations)) iterations=5000
    burnin=estoptions$mcmcMeth$burnin
    if(is.null(burnin)) burnin=500
    thinning=estoptions$mcmcMeth$thinning
    if(is.null(thinning)) thinning=1
    
    time1=proc.time()
    system2(cmd, args = args, stdout=stdout, stderr=stderr)
    cat("\n")
    time2=proc.time()-time1
    
    estIGLS <- read.dta(IGLSfile)

    FP[] <- na.omit(estIGLS[,1])    

    estIGLS2 <- na.omit(estIGLS[,2])
    k <- 1
    for (i in 1:length(FP)){
      for (j in 1:i){
        FP.cov[i,j] <- estIGLS2[k]
        FP.cov[j,i] <- FP.cov[i,j]
        k <- k+1
      }
    }

    RP[] <- na.omit(estIGLS[,3])

    estIGLS4 <- na.omit(estIGLS[,4])
    k <- 1
    for (i in 1:length(RP)){
      for (j in 1:i){
        RP.cov[i,j] <- estIGLS4[k]
        RP.cov[j,i] <- RP.cov[i,j]
        k <- k+1
      }
    }

    LIKE <- estIGLS[,dim(estIGLS)[2]][3]
    if (!is.na(LIKE)) {
      if (LIKE==1)
        LIKE <- NA
    }

    NTotal <- estIGLS[,dim(estIGLS)[2]][1]
    NUsed <- estIGLS[,dim(estIGLS)[2]][2]

    if (estIGLS[,dim(estIGLS)[2]][8] == 1) {
      Converged <- TRUE
    } else {
      Converged <- FALSE
    }

    Iterations <-  estIGLS[,dim(estIGLS)[2]][7]
  }
  
  # MCMC algorithm (using the starting values obtain from IGLS algorithm)
  if (EstM==1){
    MacroScript2(outdata, dtafile,resp, levID, expl, rp, D,nonlinear, categ,notation,nonfp,clre,Meth,merr,carcentre,maxiter,convtol,
                 seed,iterations,burnin,scale,thinning,priorParam,refresh,fixM,residM,Lev1VarM, 
                 OtherVarM,adaption,priorcode,rate, tol,lclo,mcmcOptions,fact,xclass,BUGO,mem.init,optimat,
                 nopause,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,IGLSfile=IGLSfile,MCMCfile=MCMCfile,
                 chainfile=chainfile,MIfile=MIfile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,
                 resichains=resichains,FACTchainfile=FACTchainfile,resi.store.levs=resi.store.levs,debugmode=debugmode,startval=startval, dami=dami)
    
    cat("MLwiN is running, please wait......\n")
    time1=proc.time()
    system2(cmd, args=args, stdout=stdout, stderr=stderr)
    cat("\n")
    time2=proc.time()-time1
    
    nlev=length(levID)
    if (is.null(BUGO)){
      estMCMC <- read.dta(MCMCfile)

      FP[] <- na.omit(estMCMC[,1])

      estMCMC2 <- na.omit(estMCMC[,2])
      k <- 1
      for (i in 1:length(FP)){
        for (j in 1:i){
          FP.cov[i,j] <- estMCMC2[k]
          FP.cov[j,i] <- FP.cov[i,j]
          k <- k+1
        }
      }

      RP[] <- na.omit(estMCMC[,3])

      estMCMC4 <- na.omit(estMCMC[,4])
      k <- 1
      for (i in 1:length(RP)){
        for (j in 1:i){
          RP.cov[i,j] <- estMCMC4[k]
          RP.cov[j,i] <- RP.cov[i,j]
          k <- k+1
        }
      }


      chains <- read.dta(chainfile)

      chains <- mcmc(data=chains[,-1], thin = thinning)       
      chain.names <- colnames(chains)
      chain.names[grep('RP',chain.names)] <- RP.names
      colnames(chains) <- chain.names

      ESS <- effectiveSize(chains)
      
      if (!(D[1]=="Mixed")&&is.null(merr)&&is.null(fact)){
        BDIC <- estMCMC[,dim(estMCMC)[2]][c(5,6,4,3)]
        BDIC.names <- c("Dbar", "D(thetabar)",  "pD", "DIC")
        names(BDIC) <- BDIC.names
      }else{
        LIKE <- estMCMC[,dim(estMCMC)[2]][3]
        if(!is.na(LIKE)){
          if(LIKE==1) 
            LIKE <- NA
        }
      }

      NTotal <- estMCMC[,dim(estMCMC)[2]][1]
      NUsed <- estMCMC[,dim(estMCMC)[2]][2]
      levID.display=""
      if (is.na(levID0[length(levID0)])){
        levID0=levID0[-length(levID0)]
      }
      for (i in 1:length(levID0)){
        levID.display=paste(levID.display,"Level ",length(levID0)+1-i,": ",levID0[i],"     ",sep="")
      }
      if (!is.null(fact)){
        loadings=na.omit(estMCMC[,5])
        load.names=rep(NA,length(loadings))
        k=1
        for (i in 1:fact$nfact){
          for (j in resp){
            load.names[k]=paste("load",i,"_",j,sep="")
            k=k+1
          }
        }
        loadings.sd=na.omit(estMCMC[,6])
        names(loadings) <- load.names
        names(loadings.sd) <- load.names
        
        fact.cov=fact.cov.names=na.omit(estMCMC[,7])
        fact.cov.sd=na.omit(estMCMC[,8])
        k=1
        for (i in 1:fact$nfact){
          for(j in 1:i){
            if(i==j){
              fact.cov.names[k]=paste("var_fact",i,sep="")
            }else{
              fact.cov.names[k]=paste("cov_fact",i,"_fact",j,sep="")
            }
            k=k+1
          }
        }
        names(fact.cov) <- fact.cov.names
        names(fact.cov.sd) <- fact.cov.names
        
        factchains=read.dta(FACTchainfile)
        factscores=matrix(na.omit(factchains[,"_FACT_value_b"]), ncol=fact$nfact, byrow=FALSE)
        factscores_v=matrix(na.omit(factchains[,"_FACT_value_v"]), ncol=fact$nfact, byrow=FALSE)
        factloads=matrix(na.omit(factchains[,"_FACT_load_b_chain"]), nrow=iterations/thinning, byrow=TRUE)
        factcovs=matrix(na.omit(factchains[,"_FACT_load_v_chain"]), nrow=iterations/thinning, byrow=TRUE)
        nameloads=NULL
        namefacts=NULL
        namefacts_v=NULL
        namecovs=NULL
        for (i in 1:fact$nfact){
          if (fact$lev.fact[i] > 1) {
            nunit <- nrow(unique(indata[rev(na.omit(levID))[fact$lev.fact[i]]]))
            if (length(factscores) > nunit) {
              factscores[(nunit+1):nrow(factscores),i] <- NA
            }
          }
          namefacts = c(namefacts, paste0("factorscores", i))
          namefacts_v = c(namefacts_v, paste0("factorscores_var", i))
        }
        colnames(factscores) = namefacts
        colnames(factscores_v) = namefacts_v
        colnames(factloads) = load.names
        colnames(factcovs) = fact.cov.names
        factChains = list(scores=factscores, scores_v=factscores_v, loadings=mcmc(data=factloads, thin = thinning), cov=mcmc(data=factcovs, thin = thinning))
      }
       
      if (sum(grepl("bcons",colnames(chains)))>0){
        bcons.pos=grep("bcons",colnames(chains))
        chains[1,bcons.pos]=chains[1,bcons.pos]-0.001
      }
      
      if (is.na(levID[length(levID)])){
        mlwinlev=(nlev-1):1
        levID2=levID0
      }else{
        mlwinlev=nlev:1
        levID2=levID
      }
    }
  }
  
  if (show.file) file.show(macrofile)
  if ((!is.null(BUGO))&&!(D[1]=="Mixed")){
    if (show.file) file.show(modelfile)
    n.iter=iterations+burnin
    addmore=NULL
    if(EstM==1){
      if(!is.null(xclass)&&length(xclass)==5){
        if(sum(xclass[[5]])>0){
          addmore=c(addmore,"carmean")
        }
      }	
      if(is.matrix(mcmcOptions$paex)){
        if(sum(mcmcOptions$paex[,2])>0){
          vx=sapply(2:nlev, function(x) paste("v",x,sep=""))
          sigma2v=sapply(2:nlev, function(x) paste("sigma2.v",x,sep=""))
          addmore=c(addmore,vx,sigma2v)
        }
      }else{
        if(is.vector(mcmcOptions$paex)&&length(mcmcOptions$paex)==2){
          if(mcmcOptions$paex[2]>0){
            vx=sapply(2:nlev, function(x) paste("v",x,sep=""))
            sigma2v=sapply(2:nlev, function(x) paste("sigma2.v",x,sep=""))
            addmore=c(addmore, vx,sigma2v)
          }
        }
      }
    }
    
    debug=as.logical(BUGO["debug"])
    if (is.na(debug)){
      debug=F
    }
    OpenBugs=as.logical(BUGO["OpenBugs"])
    if (is.na(OpenBugs)){
      OpenBugs=F
    }
    version=as.integer(BUGO["version"])
    if (is.na(version)){
      version=4
    }
    n.chains=as.integer(BUGO["n.chains"])
    if (is.na(n.chains)){
      n.chains=1
    }
    bugs.seed=BUGO["seed"]
    bugs=BUGO["bugs"]
    if (is.na(bugs.seed)){
      bugs.seed=NULL
    }
    if (is.na(bugs)){
      stop("Need to specify path to the BUGS executable.")
    }
    chains.bugs.mcmc=mlwin2bugs(D,levID, datafile, initfile, modelfile, bugEst, fact, addmore, n.chains = n.chains, n.iter = n.iter, n.burnin=burnin, n.thin=thinning, debug=debug, bugs=bugs,
                                bugsWorkingDir=workdir, OpenBugs = OpenBugs, cleanBugsWorkingDir=clean.files, seed = bugs.seed)
    time2=proc.time()-time1
  }else{
    if (D[1]=="Mixed"&&(!is.null(BUGO)))  warning("The Mixed response model is currently not implemented in WinBUGS/OpenBUGS.")
  }
  
  if (resi.store&& is.null(BUGO)){
    resiraw=read.dta(resifile)
    residelpos=grep("^[c]?[[:digit:]]+$", names(resiraw))
    if(length(residelpos)==0){
    }else{
      resisavename=names(resiraw)[-residelpos]
    }
  }
  
  if (EstM==1 && is.null(BUGO) && !is.null(resi.store.levs)){
    residata <- read.dta(resichains)
    resiChains <- list()
    for (name in colnames(residata)) {
      lev <- as.integer(gsub("resi_lev", "", name))
      nunit <- nrow(unique(indata[rev(levID)[lev]]))
      pnames <- NULL
      ucount = 0
      for (varname in rp[[paste0("rp", lev)]]) {
        pnames <- c(pnames, paste("u", ucount, seq(1:nunit), sep="_"))
        ucount = ucount +1
      }
      resiChains[[name]] <- mcmc(data=matrix(na.omit(residata[, name]), nrow=iterations/thinning, byrow=TRUE, dimnames=list(1:(iterations/thinning), pnames)), thin = thinning)
    }
  }
  
  if (EstM==0){
    if (is.null(BUGO)){
      outIGLS=new("mlwinfitIGLS")
      outIGLS["Nobs"]=NUsed
      outIGLS["DataLength"]=NTotal
      outIGLS["Hierarchy"]=hierarchy
      outIGLS["D"]=D
      outIGLS["Formula"]=Formula
      outIGLS["levID"]=levID
      outIGLS["FP"]=FP
      outIGLS["RP"]=RP
      outIGLS["FP.cov"]=FP.cov
      outIGLS["RP.cov"]=RP.cov
      outIGLS["LIKE"]=LIKE
      outIGLS["Meth"]=Meth
      outIGLS["Converged"]=Converged
      outIGLS["Iterations"]=Iterations
      outIGLS["elapsed.time"]=time2[3]
      outIGLS["call"]=cl
      
      if (resi.store){
        if(length(residelpos)==0){
          outIGLS["residual"]=resiraw
        }else{
          outIGLS["residual"]=resiraw[resisavename]
        }
      }
      
      finalClean(clean.files)
      return(outIGLS)
    }
    else{
      finalClean(clean.files)
      print(summary(chains.bugs.mcmc))
      return(chains.bugs.mcmc)
    }
  }
  if (EstM==1){
    if (is.null(BUGO)){
      outMCMC=new("mlwinfitMCMC")
      outMCMC["Nobs"]=NUsed
      outMCMC["DataLength"]=NTotal
      outMCMC["Hierarchy"]=hierarchy
      outMCMC["burnin"]=burnin
      outMCMC["iterations"]=iterations
      outMCMC["D"]=D
      outMCMC["Formula"]=Formula
      outMCMC["levID"]=levID
      outMCMC["merr"]=merr
      outMCMC["fact"]=fact
      outMCMC["xclass"]=xclass
      outMCMC["FP"]=FP
      outMCMC["RP"]=RP
      outMCMC["FP.cov"]=FP.cov
      outMCMC["RP.cov"]=RP.cov
      outMCMC["chains"]=chains
      outMCMC["elapsed.time"]=time2[3]
      outMCMC["call"]=cl
      if (!(D[1]=="Mixed")&&is.null(merr)&&is.null(fact)){
        outMCMC["BDIC"]=BDIC
      }else{
        outMCMC["LIKE"]=LIKE
      }
      if (!is.null(fact)){
        outMCMC["fact.loadings"]=loadings
        outMCMC["fact.loadings.sd"]=loadings.sd
        outMCMC["fact.cov"]=fact.cov
        outMCMC["fact.cov.sd"]=fact.cov.sd
        outMCMC["fact.chains"]=factChains
      }
      if (!is.null(resi.store.levs)){
        outMCMC["resi.chains"]=resiChains
      }
      if (!is.null(dami)){
        outMCMC["MIdata"]=read.dta(MIfile)
      }
      
      if (resi.store){
        if(length(residelpos)==0){
          outMCMC["residual"]=resiraw
        }else{
          outMCMC["residual"]=resiraw[resisavename]
        }
      }
      finalClean(clean.files)
      return(outMCMC)
    }
    else{
      finalClean(clean.files)
      print(summary(chains.bugs.mcmc))
      return(chains.bugs.mcmc)
    }
  }
}
