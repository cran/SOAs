#' Function to do level permutations according to Weng's algorithm
#'
#' Takes a workhorse function and creates random one- or two-neighbors
#'
#' @param funname function that creates the individual (O)SOAs
#' @param mperm number of columns of \code{startperm}
#' @param r number of rows of \code{startperm}
#' @param ... arguments for function \code{funname}
#' @param startperm matrix with position numbers of level permutations (refers to \code{allpermlist})
#' @param allpermlist list of all permutations
#' @param neighbordist 1 or 2: one- or two-neighbors in Weng's algorithm
#'
#' @return list of arrays and corresponding permutations
#'
#' @keywords internal
NeighbourcalcUniversal <- function(funname, mperm, r, ...,
                                   startperm=NULL, allpermlist=NULL,
                                   neighbordist=1){
  ### functions implemented for funname
  ## SOAs calls function soa
  ##     with the arguments oa (must be symmetric strength at least t) and t
  ##     m = m(ncol(oa),t), r=t
  ## OSOAs calls function OSOAarbitrary with the only mandatory arguments oa and optionally el, m
  ## (oa symmetric at least strength 2, result may be strength 3)
  ##     m = ncol(oa) (el=2) or 2*floor(ncol(oa)/2 (el=3)
  ##     or specified differently,
  ##     r=2
  ## SOAs2plus_regular calls function SOA2plus_regular_fast with arguments s, A, B
  ##     m = ncol(A), r=2
  ## OSOAs_LiuLiu calls function OSOA_LiuLiut with arguments oa and optionally m and t
  ##     m = ncol(oa) (slightly wasteful), r=1
  ##
  ##  ... thus the above either contain oa or s;
  ##      in the former case, s is calculated from oa
  ##
  ## SOAs8level calls function createDfromABC with argument listABC
  ##  ... now does not contain any from oa or s, but listABC[[1]] can be treated like oa
  ##
  ## ## no longer here: MDLEs (now separate function NeighbourcalcUniversal_random)
  ##
  ## ... arguments must be named in order to be found

  ## funname is the name of the function that calculates the array
  ##    (i.e. soa, OSOAarbitrary or OSOAregulart, not quoted)
  ## ... are the named arguments to be handed to that function
  ##               (problems may occur if those names permit confusion
  ##                by having the same start sequence)
  ## startperm is an rxm matrix of positions in permutations list
  ## returned by combinat::permn(s), or NULL

  funargs <- match.call(expand.dots=FALSE)$`...`
  if ("s" %in% names(funargs)) s <- eval(funargs$s, parent.frame()) else{
    stopifnot("oa" %in% names(funargs) || "listABC" %in% names(funargs))
    if ("listABC" %in% names(funargs)) oa <- eval(funargs$listABC, parent.frame())[[1]] else
    oa <- eval(funargs$oa, parent.frame())
    s <- levels.no(oa)[1]
  }

  m <- mperm

  stopifnot(is.function(funname))
  stopifnot(neighbordist %in% c(1,2))

  if (is.null(allpermlist))
    allpermlist <- lapply(combinat::permn(s), function(obj) obj-1)
  nallperms <- length(allpermlist)

  if (!is.null(startperm)) {
    stopifnot(is.matrix(startperm))
    if (any(startperm > nallperms | startperm < 1))
      stop("invalid entries in startperm")
  }
  ## starting list of permutations
  if (!is.null(startperm)) permpickstart <- startperm else
    permpickstart <- matrix(sapply(1:m,
                            function(obj) sample(1:nallperms,r,replace=TRUE)),
                            nrow=r,ncol=m)
  permpickneighbour1 <- NA*permpickstart
  for (i in 1:m)
    for (j in 1:r)
      permpickneighbour1[j,i] <- sample(setdiff(1:nallperms,
                                                permpickstart[j,i]), 1 )
  permlist <- lapply(1:m, function(obj)
    allpermlist[permpickstart[,obj]])
  returnlist <- vector(mode="list")
  docpermlist <- vector(mode="list")
  returnlist[[1]] <- funname(..., permlist)
  docpermlist[[1]] <- permpickstart
  zaehl <- 1
  ## picking distance one neighbors
  if (neighbordist==1){
    for (i in 1:m)
      for (j in 1:r){
        zaehl <- zaehl + 1
        hilf <- permpickstart
        hilf[j,i] <- permpickneighbour1[j,i]
        permlist <- lapply(1:m, function(obj) allpermlist[hilf[,obj]])
        returnlist[[zaehl]] <- funname(..., permlist)
        docpermlist[[zaehl]] <- hilf
      }
  }
  else{
    paare <- nchoosek(r*m, 2)
    for (i in 1:ncol(paare)){
      zaehl <- zaehl + 1
      hilf <- permpickstart
      hilf[paare[1,i]] <- permpickneighbour1[paare[1,i]]
      hilf[paare[2,i]] <- permpickneighbour1[paare[2,i]]
      permlist <- lapply(1:m, function(obj) allpermlist[hilf[,obj]])
      returnlist[[zaehl]] <- funname(..., permlist)
      docpermlist[[zaehl]] <- hilf
    }
  }
  return(list(arrays=returnlist, docpermlist=docpermlist))
}
