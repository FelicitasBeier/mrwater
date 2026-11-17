#' @title       toolRiverDischargeAllocation
#' @description This tool function allocates discharge for a prepared
#'              selected-cell subset respecting upstream-downstream
#'              relationships and various water constraints.
#'
#' @param iteration      Currently active iteration of river discharge allocation.
#'                       Arguments:
#'                       "main" for case of main river cells
#'                       "neighbor" for case of neighboring cells of main river cells
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param c              Current cell for which water shall be allocated
#' @param downCells      Downstream cells of c, as global cell IDs unless
#'                       localDownCells is provided
#' @param rs             River structure with information on upstreamcells,
#'                       downstreamcells and neighboring cells and distances
#' @param inLIST         List of read-only inputs: current water requests
#'                       (currReqWW, currReqWC), inaccessible discharge (inaccD)
#'                       and precomputed selected-cell positions (allocationCells)
#' @param inoutLIST      List of vectors that are read by this tool and returned
#'                       after updates (discharge, prevReservedWW, prevReservedWC,
#'                       protInaccD)
#'
#' @return list of updated discharge, updated previous withdrawal and consumption
#'         reservation, and fulfilled local and neighbor water withdrawals and consumption
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolRiverDischargeAllocation <- function(rs, c,
                                         downCells,
                                         iteration, transDist,
                                         inLIST, inoutLIST) {
  # Check that all required inputs are provided
  reqIn    <- c("currReqWW", "currReqWC", "inaccD", "allocationCells")
  reqInout <- c("discharge", "prevReservedWW", "prevReservedWC", "protInaccD")
  missingInputs <- c(reqIn[vapply(reqIn, function(x) is.null(inLIST[[x]]), logical(1))],
                     reqInout[vapply(reqInout, function(x) is.null(inoutLIST[[x]]), logical(1))])
  if (length(missingInputs) > 0) {
    stop("Inputs missing in toolRiverDischargeAllocation: ", paste(missingInputs, collapse = ", "))
  }

  # Inputs
  currReqWW <- inLIST$currReqWW
  currReqWC <- inLIST$currReqWC
  # Inaccessible discharge (not usable for new withdrawals in the respective cell)
  inaccD    <- inLIST$inaccD

  # Inputs that are also outputs, i.e. object that are updated by this function
  discharge      <- inoutLIST$discharge[drop = FALSE]
  prevReservedWW <- inoutLIST$prevReservedWW[drop = FALSE]
  prevReservedWC <- inoutLIST$prevReservedWC[drop = FALSE]
  # Inaccessible discharge of cells with new withdrawals (protected from upstream consumption)
  protInaccD     <- inoutLIST$protInaccD[drop = FALSE]

  # Precomputed selected-cell positions
  allocationCells <- inLIST$allocationCells
  cells           <- allocationCells$cells
  localCell       <- allocationCells$localCell
  localDownCells  <- allocationCells$localDownCells
  neighborCells   <- allocationCells$neighborCells
  neighborCell    <- allocationCells$neighborCell
  neighborSelectedCells <- allocationCells$neighborSelectedCells

  # Global cell IDs represented by the local discharge vector. Keeping this
  # mapping allows position-based indexing below instead of repeated name lookup.
  if (is.null(cells)) {
    # Fallback for direct calls that do not pass the cell mapping.
    if (length(discharge) == 1) {
      cells <- c
    } else {
      cells <- match(names(discharge), rs$isoCoord)
      if (anyNA(cells)) {
        stop("Could not map discharge names to river structure cells")
      }
    }
  }

  # Selected cells
  # Convert global cell IDs to local vector positions in discharge/prevReservedWW.
  cell <- localCell
  if (is.null(cell)) {
    cell <- match(c, cells)
  }
  if (is.na(cell)) {
    stop("Current cell is not part of selected discharge cells")
  }
  if (!is.null(localDownCells)) {
    downCells <- localDownCells
  } else if (length(downCells) > 0) {
    downCells <- match(downCells, cells)
    if (anyNA(downCells)) {
      stop("Downstream cells are not part of selected discharge cells")
    }
  }
  allCells <- c(cell, downCells)

  ##########################
  ###  Water Allocation  ###
  ##########################
  # Local water availability
  # (Note: discharge is the outflow after local consumption. Local inflow is
  #        discharge + prevReservedWC. It is compared to previously reserved
  #        withdrawals, which include the reserved consumption.
  #        Inaccessible discharge of the cell is not available for new withdrawals.)
  avlWatWW <- max(discharge[cell] + prevReservedWC[cell] - prevReservedWW[cell] - inaccD[cell], 0)

  # Is water required for withdrawal in current grid cell?
  if (currReqWW > 0 && avlWatWW > 0) {
    ### Withdrawal Constraint:
    # Only as much can be withdrawn locally as is available
    fracFulfilled <- min(avlWatWW / currReqWW, 1)

    ### Consumption Constraint:
    # Cannot consume water locally that is required further downstream
    if (currReqWC > 0 && length(downCells) > 0) {

      # Water availability (considering downstream availability)
      # (inaccessible discharge protected only in cells with new withdrawals)
      avlWatWC <- max(min(discharge[downCells] + prevReservedWC[downCells] -
                            prevReservedWW[downCells] - protInaccD[downCells]), 0)
      # Update fraction of irrigation water requirements that can be fulfilled
      fracFulfilled <- min(avlWatWC / currReqWC, fracFulfilled)
    }
  } else {
    # If no water requested: fracFulfilled not relevant
    fracFulfilled <- 0
  }

  # store locally fulfilled water withdrawals and consumption
  currWClocal <- currReqWC * fracFulfilled
  currWWlocal <- currReqWW * fracFulfilled

  # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
  discharge[allCells]  <- discharge[allCells] - currWClocal

  # update minimum water required in cell:
  prevReservedWW[cell] <- prevReservedWW[cell] + currWWlocal
  prevReservedWC[cell] <- prevReservedWC[cell] + currWClocal
  # protect inaccessible discharge of cell with new withdrawal from upstream consumption
  if (currWWlocal > 0) {
    protInaccD[cell] <- inaccD[cell]
  }


  # Initialize objects
  # to be filled (in case of main iteration)
  # remain zero (in case of neighbor iteration)
  fromNeighborWC <- fromNeighborWW <- 0

  # The following calculations are only relevant
  # if the current cell c is a cell of the main river,
  # i.e. water can be requested from neighboring cells
  if (iteration == "main") {

    # Locally missing water that might be fulfilled by surrounding cells
    missingWW <- currReqWW - currWWlocal
    missingWC <- currReqWC - currWClocal

    neighborsOfC <- neighborCells
    if (is.null(neighborsOfC)) {
      neighborsOfC <- rs$neighborcell[[c]]
    }
    # Neighbor Water Provision
    if ((transDist != 0) &&
          !is.null(neighborsOfC) &&
          length(neighborsOfC) > 0 &&
          (missingWW > 1e-4 || missingWC > 1e-4)) {
      # Loop over neighbor cells (by distance) until water requirements fulfilled
      for (neighborIndex in seq_along(neighborsOfC)) {
        n <- neighborsOfC[neighborIndex]

        # If withdrawal constraint not fulfilled in neighbor cell:
        # jump directly to next neighbor
        if (!is.null(neighborCell)) {
          neighborPosition <- neighborCell[neighborIndex]
        } else {
          neighborPosition <- match(n, cells)
        }
        if (is.na(neighborPosition)) {
          stop("Neighbor cell is not part of selected discharge cells")
        }
        avlWatWWNeighbor <- discharge[neighborPosition] + prevReservedWC[neighborPosition] -
          prevReservedWW[neighborPosition] - inaccD[neighborPosition]
        if (avlWatWWNeighbor <= 0) {
          next
        }
        # Select relevant cells
        if (!is.null(neighborSelectedCells)) {
          selectedCells <- neighborSelectedCells[[neighborIndex]]
        } else {
          selectCells <- c(n, rs$downstreamcells[[n]])
          selectedCells <- match(selectCells, cells)
        }
        if (anyNA(selectedCells)) {
          stop("Neighbor downstream cells are not part of selected discharge cells")
        }
        # Allocation from selected neighbor cell
        if (missingWW > 0 && avlWatWWNeighbor > 0) {
          fracNeighbor <- min(avlWatWWNeighbor / missingWW, 1)

          if (missingWC > 0 && length(selectedCells) > 1) {
            downstreamNeighborCells <- selectedCells[-1]
            avlWatWC <- max(min(discharge[downstreamNeighborCells] +
                                  prevReservedWC[downstreamNeighborCells] -
                                  prevReservedWW[downstreamNeighborCells] -
                                  protInaccD[downstreamNeighborCells]), 0)
            fracNeighbor <- min(avlWatWC / missingWC, fracNeighbor)
          }
        } else {
          fracNeighbor <- 0
        }

        neighborWClocal <- missingWC * fracNeighbor
        neighborWWlocal <- missingWW * fracNeighbor

        discharge[selectedCells] <- discharge[selectedCells] - neighborWClocal
        prevReservedWW[neighborPosition] <- prevReservedWW[neighborPosition] + neighborWWlocal
        prevReservedWC[neighborPosition] <- prevReservedWC[neighborPosition] + neighborWClocal
        if (neighborWWlocal > 0) {
          protInaccD[neighborPosition] <- inaccD[neighborPosition]
        }

        # update reserved water in respective neighboring cell (current cell)
        fromNeighborWW <- fromNeighborWW + neighborWWlocal
        fromNeighborWC <- fromNeighborWC + neighborWClocal

        # Update locally missing water in c
        missingWW <- missingWW - neighborWWlocal
        missingWC <- missingWC - neighborWClocal

        # Checks
        if (round(missingWW, digits = 4) < 0) {
          stop(paste0("More water than necessary provided ",
                      "in toolRiverDischargeAllocation by neighborcell ", n,
                      "to main cell ", c))
        }
        # Exit Neighbor Water Provision when enough water provided
        if (missingWW <= 1e-4 && missingWC <= 1e-4) {
          break
        }
      }
    }
  }

  out <- list(discharge = discharge,
              prevReservedWW = prevReservedWW,
              prevReservedWC = prevReservedWC,
              protInaccD = protInaccD,
              fromNeighborWC = fromNeighborWC,
              fromNeighborWW = fromNeighborWW,
              currWWlocal = currWWlocal,
              currWClocal = currWClocal)
  return(out)
}
