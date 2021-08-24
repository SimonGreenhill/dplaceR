#' Loads trees from DPLACE
#'
#' @param type the type of tree to load (`summary` or `posterior`)
#' @param mappingfile a filename to remap tip names to (default=taxa.csv).
#' @param renameto column in mappingfile to rename tips too.
#'    If NA, then no remapping is done.
#'    If a tip has no mapping in the mappingfile::renameto column it will be removed
#' @return An ape::multiPhylo object
#' @export
#' @examples
#' trees <- load_trees("kolipakam_et_al2018")
load_trees <- function(dirname, type='summary', mappingfile='taxa.csv', renameto=NA) {
    # check file type
    if (type == 'summary') {
        treefile <- file.path(dirname, 'summary.trees')
    }
    else if (type == 'posterior') {
        treefile <- file.path(dirname, 'posterior.trees')
    } else {
        stop(paste("Unknown Tree Type:", type))
    }

    # check file exists
    if (file.exists(treefile) == FALSE) {
        stop(paste("Invalid file:", treefile))
    }

    trees <- ape::read.nexus(treefile)
    if (class(trees) == 'phylo') { trees <- c(trees) ; class(trees) <- 'multiPhylo' }

    # make full path if just given taxa.csv
    if (mappingfile == 'taxa.csv') { mappingfile <- file.path(dirname, mappingfile) }

    if (file.exists(mappingfile) & is.na(renameto) == FALSE) {
        mapping <- read.csv(mappingfile, header = TRUE, stringsAsFactors = FALSE, na.string="")

        # check the required columns exist
        if ('taxon' %in% colnames(mapping) == FALSE) stop(paste('column `taxon` not in', mappingfile))

        if (renameto %in% colnames(mapping) == FALSE) stop(paste('colname', renameto, 'not in', mappingfile))
        trees <- ape::.uncompressTipLabel(trees)
        for (i in 1:length(trees)){
            # remove tips not in mapping
            missing <- mapping[is.na(mapping[[renameto]]), 'taxon']
            if (length(missing) > 0) {
                trees[[i]] <- ape::drop.tip(trees[[i]], missing)
            }
            # todo: handle duplicates in renameto?
            # handle duplicate glottocodes
            #dupes <- mapping[duplicated(mapping$glottocode), ]
            #mapping <- mapping[!duplicated(mapping$glottocode), ]
            #mapping <- rbind(mapping, data.frame(taxon=dupes$taxon, glottocode=dupes$taxon))

            # rename tips
            matches <- match(trees[[i]]$tip.label, mapping[['taxon']])
            trees[[i]]$tip.label <- mapping[matches, renameto]
        }
        trees <- ape::.compressTipLabel(trees, ref=mapping[matches, renameto])
    }
    trees
}
