#' Save a ggplot to a A3 pdf file on disk
#'
#' @param plot 
#' @param save_plot 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
save_pdf_plot <- function(plot, file_name, save_plot = FALSE, logger = NULL) {
  stopifnot("ggplot" %in% class(plot))
  dir_name <- dirname(file_name)
  if (!dir.exists(dir_name)) { 
    # if directory does not exist create it
    if (!is.null(logger)) logger$log("Creating directory '%s'...", dir_name)
    if (!dir.create(dir_name)) {
      # if directory cannot be created, save pdf file in current directory
      if (!is.null(logger)) logger$log("Directory '%s' cannot be created, storing plot in './'...", dir_name)
      file_name <- basename(file_name)
      }
  }
  if (save_plot) {
    if (!grepl("\\.pdf$", file_name)) file_name <- paste0(file_name, ".pdf")
    if (!is.null(logger)) logger$log("Saving ggplot '%s'...", file_name)
    ggsave(filename = file_name, plot = plot, width = 420, height = 297, units = "mm")
  }
}

extract_random_coeff <- function(lmer_model) {

  f <- function(effect, random_effect_name) {
    pv   <- attr(effect, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(effect, order)) + rep((0:(ncol(effect) - 1)) * nrow(effect), each=nrow(effect))
    data.frame(y=unlist(effect)[ord],
               ci=1.96*se[ord],
               nQQ=rep(qnorm(ppoints(nrow(effect))), ncol(effect)),
               ID=factor(rep(rownames(effect), ncol(effect))[ord], levels=rownames(effect)[ord]),
               ind=gl(ncol(effect), nrow(effect), labels=names(effect)),
               random_effect = random_effect_name)
    
  }
  
  stopifnot(grepl("merMod$", class(lmer_model)))
  
  rand_effects <- ranef(lmer_model, condVar = TRUE)
  bind_rows(mapply(
    f, 
    rand_effects, 
    names(rand_effects), 
    SIMPLIFY = FALSE))
  
}

ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {

  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue") 
    p <- p + guides(size = "none")
    return(p)
  }
  
  lapply(re, f)
}


# plot templates ----------------------------------------------------------

to_vegetation_index_range <- function(x){
  x / 1e4
}
to_flux_range <- function(x){
  x / 1e3
}

plot_l <- list(
  raw_data_plot = list(
    geom_line(aes(x = date, y = smooth_EVI, color = vi_usefulness_level, group = pixel)),
    scale_y_continuous("EVI", label = to_vegetation_index_range),
    scale_colour_continuous(name = "Usefulness"),
    facet_grid(site_id~year),
    theme_light() +
      theme(axis.text.x = element_text(angle = 90))
  ),
  
  start_end_plot = list(
    aes(x = doy, y = smooth_vi, group = pixel),
    geom_point(aes(size = weight, color = vi_usefulness_level, shape = season_timing)),
    geom_line(),
    scale_shape_discrete("Timing of season", solid = TRUE),
    scale_color_gradient("Usefulness", low = "red", high = "green"),
    scale_size_continuous("Weight of timing"),
    facet_wrap(~Year),
    scale_y_continuous("Vegetation index", label = to_vegetation_index_range),
    scale_x_continuous("Day of year [0..365]"),
    theme_light(),
    theme(legend.position = "bottom", legend.box = "horizontal"),
    theme(axis.text.x = element_text(angle = -90)),
    ggtitle("Start/End of season for each year")
  ),
  
  green_season_plot = list(
    aes(x = Year, ymin = sos_doy, ymax = eos_doy, y = length_season),
    geom_ribbon(alpha = 1/10),
    geom_point(),
    geom_line(),
    scale_y_continuous(limits = c(1, 365)),
    facet_wrap(~pixel),
    theme_light()
  ),
  
  gpp_plot = list(
    #aes(x = Year, y = cumulative_vi, fill = mean_vi_usefulness_level),
    #geom_bar(stat = "identity"),
    aes(x = Year, y = cumulative_vi),
    geom_point(aes(color = mean_vi_usefulness_level, size = cumulative_vi_n)),
    geom_line(aes(group = pixel)),
    geom_smooth(method = "loess"),
    #facet_wrap(~pixel),
    scale_y_continuous("Proxy of Gross Pimary Productivity", label = to_vegetation_index_range),
    scale_color_continuous("Mean usefulness level"),
    scale_size_continuous("Count of observations"),
    #  scale_fill_continuous("Mean usefulness level"),
    theme_light(),
    theme(legend.position = "bottom", legend.box = "horizontal"),
    theme(axis.text.x = element_text(angle = -90)),
    ggtitle("Green season cumulative vegetation index")
  )
)
