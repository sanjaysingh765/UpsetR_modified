
#devtools::install_github("hms-dbmi/UpSetR")

# load libraries
library(UpSetR)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(extrafont)
#font_import() # only one time required when first time use the library extrafont
#y
fonts() 
loadfonts()

########################################  functions start ###############################################################

## Generate main bar plot
Make_main_bar <- function(Main_bar_data, Q, show_num, ratios, customQ, number_angles,
                          ebar, ylabel, ymax, scale_intersections, text_scale, attribute_plots){

  bottom_margin <- (-1)*0.65

  if(is.null(attribute_plots) == FALSE){
    bottom_margin <- (-1)*0.45
  }
  
  if(length(text_scale) > 1 && length(text_scale) <= 6){
    y_axis_title_scale <- text_scale[1]
    y_axis_tick_label_scale <- text_scale[2]
    intersection_size_number_scale <- text_scale[6]
  }
  else{
    y_axis_title_scale <- text_scale
    y_axis_tick_label_scale <- text_scale
    intersection_size_number_scale <- text_scale
  }
  
  if(is.null(Q) == F){
    inter_data <- Q
    if(nrow(inter_data) != 0){
      inter_data <- inter_data[order(inter_data$x), ]
    }
    else{inter_data <- NULL}
  }
  else{inter_data <- NULL}
  
  if(is.null(ebar) == F){
    elem_data <- ebar
    if(nrow(elem_data) != 0){
      elem_data <- elem_data[order(elem_data$x), ]
    }
    else{elem_data <- NULL}
  }
  else{elem_data <- NULL}
  
  #ten_perc creates appropriate space above highest bar so number doesnt get cut off
  if(is.null(ymax) == T){
  ten_perc <- ((max(Main_bar_data$freq)) * 0.1)
  ymax <- max(Main_bar_data$freq) + ten_perc 
  }
  
  Main_bar_plot <- (ggplot(data = Main_bar_data, aes_string(x = "x", y = "freq")) 
                    #+ scale_y_continuous(trans = scale_intersections)
                    #+scale_y_continuous(limits = c(0, 12000),breaks = seq(0, 12000, by = 2000))
                    + ylim(0, ymax)
                    + geom_bar(stat = "identity", width = 0.6,
                               fill = Main_bar_data$color)
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0),
                                         breaks = NULL)
                    + xlab(NULL) + ylab(ylabel) +labs(title = NULL)
                    + theme(
                      axis.text=element_text(size=6, color = "black",family="Times"),
                      axis.title=element_text(size=7,face="bold", color = "black"),
                      plot.title = element_text(color="black", size=5, face="bold.italic",hjust = 0.5,margin=margin(b = 5, unit = "pt")))
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin = unit(c(4,0.5,bottom_margin,0.5), "lines"), 
                            panel.border = element_blank(),
                            axis.title.y = element_text(vjust = -0.8, size = 8.3*y_axis_title_scale), 
                            axis.text.y = element_text(vjust=0.3,size=7*y_axis_tick_label_scale, color = "black",family="Times")))
  
  #
  
  if((show_num == "yes") || (show_num == "Yes")){
    Main_bar_plot <- (Main_bar_plot + geom_text(aes_string(label = "freq"), size = 2.2*intersection_size_number_scale, vjust = -1,
                                                angle = number_angles, colour = "black"))
  }
  bInterDat <- NULL
  pInterDat <- NULL
  bCustomDat <- NULL
  pCustomDat <- NULL
  bElemDat <- NULL
  pElemDat <- NULL
  if(is.null(elem_data) == F){
    bElemDat <- elem_data[which(elem_data$act == T), ]
    bElemDat <- bElemDat[order(bElemDat$x), ]
    pElemDat <- elem_data[which(elem_data$act == F), ]
  }
  if(is.null(inter_data) == F){
    bInterDat <- inter_data[which(inter_data$act == T), ]
    bInterDat <- bInterDat[order(bInterDat$x), ]
    pInterDat <- inter_data[which(inter_data$act == F), ]
  }
  if(length(customQ) != 0){
    pCustomDat <- customQ[which(customQ$act == F), ]
    bCustomDat <- customQ[which(customQ$act == T), ]
    bCustomDat <- bCustomDat[order(bCustomDat$x), ]
  }
  if(length(bInterDat) != 0){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = bInterDat,
                                              aes_string(x="x", y = "freq"),
                                              fill = bInterDat$color,
                                              stat = "identity", position = "identity", width = 0.6)
  }
  if(length(bElemDat) != 0){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = bElemDat,
                                              aes_string(x="x", y = "freq"),
                                              fill = bElemDat$color,
                                              stat = "identity", position = "identity", width = 0.6)
  }
  if(length(bCustomDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_bar(data = bCustomDat, aes_string(x="x", y = "freq2"),
                                               fill = bCustomDat$color2,
                                               stat = "identity", position ="identity", width = 0.6))
  }
  if(length(pCustomDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pCustomDat, aes_string(x="x", y = "freq2"), colour = pCustomDat$color2,
                                                 size = 2, shape = 17, position = position_jitter(width = 0.2, height = 0.2)))
  }
  if(length(pInterDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pInterDat, aes_string(x="x", y = "freq"),
                                                 position = position_jitter(width = 0.2, height = 0.2),
                                                 colour = pInterDat$color, size = 2, shape = 17))
  }
  if(length(pElemDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pElemDat, aes_string(x="x", y = "freq"),
                                                 position = position_jitter(width = 0.2, height = 0.2),
                                                 colour = pElemDat$color, size = 2, shape = 17))
  }
  
  Main_bar_plot <- (Main_bar_plot 
                    + geom_vline(xintercept = 0, color = "black")
                    + geom_hline(yintercept = 0, color = "black"))
  
  Main_bar_plot <- ggplotGrob(Main_bar_plot)
  return(Main_bar_plot)
}




## Generate set size plot
Make_size_plot <- function(Set_size_data, sbar_color, ratios, ylabel, scale_sets, text_scale, set_size_angle, set_size.show, set_size.scale_max,
                           set_size.number_size){
  #   if(ratios[1] < 0.4){
  #     m <- (-0.05)
  #   }
  #   else if((ratios[1] > 0.4) & (ratios[1] < 0.46)){
  #     m <- (-0.03)
  #   }
  #   else{
  #     m <- 0
  #   }
  
  if(length(text_scale) > 1 && length(text_scale) <= 6){
    x_axis_title_scale <- text_scale[3]
    x_axis_tick_label_scale <- text_scale[4]
  }
  else{
    x_axis_title_scale <- text_scale
    x_axis_tick_label_scale <- text_scale
  }
  
  if(ylabel == "Set Size" && scale_sets != "identity"){
    ylabel <- paste("Set Size", paste0("( ", scale_sets, " )"))
    if(scale_sets == "log2"){
      Set_size_data$y <- log2(Set_size_data$y)
    }
    if(scale_sets == "log10"){
      Set_size_data$y <- log10(Set_size_data$y)
    }
  }
  
  if(!is.null(set_size.number_size)) {
    num.size <- (set_size.number_size/ggplot2:::.pt)*x_axis_tick_label_scale
  } else {
    num.size <- (7/ggplot2:::.pt)*x_axis_tick_label_scale
  }
  #ten_perc creates appropriate space above highest bar so number doesnt get cut off
  #if(is.null(ymax) == T){
   # ten_perc <- ((max(Main_bar_data$freq)) * 0.1)
   # ymax <- max(Main_bar_data$freq) + ten_perc 
 # }
  Size_plot <- (ggplot(data = Set_size_data, aes_string(x ="x", y = "y"))
                + geom_bar(stat = "identity",colour = sbar_color, width = 0.4,
                           fill = sbar_color, position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)),
                                     expand = c(0,0))
                #+ scale_y_continuous(limits = c(0, 12000),
                 #                      breaks = c(0, 12000),
               #                        expand = c(0,0))
                #+ ylim(0, ymax)
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(-0.11,-1.3,0.5,15), "lines"),
                        axis.title.x = element_text(size = 8.3*x_axis_title_scale,color = "black",family="Times"),
                        axis.text.x = element_text(size = 7*x_axis_tick_label_scale, angle = set_size_angle,
                                                   vjust = 1, hjust = 0.5,color = "black",family="Times"),
                        axis.line = element_line(colour = "black"),
                        axis.line.y = element_blank(),
                        axis.line.x = element_line(colour = "black", size = 0.3),
                        #axis.text.y = element_text(color = "black",family="Times"),
                        #axis.ticks.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
                + xlab(NULL) + ylab(ylabel)
                + coord_flip())
  
  if(set_size.show == TRUE){
    Size_plot <- (Size_plot + geom_text(aes(label=y,vjust=0.5,hjust=0.1),size=(7/ggplot2:::.pt)*x_axis_tick_label_scale))
  }
  
   else{
    Size_plot <- (Size_plot + scale_y_continuous(trans = "reverse"))
  }
  
  Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
  
  return(Size_plot)

}

########################################  functions end ###############################################################

# Replace Create_layout in UpSetR with the modified function
assignInNamespace(x="Make_main_bar", value=Make_main_bar, ns="UpSetR")
assignInNamespace(x="Make_size_plot", value=Make_size_plot, ns="UpSetR")


#load data
gene_list <- read.csv("list")
head(gene_list)



#calculate possible comparision pairs
nsets = length(gene_list)
nsets

cond <- colnames(gene_list)
cond
m <- length(cond)
m
combine <- function (x, y) 
  combn (y, x, paste, collapse = "")
res1 <- unlist (lapply (0:nsets, combine, cond))
res1
n <- length(res1)-1
n


# color calculation
set.seed(1234)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector
cols_n <- col_vector[1:n]
cols_m <- tail(col_vector, m)




#create plot
png("gene_intersections.png", units="in", family="Times New Roman",  width=10, height=6, res=300, pointsize = 2)
#pdf("file.pdf",width=20,height=8, onefile=FALSE)  
upset(
  fromList(gene_list),
  nsets = length(gene_list), # number of sets
  nintersects = NA, #Number of intersections to plot. If set to NA, all intersections will be plotted.
  #sets =    #Specific sets to look at (Include as combinations. Ex: c("Name1", "Name2"
  #keep.order  # TRUE or FALSE
  order.by = "freq",   #Order tbars by size ("freq"`) or by the number of sets that contributed to the intersection ("degree"`) or by both:  order.by = c("freq", "degree")
  matrix.color = "gray23", #Color of the intersection points
  #sets.bar.color = "#56B4E9",
  sets.bar.color = cols_m,    #c("red","yellow","blue","green"),
  #main.bar.color = "#ea1951", #Color of the main bar plot
  main.bar.color = cols_n,#Color of the main bar plot #rainbow(n)
  empty.intersections = "on",
  mb.ratio = c(0.7, 0.3), #ratio between matrix plot and main bar plot
  point.size = 3.5, 
  line.size = 1, 
  mainbar.y.label = "Set Intersections", 
  sets.x.label = "Sets of interest",
  mainbar.y.max = 12000,  # The maximum y value of the intersection size bar plot scale. 
  #show.numbers = TRUE, # Show numbers of intersection sizes above bars
  number.angles = 360,#The angle of the numbers atop the intersection size bars
  #group.by # How the data should be grouped ("degree" or "sets")
  #shade.color =c("#ea1951","#56B4E9"), #Color of row shading in matrix
  shade.alpha = 0.5,   #Transparency of shading in matrix
  text.scale = c(2,1.5,2,1.5,2,2)#format:  c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
 
  )  
 
dev.off()
