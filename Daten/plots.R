library(ggplot2)

geon <- readRDS("tidy_data.rds")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

p1 <- ggplot(aes(x="", y=barthel), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="Barthel-Index")
p2 <- ggplot(aes(x="", y=iadl), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="IADL")
p3 <- ggplot(aes(x="", y=tinetti), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="Tinetti-Test")
p4 <- ggplot(aes(x="", y=tug), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="TUG")
p5 <- ggplot(aes(x="", y=mmst), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="MMST")
p6 <- ggplot(aes(x="", y=DemTect), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="DemTect")
p7 <- ggplot(aes(x="", y=gds), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="GDS")
p8 <- ggplot(aes(x="", y=dia), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="DIA-S")
p9 <- ggplot(aes(x="", y=charlson), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="Charlson-Score")
p10 <- ggplot(aes(x="", y=mna), data=geon) + geom_boxplot() +
    coord_flip() + labs(x="", y="", title="MNA")

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=2)

