library(rvest)    
library(stringr)
#' Extracts a table from a website using a CSS selector,
#' converts it to a dataframe, and returns it.
#'
#' @param url The URL to scrape.
#' @param css.selector The css.selector of the table to scrape.
#' @param fill.table Whether or not R should attempt to fill in missing data.
#'
#' @return The requested table as a dataframe.
web.table.to.dataframe = function(url, css.selector, fill.table=FALSE) {
	temp = url %>% read_html %>% html_nodes(css=css.selector) 
	return(as.data.frame(html_table(temp, fill=fill.table)))
}

#' Uses regular expressions to extract the numeric
#' portion from the provided percentage string.
#'
#' @param text The string from which to extract.
#' @return The number in the string, as a numeric.
percentage.to.numeric = function(text) {
	return(as.numeric(str_extract(text, pattern='([0-9]+.[0-9]+)')))
}

#' Wrapper for the function \code{qqplot} - generates quantile data
#' for a vector of values, and colors the plot.
#'
#' @param x The data to graph.
#' @param party The name of the party for the data.
#' @param xlabel A label for the x-axis.
#' @param color  The color to use in the graph.
quantile.plot = function(x, party, xlabel, color) {
	x.as.normal.distribution = (x - mean(x)) / sd(x)
	qq.plot.title = paste("Q-Q Plot of", party, xlabel)
	qqnorm(x.as.normal.distribution, main=qq.plot.title, col=color,  pch=19, datax=TRUE)
	abline(0, 1)
}


#' Plots a series of political data, along with an R^2 value for a 
#' least squares linear regression and an optional Q-Q Plot.
#'
#' @param x      X-axis data; defaults to the range 1:length(y) if omitted
#' @param y      Y-axis data
#' @param color  The color with which to plot the party data
#' @param party  The name of the data's political party 
#' @param ylabel The label for the graph's y-axis
#' @param xlabel The label for the graph's x-axis
#' @param plot.quantiles Whether or not to print a Q-Q Plot of the graph.
plot_series = function(x = NULL, y, color, party, ylabel, xlabel, plot.quantiles = FALSE) {
	title = paste(party, ylabel, "vs", xlabel)
	if (is.null(x)) {
		x = 1:length(y)
	} 
	least.squares = lm(y~x)
	r.squared = sprintf("%.4f", summary(least.squares)$adj.r.squared)
	plot(NULL, xlim=range(x), ylim=range(y), xlab=xlabel, ylab=ylabel, main=title)
	legend("right", bty="n", legend=paste("R^2 is", r.squared))
	points(x, y, col=color, pch=19)
	abline(least.squares, col=color)
	if (plot.quantiles) {
		quantile.plot(y, party, ylabel, color)
	}
}

vote_margin_url = 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# The first two rows of the table are useless, so we remove them
votes = web.table.to.dataframe(vote_margin_url, ".sortable", FALSE)[-1:-2,]

# Convert these two columns from percentages to a more useful form
votes$Margins = percentage.to.numeric(votes$Popular.vote...)
votes$Turnout = percentage.to.numeric(votes$Turnout)

democrats = subset(votes, votes$Party == "Dem.")
republicans = subset(votes, votes$Party == "Rep.")

# Start outputting plot
pdf("plots.pdf", width=8.5, height=11)
# mar sets the margins of the charts.
# For charts of this size, failure to manually set mar causes an error or cropping.
par(mfcol=c(3,2), oma=c(0,0,2,0), mar=c(4,4,3,2))

# "dodgerblue" is a light blue, chosen because it is vaguely Democratic colored and looks better than "blue"
plot_series(NULL, democrats$Margins, "dodgerblue", "Democratic", "Victory Margin %", "Victory Number", TRUE)
plot_series(democrats$Margins, democrats$Turnout, "dodgerblue", "Democratic", "Turnout %", "Victory Margin %")
plot_series(NULL, republicans$Margins, "red", "Republican", "Victory Margin %", "Victory Number", TRUE)
plot_series(republicans$Margins, republicans$Turnout, "red", "Republican", "Turnout %", "Victory Margin %")
title("Victory Margins in American Presidential Elections by Party and Turnout", outer=TRUE)

# Stop outputting plot
dev.off()


