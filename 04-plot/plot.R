library(plotly)

# set up colors for the different caucuses
colorset <- c("lightpink2", "darkturquoise", "cornsilk", "red")
colorset <- setNames(colorset, c("建制", "泛民", "獨立或其他", "政府"))



####################
# 1 ABSENT RECORDS #
####################

absence <- read.csv("../03-analysis/legco-201610-202006-absences.csv")

# all

fig_absentdays_all <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -total_percentageabsentdays), y=~(total_percentageabsentdays*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(全部)表決日缺席率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="表決日缺席率 (%)"),
			legend=list(x=0.8, y=0.9))

fig_absentvotes_all <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -total_percentageabsentvotes), y=~(total_percentageabsentvotes*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(全部)無㩒掣投票率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="無㩒掣投票率 (%)"),
			legend=list(x=0.8, y=0.9))




# council only

fig_absentdays_council <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -council_percentageabsentdays), y=~(council_percentageabsentdays*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)表決日缺席率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="表決日缺席率 (%)"),
			legend=list(x=0.8, y=0.9))

fig_absentvotes_council <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -council_percentageabsentvotes), y=~(council_percentageabsentvotes*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)無㩒掣投票率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="無㩒掣投票率 (%)"),
			legend=list(x=0.8, y=0.9))



# finance committee only

fig_absentdays_finance <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -finance_percentageabsentdays), y=~(finance_percentageabsentdays*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(財委會)表決日缺席率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="表決日缺席率 (%)"),
			legend=list(x=0.8, y=0.9))

fig_absentvotes_finance <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -finance_percentageabsentvotes), y=~(finance_percentageabsentvotes*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(財委會)無㩒掣投票率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="無㩒掣投票率 (%)"),
			legend=list(x=0.8, y=0.9))


# house committee only

fig_absentdays_house <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -house_percentageabsentdays), y=~(house_percentageabsentdays*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(內會)表決日缺席率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="表決日缺席率 (%)"),
			legend=list(x=0.8, y=0.9))

fig_absentvotes_house <-
plot_ly(data=absence) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -house_percentageabsentdays), y=~(house_percentageabsentdays*100),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(內會)無㩒掣投票率", y=0.99),
			xaxis=list(title="", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="無㩒掣投票率 (%)"),
			legend=list(x=0.8, y=0.9))



htmlwidgets::saveWidget(fig_absentdays_all, "fig_absentdays_all.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentvotes_all, "fig_absentvotes_all.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentdays_council, "fig_absentdays_council.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentvotes_council, "fig_absentvotes_council.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentdays_finance, "fig_absentdays_finance.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentvotes_finance, "fig_absentvotes_finance.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentdays_house, "fig_absentdays_house.html", selfcontained=T)
htmlwidgets::saveWidget(fig_absentvotes_house, "fig_absentvotes_house.html", selfcontained=T)














####################
# 2 MDS SIMILARITY #
####################

# read
mds_to_plot <- read.csv("../03-analysis/legco-201610-202006-vote-mds.csv")

# plot
fig_mds_council <-
plot_ly(data=mds_to_plot) %>%
	add_trace(type="scatter", mode="markers",
			x=~round(council_dim1, 3), y=~round(council_dim2, 3),
			color=~caucus_ch, colors=colorset,
			marker=list(size=12, line=list(color="black", width=1)),
			text=~paste(member_ch, " (", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)投票相似度", y=0.99),
			xaxis=list(title="第一維度", zeroline=F),
			yaxis=list(title="第二維度", zeroline=F),
			legend=list(x=0.8, y=0.9))

fig_mds_finance <-
plot_ly(data=mds_to_plot) %>%
	add_trace(type="scatter", mode="markers",
			x=~round(finance_dim1, 3), y=~round(finance_dim2, 3),
			color=~caucus_ch, colors=colorset,
			marker=list(size=12, line=list(color="black", width=1)),
			text=~paste(member_ch, " (", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(財委會)投票相似度", y=0.99),
			xaxis=list(title="第一維度", zeroline=F),
			yaxis=list(title="第二維度", zeroline=F),
			legend=list(x=0.8, y=0.9))

fig_mds_house <-
plot_ly(data=mds_to_plot) %>%
	add_trace(type="scatter", mode="markers",
			x=~round(house_dim1, 3), y=~round(house_dim2, 3),
			color=~caucus_ch, colors=colorset,
			marker=list(size=12, line=list(color="black", width=1)),
			text=~paste(member_ch, " (", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(內會)投票相似度", y=0.99),
			xaxis=list(title="第一維度", zeroline=F, autorange="reversed"),
			yaxis=list(title="第二維度", zeroline=F),
			legend=list(x=0.8, y=0.9))




# save
htmlwidgets::saveWidget(fig_mds_council, "fig_mds_council.html")
htmlwidgets::saveWidget(fig_mds_finance, "fig_mds_finance.html")
htmlwidgets::saveWidget(fig_mds_house, "fig_mds_house.html")











###################
# 3 SUCCESS VOTES #
###################

success <- read.csv("../03-analysis/legco-201610-202006-vote-success.csv")
head(success)


fig_success_council <-
plot_ly(data=success) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -council_percentage), y=~council_percentage*100,
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)成功爭取率", y=0.99),
			xaxis=list(title="議員", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="成功爭取率 (%)"),
			legend=list(x=0.8, y=0.9))


fig_success_finance <-
plot_ly(data=success) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -finance_percentage), y=~finance_percentage*100,
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(財委會)成功爭取率", y=0.99),
			xaxis=list(title="議員", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="成功爭取率 (%)"),
			legend=list(x=0.8, y=0.9))


fig_success_house <-
plot_ly(data=success) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -house_percentage), y=~house_percentage*100,
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(內會)成功爭取率", y=0.99),
			xaxis=list(title="議員", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="成功爭取率 (%)"),
			legend=list(x=0.8, y=0.9))

htmlwidgets::saveWidget(fig_success_council, "fig_success_council.html", selfcontained=T)
htmlwidgets::saveWidget(fig_success_finance, "fig_success_finance.html", selfcontained=T)
htmlwidgets::saveWidget(fig_success_house, "fig_success_house.html", selfcontained=T)






###################################################################
# 4 SUCCESSFUL MOTIONS, PER MOVERTYPE, PER MOVERCAUCUS, PER MOVER #
###################################################################

# read
motion <- read.csv("../03-analysis/legco-201610-202006-council-motion-results.csv")



# also calculate who's the most "hard working" member

motion$total <- motion$passed + motion$negatived



fig_motion_frequency <-
plot_ly(data=motion) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -total), y=~total,
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)提出動議次數", y=0.99),
			xaxis=list(title="動議者", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="動議次數"),
			legend=list(x=0.8, y=0.9))




fig_motion_passed <-
plot_ly(data=motion) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -passed), y=~passed,
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)動議獲得通過次數", y=0.99),
			xaxis=list(title="動議者", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="動議被通過次數"),
			legend=list(x=0.8, y=0.9))




fig_motion_negatived <-
plot_ly(data=motion) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -negatived), y=~negatived,
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)動議被否決次數", y=0.01),
			xaxis=list(title="動議者", tickangle=-90, tickfont = list(size=9), side="top"),
			yaxis=list(title="動議被否決次數", autorange = "reversed"),
			legend=list(x=0.8, y=0.9))




fig_motion_passedrate <-
plot_ly(data=motion) %>%
	add_trace(type="bar",
			x=~reorder(member_ch, -(passed/total)), y=~round(passed/total*100, 2),
			color=~caucus_ch, colors=colorset,
			marker=list(line=list(color="black", width=1)),
			text=~paste("(", party_ch, ")", sep="")) %>%
	layout(title=list(text="第六屆香港立法會(大會)動議獲得通過率", y=0.99),
			xaxis=list(title="動議者", tickangle=-90, tickfont = list(size=9)),
			yaxis=list(title="動議被通過率 (%)"),
			legend=list(x=0.8, y=0.9))



# some descriptive stats
tapply(motion$passed, motion$caucus_en, sum)
tapply(motion$negatived, motion$caucus_en, sum)

tapply(motion$passed, motion$caucus_en, sum)
tapply(motion$total, motion$caucus_en, sum)

tapply(motion$passed, motion$caucus_en, sum) / tapply(motion$total, motion$caucus_en, sum)


# save
htmlwidgets::saveWidget(fig_motion_frequency, "fig_motion_frequency.html", selfcontained=T)
htmlwidgets::saveWidget(fig_motion_passed, "fig_motion_passed.html", selfcontained=T)
htmlwidgets::saveWidget(fig_motion_negatived, "fig_motion_negatived.html", selfcontained=T)
htmlwidgets::saveWidget(fig_motion_passedrate, "fig_motion_passedrate.html", selfcontained=T)






# move all htmls to ../docs/
all_htmls <- dir(pattern="*.html")
for (i in 1:length(all_htmls)){
	file.copy(all_htmls[i], "../docs/")
}
