library(data.table)

master <- fread("../01-extract-data-from-XML/legco-201610-202006.csv")
member <- fread("../02-manual-members-info/legcomembers.csv")

# remove youngspiration as they were unable to vote, due to the oath-taking controversy
# cf https://en.wikipedia.org/wiki/Hong_Kong_Legislative_Council_oath-taking_controversy
master$sixtusleung <- NULL
master$yauwaiching <- NULL
member <- subset(member, member$party_en != "youngspiration")


# note a special case in the motionresult
master[1589,]

# cf. https://www.legco.gov.hk/yr16-17/chinese/hc/minutes/hc20170428.pdf
# end result was that starrylee voted no to break the tie, thus "negatived"
master[1589,]$motionresult <- c("negatived")


# also subset records that belong to each "committee"

council <- master[committee=="council"]
finance <- master[committee=="finance"]
house <- master[committee=="house"]





####################
# 1 ABSENT RECORDS #
####################

# simple definition:
# if *all* votes on a voting day = absent, then member is absent that day

# to check if a member is present
is.present <- function(x){

	# calculate num of "absent" votes
	numofabsent <- sum(x == c("absent"))

	# calculate num of motions in total
	numofmotions <- length(x)

	# calculate absent percentage
	absentpercentage <- numofabsent/numofmotions

	# IF percentage is NA, return NA,
	ifelse(is.na(absentpercentage), NA,

		# ELSEIF absent percentage is 100, return False
		# ELSE return T
		ifelse(numofabsent/numofmotions == 1, F, T)
	)
}





# since we will organize the "is present data" per motion date
# use unique(master$motiondate) as the base

master_present_days <- data.table(motiondate = master[, unique(motiondate)])
council_present_days <- data.table(motiondate = council[, unique(motiondate)])
finance_present_days <- data.table(motiondate = finance[, unique(motiondate)])
house_present_days <- data.table(motiondate = house[, unique(motiondate)])


# loop through each member
for (memberindex in 1:length(member$member_en)){

	# get name of target member
	targetmember <- member$member_en[memberindex]

	# check if a certain member has voted on certain day
	targetmembervotingdays_master <- master[, is.present(get(targetmember)), by=motiondate]
	targetmembervotingdays_council <- council[, is.present(get(targetmember)), by=motiondate]
	targetmembervotingdays_finance <- finance[, is.present(get(targetmember)), by=motiondate]
	targetmembervotingdays_house <- house[, is.present(get(targetmember)), by=motiondate]

	# rename columns
	names(targetmembervotingdays_master) <- c("motiondate", targetmember)
	names(targetmembervotingdays_council) <- c("motiondate", targetmember)
	names(targetmembervotingdays_finance) <- c("motiondate", targetmember)
	names(targetmembervotingdays_house) <- c("motiondate", targetmember)


	# cbind/merge with the master
	master_present_days <- merge(master_present_days, targetmembervotingdays_master, by="motiondate")
	council_present_days <- merge(council_present_days, targetmembervotingdays_council, by="motiondate")
	finance_present_days <- merge(finance_present_days, targetmembervotingdays_finance, by="motiondate")
	house_present_days <- merge(house_present_days, targetmembervotingdays_house, by="motiondate")
}





# set up a master frame to hold all calculations

absent_records <- data.table(matrix(nrow=0, ncol=17, dimnames=list(c(),
												c("member_en", "total_absentdays", "total_eligibledays", "total_absentvotes", "total_eligiblevotes",
												"council_absentdays", "council_eligibledays", "council_absentvotes", "council_eligiblevotes",
												"finance_absentdays", "finance_eligibledays", "finance_absentvotes", "finance_eligiblevotes",
												"house_absentdays", "house_eligibledays", "house_absentvotes", "house_eligiblevotes"))))


# now loop through each member
# to calculate total of absent days and votes
for (memberindex in 1:length(member$member_en)){

	# get name of target member
	targetmember <- member$member_en[memberindex]

	# calculate num of absent days in each committee
	total_absentdays <- nrow(master_present_days[get(targetmember)==F, ])
	council_absentdays <- nrow(council_present_days[get(targetmember)==F, ])
	finance_absentdays <- nrow(finance_present_days[get(targetmember)==F, ])
	house_absentdays <- nrow(house_present_days[get(targetmember)==F, ])

	# calculate num of days that member is
	# a "qualified" legco member in each committee
	total_eligibledays <- master_present_days[, sum(!is.na(get(targetmember)))]
	council_eligibledays <- council_present_days[, sum(!is.na(get(targetmember)))]
	finance_eligibledays <- finance_present_days[, sum(!is.na(get(targetmember)))]
	house_eligibledays <- house_present_days[, sum(!is.na(get(targetmember)))]

	# calculate num of absent votes
	total_absentvotes <- master[get(targetmember)=="absent", .N]
	council_absentvotes <- council[get(targetmember)=="absent", .N]
	finance_absentvotes <- finance[get(targetmember)=="absent", .N]
	house_absentvotes <- house[get(targetmember)=="absent", .N]

	# calculate num of voting sessions in which member is eligible to vote
	total_eligiblevotes <- master[, sum(!is.na(get(targetmember)))]
	council_eligiblevotes <- council[, sum(!is.na(get(targetmember)))]
	finance_eligiblevotes <- finance[, sum(!is.na(get(targetmember)))]
	house_eligiblevotes <- house[, sum(!is.na(get(targetmember)))]

	# paste them into the master frame
	tempframe <- as.data.table(cbind(targetmember, total_absentdays, total_eligibledays, total_absentvotes, total_eligiblevotes,
									council_absentdays, council_eligibledays, council_absentvotes, council_eligiblevotes,
									finance_absentdays, finance_eligibledays, finance_absentvotes, finance_eligiblevotes,
									house_absentdays, house_eligibledays, house_absentvotes, house_eligiblevotes))
	names(tempframe) <- c("member_en", "total_absentdays", "total_eligibledays", "total_absentvotes", "total_eligiblevotes",
						"council_absentdays", "council_eligibledays", "council_absentvotes", "council_eligiblevotes",
						"finance_absentdays", "finance_eligibledays", "finance_absentvotes", "finance_eligiblevotes",
						"house_absentdays", "house_eligibledays", "house_absentvotes", "house_eligiblevotes")
	absent_records <- rbind(absent_records, tempframe)
}


# change columns to integers
# lazy way: loop through relevant columns
for (i in names(absent_records)[2:17]){
	absent_records[, (i) := as.integer(get(i))]
}


# calculate percentage
absent_records[, total_percentageabsentdays := round(total_absentdays / total_eligibledays, 3)]
absent_records[, total_percentageabsentvotes := round(total_absentvotes / total_eligiblevotes, 3)]
absent_records[, council_percentageabsentdays := round(council_absentdays / council_eligibledays, 3)]
absent_records[, council_percentageabsentvotes := round(council_absentvotes / council_eligiblevotes, 3)]
absent_records[, finance_percentageabsentdays := round(finance_absentdays / finance_eligibledays, 3)]
absent_records[, finance_percentageabsentvotes := round(finance_absentvotes / finance_eligiblevotes, 3)]
absent_records[, house_percentageabsentdays := round(house_absentdays / house_eligibledays, 3)]
absent_records[, house_percentageabsentvotes := round(house_absentvotes / house_eligiblevotes, 3)]



# join the member info, some aesthetics
absent_records <- merge(absent_records, member, by="member_en")
names(absent_records)

setcolorder(absent_records, c("member_en", "member_ch",
							"constituency_en", "constituency_ch",
							"party_en", "party_ch",
							"caucus_en", "caucus_ch",
							"total_absentdays", "total_eligibledays", "total_percentageabsentdays",
							"total_absentvotes", "total_eligiblevotes", "total_percentageabsentvotes",
							"council_absentdays", "council_eligibledays", "council_percentageabsentdays",
							"council_absentvotes", "council_eligiblevotes", "council_percentageabsentvotes",
							"finance_absentdays", "finance_eligibledays", "finance_percentageabsentdays",
							"finance_absentvotes", "finance_eligiblevotes", "finance_percentageabsentvotes",
							"house_absentdays", "house_eligibledays", "house_percentageabsentdays",
							"house_absentvotes", "house_eligiblevotes", "house_percentageabsentvotes"))


# save
fwrite(absent_records, "legco-201610-202006-absences.csv")













####################
# 2 MDS SIMILARITY #
####################

dim(member)
member$member_en

# set up data.tables / similarity matrices
similarity_matrix_master <- matrix(nrow=length(member$member_en), ncol=length(member$member_en),
									dimnames=list(member$member_en, member$member_en))
similarity_matrix_council <- matrix(nrow=length(member$member_en), ncol=length(member$member_en),
									dimnames=list(member$member_en, member$member_en))
similarity_matrix_finance <- matrix(nrow=length(member$member_en), ncol=length(member$member_en),
									dimnames=list(member$member_en, member$member_en))
similarity_matrix_house <- matrix(nrow=length(member$member_en), ncol=length(member$member_en),
									dimnames=list(member$member_en, member$member_en))



# For each member
for (memberindexX in 1:length(member$member_en)){

	# get his/her name
	targetmemberX <- member$member_en[memberindexX]

	# Compare against each member
	for (memberindexY in 1:length(member$member_en)){

		# get his/her name
		targetmemberY <- member$member_en[memberindexY]

		# vote similarity = if they performed the same action for a particular motion
		votesimilarity_master <- ifelse(master[, get(targetmemberX)] == master[, get(targetmemberY)], 1, 0)
		votesimilarity_council <- ifelse(council[, get(targetmemberX)] == council[, get(targetmemberY)], 1, 0)
		votesimilarity_finance <- ifelse(finance[, get(targetmemberX)] == finance[, get(targetmemberY)], 1, 0)
		votesimilarity_house <- ifelse(house[, get(targetmemberX)] == house[, get(targetmemberY)], 1, 0)

		# num of actions that are the same
		identicalvotes_master <- sum(votesimilarity_master, na.rm=T)
		identicalvotes_council <- sum(votesimilarity_council, na.rm=T)
		identicalvotes_finance <- sum(votesimilarity_finance, na.rm=T)
		identicalvotes_house <- sum(votesimilarity_house, na.rm=T)

		# num of motions that they are eligible together
		numvalidcases_master <- sum(!is.na(votesimilarity_master))
		numvalidcases_council <- sum(!is.na(votesimilarity_council))
		numvalidcases_finance <- sum(!is.na(votesimilarity_finance))
		numvalidcases_house <- sum(!is.na(votesimilarity_house))

		# calculate the percentage of similar actions / num of eligible motions
		# save it to the master matrix
		similarity_matrix_master[memberindexX, memberindexY] <- identicalvotes_master / numvalidcases_master
		similarity_matrix_council[memberindexX, memberindexY] <- identicalvotes_council / numvalidcases_council
		similarity_matrix_finance[memberindexX, memberindexY] <- identicalvotes_finance / numvalidcases_finance
		similarity_matrix_house[memberindexX, memberindexY] <- identicalvotes_house / numvalidcases_house
	}
}


# save similarity matrices if needed
# write.csv(similarity_matrix_master, "legco-201610-202006-all-voting-similarity.csv")
# write.csv(similarity_matrix_council, "legco-201610-202006-council-voting-similarity.csv")
# write.csv(similarity_matrix_finance, "legco-201610-202006-finance-voting-similarity.csv")
# write.csv(similarity_matrix_house, "legco-201610-202006-house-voting-similarity.csv")


# remove andrewleung from finance and hosue,
# since an NA row+column in dist object would not work
similarity_matrix_finance <- similarity_matrix_finance[-4,-4]
similarity_matrix_house <- similarity_matrix_house[-4,-4]


# convert to distance matrix
distance_matrix_master <- as.dist(1-similarity_matrix_master)
distance_matrix_council <- as.dist(1-similarity_matrix_council)
distance_matrix_finance <- as.dist(1-similarity_matrix_finance)
distance_matrix_house <- as.dist(1-similarity_matrix_house)


# since we have NA values in distance matrix
# transform the matrix with isomap
# make test plots
distance_ISOMAP_master <- vegan::isomapdist(distance_matrix_master, k=1000)
distance_ISOMAP_council <- vegan::isomapdist(distance_matrix_council, k=1000)
distance_ISOMAP_finance <- vegan::isomapdist(distance_matrix_finance, k=1000)
distance_ISOMAP_house <- vegan::isomapdist(distance_matrix_house, k=1000)
isomds_master <- MASS::isoMDS(distance_ISOMAP_master, k=2)
isomds_council <- MASS::isoMDS(distance_ISOMAP_council, k=2)
isomds_finance <- MASS::isoMDS(distance_ISOMAP_finance, k=2)
# isomds_house <- MASS::isoMDS(distance_ISOMAP_house, k=2) # doesn't work since we have folks with identical voting records

# try cheating a little
# change folks with 0 distance to 0.0000000000000000000000000000001
distance_ISOMAP_house[distance_ISOMAP_house==0] <- 0.0000000000000000000000000000001
isomds_house <- MASS::isoMDS(distance_ISOMAP_house, k=2)

# test plots
# plot(isomds_master$points[,1], isomds_master$points[,2])
# plot(isomds_council$points[,1], isomds_council$points[,2])
# plot(isomds_finance$points[,1], isomds_finance$points[,2])
# plot(isomds_house$points[,1], isomds_house$points[,2])


## compile all MDS points, then save
# extract the points
mds_master <- as.data.frame(isomds_master$points)
mds_council <- as.data.frame(isomds_council$points)
mds_finance <- as.data.frame(isomds_finance$points)
mds_house <- as.data.frame(isomds_house$points)

# rename columns
names(mds_master) <- c("all_dim1", "all_dim2")
names(mds_council) <- c("council_dim1", "council_dim2")
names(mds_finance) <- c("finance_dim1", "finance_dim2")
names(mds_house) <- c("house_dim1", "house_dim2")

# add member_en column for merging
mds_master$member_en <- row.names(mds_master)
mds_council$member_en <- row.names(mds_council)
mds_finance$member_en <- row.names(mds_finance)
mds_house$member_en <- row.names(mds_house)

# merge with member
mds_to_plot <- merge(x=member, y=mds_master, by="member_en", all.x=T)
mds_to_plot <- merge(x=mds_to_plot, y=mds_council, by="member_en", all.x=T)
mds_to_plot <- merge(x=mds_to_plot, y=mds_finance, by="member_en", all.x=T)
mds_to_plot <- merge(x=mds_to_plot, y=mds_house, by="member_en", all.x=T)

# save
fwrite(mds_to_plot, "legco-201610-202006-vote-mds.csv")



###################################################################
# 3 SUCCESSFUL MOTIONS, PER MOVERTYPE, PER MOVERCAUCUS, PER MOVER #
###################################################################

# there's also a case of a tie -- seems like the decision was "no immediate action required"
# cf. https://www.legco.gov.hk/yr16-17/chinese/fc/fc/minutes/fc20170719c.pdf
master[motionresult=="tied"]

# the only case where a gov officer motioned
# but failed, was one specific amendment
# https://www.legco.gov.hk/php/hansard/chinese/rundown.php?term=yr16-20&date=2018-02-08&lang=0
council[movertype=="publicofficer" & motionresult=="negatived"]



# cross-tab mover x motionresult, clean up, add the member info
council_motion_result <- as.data.table(table(council$mover, council$motionresult))
council_motion_result <- dcast(council_motion_result, V1~V2, value.var="N")
names(council_motion_result) <- c("member_en", "negatived", "passed")
council_motion_result <- merge(x=council_motion_result, y=member, all.x=T)

# steps to merge all bills from the government
# calculate the total # of negatived and passed, drop all government rows
# then add it back as one single row
# manual work, i know. Too lazy to think of an automated solution for now
council_gov_negatived <- council_motion_result[is.na(constituency_en), sum(negatived)]
council_gov_passed <- council_motion_result[is.na(constituency_en), sum(passed)]
council_motion_result <- council_motion_result[!is.na(constituency_en)]

council_motion_result <- rbindlist(list(council_motion_result,
										data.table(member_en="government",
													negatived=council_gov_negatived, passed=council_gov_passed,
													member_ch="政府法案", constituency_en="government", constituency_ch="政府",
													party_en="government", party_ch="政府", caucus_en="government", caucus_ch="政府")))

# aesthetics
setcolorder(council_motion_result, c("member_en", "member_ch", "constituency_en", "constituency_ch",
								"party_en", "party_ch", "caucus_en", "caucus_ch",
								"negatived", "passed"))

# save
fwrite(council_motion_result, "legco-201610-202006-council-motion-results.csv")






####################
# Successful votes #
# i.e. "yes" then pass
# i.e. "no" then negatived

# function to check if the vote is "successful"
is.successful <- function(result, vote){

	# if "negatived" + "no" then T, otherwise
	ifelse(result=="negatived" & vote=="no", T,

			# if "passed" + "yes" then T, otherwise F
			ifelse(result=="passed" & vote=="yes", T, F))
}

# copy master as a template
master_successful <- master[,1:7]

# loop through each member
for (memberindex in 1:length(member$member_en)){

	# get name of target member
	targetmember <- member$member_en[memberindex]

	# T = "successful", F = "failed"

	# overwrite column
	# master[, get(targetmember) := is.successful(motionresult, get(targetmember))]
	# not sure why the above doesn't work; i have a feeling that you can't do get() := get()
	# let's use the roundabout way below

	# get the comparison results
	tempsuccess <- master[, is.successful(motionresult, get(targetmember))]

	# add column, then rename that new column
	master_successful[, ":=" (abc = tempsuccess)]
	names(master_successful)[dim(master_successful)[2]] <- targetmember
}



# set up blank data.table for summary
successful_summary <- data.table(matrix(nrow=0, ncol=9, dimnames=list(c(),
												c("member_en", "total_successful", "total_eligible",
												"council_successful", "council_eligible",
												"finance_successful", "finance_eligible",
												"house_successful", "house_eligible"))))


# loop through each member again
for (memberindex in 1:length(member$member_en)){

	# get name of target member
	targetmember <- member$member_en[memberindex]

	# get the needed information
	# - successful votes
	# - eligible = motions for which member is an eligible member of legco

	total_successful <- master_successful[, sum(get(targetmember), na.rm=T)]
	total_eligible <- master_successful[, sum(!is.na(get(targetmember)), na.rm=T)]

	council_successful <- master_successful[committee=="council", sum(get(targetmember), na.rm=T)]
	council_eligible <- master_successful[committee=="council", sum(!is.na(get(targetmember)), na.rm=T)]

	finance_successful <- master_successful[committee=="finance", sum(get(targetmember), na.rm=T)]
	finance_eligible <- master_successful[committee=="finance", sum(!is.na(get(targetmember)), na.rm=T)]

	house_successful <- master_successful[committee=="house", sum(get(targetmember), na.rm=T)]
	house_eligible <- master_successful[committee=="house", sum(!is.na(get(targetmember)), na.rm=T)]

	# mold into a temp row
	tempsummary <- data.table(cbind(targetmember, total_successful, total_eligible,
									council_successful, council_eligible,
									finance_successful, finance_eligible,
									house_successful, house_eligible))

	# rename just in case
	names(tempsummary) <- c("member_en", "total_successful", "total_eligible",
							"council_successful", "council_eligible",
							"finance_successful", "finance_eligible",
							"house_successful", "house_eligible")

	# rbind with the summary data.table
	successful_summary <- rbind(successful_summary, tempsummary)

}

# change columns to integers
# lazy way: loop through relevant columns
for (i in names(successful_summary)[2:9]){
	successful_summary[, (i) := as.integer(get(i))]
}


# calculate percentages
successful_summary[, total_percentage := round(total_successful / total_eligible, 3)]
successful_summary[, council_percentage := round(council_successful / council_eligible, 3)]
successful_summary[, finance_percentage := round(finance_successful / finance_eligible, 3)]
successful_summary[, house_percentage := round(house_successful / house_eligible, 3)]


# left join the member info
successful_summary <- merge(x=successful_summary, y=member, by="member_en")

# aesthetics
setcolorder(successful_summary, c("member_en", "member_ch", "constituency_en", "constituency_ch",
								"party_en", "party_ch", "caucus_en", "caucus_ch",
								"total_successful", "total_eligible", "total_percentage",
								"council_successful", "council_eligible", "council_percentage",
								"finance_successful", "finance_eligible", "finance_percentage",
								"house_successful", "house_eligible", "house_percentage"))

# save
fwrite(successful_summary, "legco-201610-202006-vote-success.csv")
