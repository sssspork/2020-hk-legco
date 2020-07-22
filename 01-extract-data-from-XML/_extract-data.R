library(xml2)


#############
# PREP WORK #
#############


#####
# First loop through all xml files and get all members' names
#####

# read all xml files
alllegco <- dir(path="../00-original/1-council/", pattern="*.xml")
allfinance <- dir(path="../00-original/2-finance-committee/", pattern="*.xml")
allhouse <- dir(path="../00-original/3-house-committee/", pattern="*.xml")


# set up dummy vector
legcomembers <- c()

# 1. get member names from "main" legislative council
# loop through each xml file
for (i in 1:length(alllegco)){

	temp <- read_xml(paste("../00-original/1-council/", alllegco[i], sep=""))

	# find all "member" nodes
	allmembernodes <- xml_find_all(temp, ".//member")

	# save all the member names into the vector
	legcomembers <- c(legcomembers, xml_attr(allmembernodes, "name-en"))
}

# 2. repeat process for finance committee
for (i in 1:length(allfinance)){

	temp <- read_xml(paste("../00-original/2-finance-committee/", allfinance[i], sep=""))

	# find all "member" nodes
	allmembernodes <- xml_find_all(temp, ".//member")

	# save all the member names into the vector
	legcomembers <- c(legcomembers, xml_attr(allmembernodes, "name-en"))
}

# 3. repeat process for house committee
for (i in 1:length(allhouse)){

	temp <- read_xml(paste("../00-original/3-house-committee/", allhouse[i], sep=""))

	# find all "member" nodes
	allmembernodes <- xml_find_all(temp, ".//member")

	# save all the member names into the vector
	legcomembers <- c(legcomembers, xml_attr(allmembernodes, "name-en"))
}

legcomembernames <- tolower(gsub("[- ]", "", unique(legcomembers)))





#####
# Second, set up frames to retain the extracted data:
#####

# think of the information needed
# motion-date, motion-names, mover, mover-type, + all the member's names
infoneeded <- c("committee", "motiondate", "motionname_ch", "motionname_en", "mover", "movertype", "motionresult", legcomembernames)

# set up an empty master frame, to retain the data
master <- data.frame(matrix(nrow=0, ncol=length(infoneeded)))
names(master) <- infoneeded







##########################
# ACTUAL DATA EXTRACTION #
##########################

#####################
# 1. "Main" Council #
#####################

# loop through each xml file
for (targetfileID in 1:length(alllegco)){

	# read in each xml record, parse it
	targetfile <- paste("../00-original/1-council/", alllegco[targetfileID], sep="")
	targetfile_parsed <- read_xml(targetfile)

	# extract all motions there are in each file
	allmotions <- xml_find_all(targetfile_parsed, "meeting/vote")

	##########
	# loop through each motion
	##########

	for (motionID in 1:length(allmotions)){

		# retrieve each motion
		motion <- xml_children(allmotions[motionID])

		# extract the basic info of each motion
		# names of motion, date, mover, movertype
		motioncommittee <- c("council")
		motionname_ch <- xml_text(xml_find_all(motion, "..//motion-ch"))
		motionname_en <- tolower(xml_text(xml_find_all(motion, "..//motion-en")))
		motiondate <- as.Date(xml_text(xml_find_all(motion, "..//vote-date")), tryFormats = c("%d/%m/%Y", "%d/%m/%y"))
		mover <- xml_text(xml_find_all(motion, "../mover-en"))
		mover <- tolower(gsub("[- ]", "", mover))	# slight clean up for easier organization
		movertype <- tolower(xml_text(xml_find_all(motion, "../mover-type")))
		motionresult <- tolower(xml_text(xml_find_all(motion, "..//overall/result")))

		# set up a temp row to fill out the info from current motion
		tempmotionrow <- data.frame(matrix(NA, nrow=1, ncol=length(infoneeded)))
		names(tempmotionrow) <- infoneeded

		# fill out the temp row
		tempmotionrow$committee <- motioncommittee
		tempmotionrow$motiondate <- motiondate
		tempmotionrow$motionname_ch <- motionname_ch
		tempmotionrow$motionname_en <- motionname_en
		tempmotionrow$mover <- mover
		tempmotionrow$movertype <- movertype
		tempmotionrow$motionresult <- motionresult

		# then go into each member's vote
		allmembers <- xml_find_all(motion, ".//member")

		# loop through each member
		for (membersID in 1:length(allmembers)){

			# for each member
			member <- allmembers[membersID]

			# get their name
			membername <- xml_attr(member, "name-en")
			membername <- tolower(gsub("[- ]", "", membername))

			# get their vote
			membervote <- tolower(xml_text(xml_children(member)))

			# add their record to the temprow
			tempmotionrow[grep(membername, names(tempmotionrow))] <- membervote

		}

		# attach each tempmotionrow to the master
		master <- rbind(master, tempmotionrow)

	}
}





########################
# 2. Finance committee #
########################

# loop through each xml file
for (targetfileID in 1:length(allfinance)){

	# read in each xml record, parse it
	targetfile <- paste("../00-original/2-finance-committee/", allfinance[targetfileID], sep="")
	targetfile_parsed <- read_xml(targetfile)

	# extract all motions there are in each file
	allmotions <- xml_find_all(targetfile_parsed, "meeting/vote")

	##########
	# loop through each motion
	##########

	for (motionID in 1:length(allmotions)){

		# retrieve each motion
		motion <- xml_children(allmotions[motionID])

		# extract the basic info of each motion
		# names of motion, date, mover, movertype
		motioncommittee <- c("finance")
		motionname_ch <- xml_text(xml_find_all(motion, "..//motion-ch"))
		motionname_en <- tolower(xml_text(xml_find_all(motion, "..//motion-en")))
		motiondate <- as.Date(xml_text(xml_find_all(motion, "..//vote-date")), tryFormats = c("%d/%m/%Y", "%d/%m/%y"))
		mover <- xml_text(xml_find_all(motion, "../mover-en"))
		mover <- tolower(gsub("[- ]", "", mover))	# slight clean up for easier organization
		movertype <- tolower(xml_text(xml_find_all(motion, "../mover-type")))
		motionresult <- tolower(xml_text(xml_find_all(motion, "..//overall/result")))

		# set up a temp row to fill out the info from current motion
		tempmotionrow <- data.frame(matrix(NA, nrow=1, ncol=length(infoneeded)))
		names(tempmotionrow) <- infoneeded

		# fill out the temp row
		tempmotionrow$committee <- motioncommittee
		tempmotionrow$motiondate <- motiondate
		tempmotionrow$motionname_ch <- motionname_ch
		tempmotionrow$motionname_en <- motionname_en
		tempmotionrow$mover <- mover
		tempmotionrow$movertype <- movertype
		tempmotionrow$motionresult <- motionresult

		# then go into each member's vote
		allmembers <- xml_find_all(motion, ".//member")

		# loop through each member
		for (membersID in 1:length(allmembers)){

			# for each member
			member <- allmembers[membersID]

			# get their name
			membername <- xml_attr(member, "name-en")
			membername <- tolower(gsub("[- ]", "", membername))

			# get their vote
			membervote <- tolower(xml_text(xml_children(member)))

			# add their record to the temprow
			tempmotionrow[grep(membername, names(tempmotionrow))] <- membervote

		}

		# attach each tempmotionrow to the master
		master <- rbind(master, tempmotionrow)

	}
}





######################
# 3. House committee #
######################

# loop through each xml file
for (targetfileID in 1:length(allhouse)){

	# read in each xml record, parse it
	targetfile <- paste("../00-original/3-house-committee/", allhouse[targetfileID], sep="")
	targetfile_parsed <- read_xml(targetfile)

	# extract all motions there are in each file
	allmotions <- xml_find_all(targetfile_parsed, "meeting/vote")

	##########
	# loop through each motion
	##########

	for (motionID in 1:length(allmotions)){

		# retrieve each motion
		motion <- xml_children(allmotions[motionID])

		# extract the basic info of each motion
		# names of motion, date, mover, movertype
		motioncommittee <- c("house")
		motionname_ch <- xml_text(xml_find_all(motion, "..//motion-ch"))
		motionname_en <- tolower(xml_text(xml_find_all(motion, "..//motion-en")))
		motiondate <- as.Date(xml_text(xml_find_all(motion, "..//vote-date")), tryFormats = c("%d/%m/%Y", "%d/%m/%y"))
		mover <- xml_text(xml_find_all(motion, "../mover-en"))
		mover <- tolower(gsub("[- ]", "", mover))	# slight clean up for easier organization
		movertype <- tolower(xml_text(xml_find_all(motion, "../mover-type")))
		motionresult <- tolower(xml_text(xml_find_all(motion, "..//overall/result")))

		# set up a temp row to fill out the info from current motion
		tempmotionrow <- data.frame(matrix(NA, nrow=1, ncol=length(infoneeded)))
		names(tempmotionrow) <- infoneeded

		# fill out the temp row
		tempmotionrow$committee <- motioncommittee
		tempmotionrow$motiondate <- motiondate
		tempmotionrow$motionname_ch <- motionname_ch
		tempmotionrow$motionname_en <- motionname_en
		tempmotionrow$mover <- mover
		tempmotionrow$movertype <- movertype
		tempmotionrow$motionresult <- motionresult

		# then go into each member's vote
		allmembers <- xml_find_all(motion, ".//member")

		# loop through each member
		for (membersID in 1:length(allmembers)){

			# for each member
			member <- allmembers[membersID]

			# get their name
			membername <- xml_attr(member, "name-en")
			membername <- tolower(gsub("[- ]", "", membername))

			# get their vote
			membervote <- tolower(xml_text(xml_children(member)))

			# add their record to the temprow
			tempmotionrow[grep(membername, names(tempmotionrow))] <- membervote

		}

		# attach each tempmotionrow to the master
		master <- rbind(master, tempmotionrow)

	}
}






#########################
# MINOR CLEAN UP & SAVE #
#########################

# clean up the mover names;
# remove the titles
master$mover <- gsub("^dr|^irdr|^mrs|^prof", "" , master$mover)

# clean up mover type;
master$movertype <- gsub(" ", "", master$movertype)



# luckily only "chuhoidick" appear as a mover
# but both "chuhoidick" and "eddiechu" appear as voter
# also in each motion, one of them would vote, the other would be NA
# thus we only need a function to merge both the columns "chuhoidick":

merge_columns <- function(x){
	x[!is.na(x)]		# just pick out the element that's not NA
}

# extract the two columns as a temp frame
chutemp <- cbind(master$chuhoidick, master$eddiechu)

# loop through each row, get the non-NA vote, overwrite the "chuhoidick" column
master$chuhoidick <- apply(chutemp, 1, merge_columns)

# remove "eddiechu" column
master$edditchu <- NULL



# remove duplicated columns "juniusho", "elizabethquat"
# keep only the columns "drjuniusho", "drelizabethquat"
# no need to move because we simply grep-ed the member's names
# i.e. "juniusho"'s voting record would be inserted in both columns "juniusho" and "drjuniusho"
master$juniusho <- NULL
master$elizabethquat <- NULL

# clean up the column names;
# remove the titles
names(master) <- gsub("^dr|^irdr|^mrs|^prof", "" , names(master))


# save
write.csv(master, "legco-201610-202006.csv", row.names=F, fileEncoding="UTF-8")

