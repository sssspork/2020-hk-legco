### General information

**member_en, member_ch**:
- Members' names

**constituency_en, constituency_ch**
- [Functional vs geographical](https://en.wikipedia.org/wiki/Legislative_Council_of_Hong_Kong#Membership_composition)

**party_en, party_ch, caucus_en, caucus_ch**
- Member's political party/affiliation


---


### "absences"

**total_ / council_ / finance_ / house_**
- counts for all records, for the "main" Legislative Council meetings, for the Finance Committee meetings, and for the House Committee meetings, respectively
  - **absentdays**: days when member only casted "absent" votes
  - **eligibledays**: days when member was a qualified legco member, eligibile to cast votes
  - **percentageabsentdays**: absentdays / eligibledays
  - **absentvotes**: number of "absent" votes casted
  - **eligiblevotes**: number of votes that a member was qualified / eligible to cast
  - **percentageabsentvotes**: absentvotes / eligiblevotes


---


### "council-motion-result"
Motion results for motions in the "main" Legislative Council only

**member_en, member_ch**:
- Motion movers' names

**negatived, passed**:
- Number of motions from that particular mover, that have been negatived or passed


---


### "vote-success"

**total_ / council_ / finance_ / house_**
- counts for all records, for the "main" Legislative Council meetings, for the Finance Committee meetings, and for the House Committee meetings, respectively

  - **successful**: number of votes that concurred with the result of the motion, i.e. a "yes" vote for a "passed" motion, or a "no" vote for a "negatived motion"

  - **eligible**: number of votes that a member was qualified / eligible to cast

  - **percentage**: successful / eligible


---


### "vote-mds"
#### Method:
1. Transformed all voting records into similarity scores ("1" if 2 members casted the same vote for a motion, "0" if they casted different votes)
2. Calculated percentage of identical votes / eligible votes between the members, formed a similarity matrix
3. Transformed similarity matrix into distance matrix
4. Processed the distance matrix with `vegan::isomapdist` to remove NAs in distance matrix (because of [the non-overlapping eligibility of the disqualified members and supplementary members](https://en.wikipedia.org/wiki/Hong_Kong_Legislative_Council_oath-taking_controversy)). Also cheated a little - members with 0 distances were changed to have a distances of 1e-31
5. Used `MASS::isoMDS` to transform distance matrix to 2-dimentional coordinates

**all_ / council_ / finance_ / house_**
- results for all records, for the "main" Legislative Council meetings, for the Finance Committee meetings, and for the House Committee meetings, respectively
  - **dim1, dim2**: 2-dimentional coordinates generated from voting records
