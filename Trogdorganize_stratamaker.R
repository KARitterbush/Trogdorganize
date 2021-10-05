# Jenny Hambleton and Kathleen Ritterbush are here to organize microscopy files.

# But first, let's make some strat columns for funsiewunsies.
strat_intervals<-sequence<-letter_set<-vector()

for(i in 1:12){
	n<-sample(c(1:6),1)
	letter<-rep(LETTERS[i],n)
	numbers<-c(1:n)
	strat_intervals<-c(strat_intervals,paste(letter,numbers,sep="_"))
	letter_set<-c(letter_set,letter)
	sequence<-c(sequence,numbers)

}


unit_thickness<-as.integer(rnorm(mean=150,sd=75,n=length(strat_intervals)))

carbonates<-factor(x=c("covered","fisile mudstone","flaggy mudestone","platy mudstone","wackestone","cherty wackestone","packstone","grainstone"),levels=c("covered","fisile mudstone","flaggy mudestone","platy mudstone","wackestone","cherty wackestone","packstone","grainstone"),ordered=T)
carbonates_resistance<-c(0,1.5,2.0,2.5,4.0,5.0,6.0,7.0)

unit_carbonate_category_assigner<-sequence+sample(c(0,1,1,2),length(sequence),replace=T)

unit_carbonate_category<-carbonates[unit_carbonate_category_assigner]

resistance_values<-carbonates_resistance[unit_carbonate_category_assigner]

data_strat<-data.frame(strat_intervals,unit_thickness,unit_carbonate_category)
unit_base_height<-(c(0,cumsum(unit_thickness)[-length(strat_intervals)]))
unit_top_height<-cumsum(unit_thickness)
colors<-hcl.colors(n=length(unique(letter_set)))

# laptop settings
# dev.new(width=9,height=7)
# desktop settings
dev.new(width=15,height=9)
par(mfrow=c(1,4))
# barplot(horiz=T,width=unit_thickness,height=resistance_values,col=grey(level=as.numeric(as.factor(letter_set))/max(as.numeric(as.factor(letter_set)))),space=0,main="measured section",axes=F,ylab="meters")
par(mar=c(10,5,3,0))
barplot(horiz=T,width=unit_thickness,height=resistance_values,col=colors[as.numeric(as.factor(letter_set))],space=0,main="Measured Section",axes=F,ylab="meters",ylim=c(0,max(unit_top_height)),xlim=c(0,20),cex.axis=0.8)
axis(side=2,at=c(0:round(sum(unit_thickness)/100))[c(T,F,F,F,F)]*100,labels=c(0:round(sum(unit_thickness)/100))[c(T,F,F,F,F)])
axis(side=1,at=carbonates_resistance,labels=carbonates,xpd=T,las=2,cex=0.8)
text(x=10,y=apply(cbind(unit_base_height,unit_top_height),1,mean),strat_intervals,cex=0.8)
segments(x0=0,x1=20,y0=unit_base_height[!duplicated(letter_set)],y1=unit_base_height[!duplicated(letter_set)],lty=3,col=colors,lwd=2)


# Now lets add some samples, thin sections, images, and surveys
# Any stratigraphic unit can be sampled. It will have more samples if it's less mud.
# say how many samples you want to hold.
samples_n<-50
sample_number<-c(1:samples_n)
# grab them from stratigraphic units. Here we'll add some probability that we sample more from units that have less mud (our more grainy, more resistant carbonate units).
sampled_from_unit<-sort(sample(strat_intervals,size=samples_n,prob=resistance_values,replace=T))
suffix<-c("i","ii","iii","iv","v","vi","vii","ix","x","xi","xii")
sample_name<-paste(sampled_from_unit,"i",sep="")
tally<-rep(1,samples_n)
for(b in 2:max(table(sampled_from_unit))){
	fix<-duplicated(sample_name)
	sample_name[fix]<-paste(sampled_from_unit[fix],suffix[b],sep="_")
	tally[fix]<-tally[fix]+1
}

# designate how high within each stratigraphic unit each sample came from. We're doing this randomly within each unit, so they could be out of order within a unit! This is realistic comparison to real field collecting.
sampled_horizon<-round(unit_thickness[match(sampled_from_unit,strat_intervals)]*sample(x=seq(from=0.05,to=0.95,by=0.05),size=samples_n,replace=T))

data_samples<-data.frame(sample_number,sample_name,sampled_from_unit,sampled_horizon)

# Now let's make thin sections and fossil specimens! Not more than one per sample for right now.
thin_sections_n<-30
thin_section_number<-c(1:thin_sections_n)
thin_section_sample<-sample(sample_number,size=thin_sections_n)
fossiliferous<-sample(c(T,F),prob=c(80,20),size=thin_sections_n,replace=T)
bivalves<-gastropods<-brachiopods<-bryozoans<-spicules<-fossiliferous

	# Now let's add an ecological signal. Let's say that brachiopods, bryozoans, and bivalves are more common in the muddy material, and gastropods are more common in the grainy material. Sponges will be random.
	fossil_eraser<-sample(c(1:nrow(data_thin_sections)),prob=resistance_values[match(sampled_from_unit[thin_section_sample],strat_intervals)],size=round(thin_sections_n/2))
	bivalves[fossil_eraser]<-F
	# run it again... same settings.
	fossil_eraser<-sample(c(1:nrow(data_thin_sections)),prob=resistance_values[match(sampled_from_unit[thin_section_sample],strat_intervals)],size=round(thin_sections_n/2))
	brachiopods[fossil_eraser]<-F
	# and run it again!
	fossil_eraser<-sample(c(1:nrow(data_thin_sections)),prob=resistance_values[match(sampled_from_unit[thin_section_sample],strat_intervals)],size=round(thin_sections_n/2))
	bryozoans[fossil_eraser]<-F
	# Now switch up the settings! Flip the probability so that the more grainy units are unlikely to have their fossils nixed.
	fossil_eraser<-sample(c(1:nrow(data_thin_sections)),prob=max(carbonates_resistance)-resistance_values[match(sampled_from_unit[thin_section_sample],strat_intervals)]+1,size=round(thin_sections_n/2))
	gastropods[fossil_eraser]<-F
	# for spicules, we'll have it removed randomely.
	fossil_eraser<-sample(c(1:nrow(data_thin_sections)),size=round(thin_sections_n/3))
	spicules[fossil_eraser]<-F


data_thin_sections<-data.frame(thin_section_number,thin_section_sample,fossiliferous,bivalves,gastropods,brachiopods,bryozoans,spicules)

# let's plot!
# x_content<-c(1:10)
# xlab<-c("sampled","bivalves","gastropods","brachiopods","bryozoans","spicules")
# y_position_samples<-unit_base_height[match(sampled_from_unit,strat_intervals)]+sampled_horizon


y_position_thin_sections<-y_position_samples[thin_section_sample]
# par(mar=c(5,0,5,0))
# par(oma=c(1,1,1,1))
# 

# points(y=y_position_samples,x=rep(0,samples_n))
# points(y=y_position_samples,x=rep(0,samples_n),pch=16,col=rgb(1,1,1,alpha=0.2))

# Now label the places we have samples
	# add points
	points(y=y_position_samples,x=tally/2+12,pch=16,col=colors[as.numeric(as.factor(letter_set))][match(sampled_from_unit,strat_intervals)])
		# and label below
		axis(side=1,labels=c("unit name","sampled"),at=c(10,13),las=2)
			# and invite the user to label any dot they want.
			print("Click on any sample dot to show a label.")
				identify(y=y_position_samples,x=tally/2+12,labels=sample_name)


# We can also make a panel to show the sample names. Setting that aside for now, not super pretty.
	# plot(y=y_position_samples,x=seq(from=0,to=max(tally),length.out=samples_n),type="n",axes=F,xlab="sampled horizons",ylab=NA,main="samples",mar=c(5,0,5,0),ylim=c(0,max(unit_top_height)),oma=c(1,0,1,0))
	# text(sample_name,y=y_position_samples,x=tally,col=colors[as.numeric(as.factor(letter_set))],cex=0.8)
	# segments(x0=0,x1=10,y0=unit_base_height[!duplicated(letter_set)],y1=unit_base_height[!duplicated(letter_set)],lty=3,col=colors,lwd=3)

# Add a panel to show results of thin section survey.
par(mar=c(10,0,3,0))
plot(x=14,y=max(unit_top_height),type="n",axes=F,main="Thin Section Observations",ylim=c(0,max(unit_top_height)),xlim=c(0,20),ylab="",xlab="")
# Show where we have made thin sections
points(y=y_position_thin_sections,x=rep(1,thin_sections_n),pch=16)
	# and where we have bivalves
	points(y=y_position_thin_sections[bivalves==T],x=rep(4,sum(bivalves==T)),pch=16,col="blue")
	# gastropods
	points(y=y_position_thin_sections[gastropods==T],x=rep(6,sum(gastropods==T)),pch=16,col="cyan")
	# brachiopods
	points(y=y_position_thin_sections[brachiopods==T],x=rep(8,sum(brachiopods==T)),pch=16,col="red")
	# bryozoans
	points(y=y_position_thin_sections[bryozoans==T],x=rep(10,sum(bryozoans==T)),pch=16,col="orange")
	# spicules
	points(y=y_position_thin_sections[spicules==T],x=rep(12,sum(spicules==T)),pch=16,col="purple")
# extend the dashed lines for context
segments(x0=0,x1=14,y0=unit_base_height[!duplicated(letter_set)],y1=unit_base_height[!duplicated(letter_set)],lty=3,col=colors,lwd=2)
# and label
axis(side=1,labels=c("thin section","bivalves","gastropods","brachiopods","bryozoans","sponge spicules"),at=c(1,4,6,8,10,12),col=c("black","blue","cyan","red","orange","purple"),las=2)
