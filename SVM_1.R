
transform_factor <- function(ind, sw) {
	if (ind >= 15) test_ind <- ind-1 else test_ind <- ind
	# combining data
	combined <- rbind(cbind(train[,ind], rep(1,nrow(train))), cbind(test[,test_ind], rep(2,nrow(test))))
	
	# NA process
	if(any(is.na(combined[,1]))) {
		combined[which(is.na(combined[,1])),1] <- "NULL"
	}
	
	# combine level
	factored <- factor(combined[,1])
	if(sw == 1){	
		p <- 0.01
		lf <- names(which(prop.table(table(factored)) < p))
		levels(factored)[levels(factored) %in% lf] <- "Other"	
	} 
	train[,ind] <<- factored[combined[,2]==1]
	test[,test_ind] <<- factored[combined[,2]==2]
}

sample_inbalance_binary <- function(training_set) {
	if(length(levels(factor(training_set[,1])))==1 || mean(training_set[,1]==1)==0) {
		return (training_set)
	}
	
	binary_amplified = do.call("rbind", replicate(round(1.0/mean(training_set[,1]==1),0)-1, training_set[training_set[,1]==1,], simplify = FALSE))
	return( rbind(training_set, binary_amplified))
}


###########################################################################
## main()
library(kernlab)

submission_filename <- "/data/Expedia2013/submission/T5_SVM_2.1.csv"

write.table(c("SearchId,PropertyId"), file = submission_filename, sep = ",", col.names = FALSE, quote = FALSE, append = FALSE, row.names = FALSE)

#train_files <- list.files(path = "/data/Expedia2013/data/by_destid", pattern = 'train_.*.csv', all.files = FALSE, full.names = TRUE, ignore.case =TRUE, include.dirs = FALSE)
train_files <- list.files(path = "/data/Expedia2013/data/by_destid", pattern = 'train_[1-4].*.csv', all.files = FALSE, full.names = TRUE, ignore.case =TRUE, include.dirs = FALSE)
#train_files <- list.files(path = "/data/Expedia2013/data/by_destid", pattern = 'train_[5-9].*.csv', all.files = FALSE, full.names = TRUE, ignore.case =TRUE, include.dirs = FALSE)

for (i in 1:length(train_files)) {
## i<-1
    train_filename <- train_files[i]
	test_filename <- gsub("train", "test", train_filename)
	
	# if testing file not exist?!
	if (!file.exists(test_filename)) next		
	
	cat("train_filename: ", train_filename, "\n")
	cat("test_filename: ", test_filename, "\n")
	train<-read.table(train_filename, sep=",", header=TRUE, na.strings="NULL")
	test<-read.table(test_filename, sep=",", header=TRUE, na.strings="NULL")
	test_srch_id <- test$srch_id
	test_prop_id <- test$prop_id

	
##1:srch_id

##2:date_time
removing = c(2)

## Filter - the last step to prevent data corrupt	
for(i in 1:ncol(train))
{
	if(mean(is.na(train[,i])) > 0.7) {
		removing <- c(removing, i)
	}
}
	
##3:site_id
transform_factor(3,0)


##4:visitor_location_country_id
transform_factor(4,0)


##5:visitor_hist_starrating
removing = c(removing, 5)


##6:visitor_hist_adr_usd
removing = c(removing, 6)


##7:prop_country_id
removing = c(removing, 7)


##8:prop_id
transform_factor(8,0)


##9:prop_starrating
train[,9] <- train[,9]/5.0
test[,9] <- test[,9]/5.0


##10:prop_review_score
train[is.na(train[,10]),10] <- 0
test[is.na(test[,10]),10] <- 0
train[,10] <- train[,10]/5.0
test[,10] <- test[,10]/5.0


##11:prop_brand_bool
transform_factor(11,0)


##12:prop_location_score1
train[,12] <- train[,12]/7.0
test[,12] <- test[,12]/7.0


##13:prop_location_score2
train[is.na(train[,13]),13] <- 0
test[is.na(test[,13]),13] <- 0


##14:prop_log_historical_price

##15:position
## removing = c(removing, 15)


##16:price_usd
train[,16] <- log10(train[,16])/train[,14]
train[!is.numeric(train[,16]),16] <- 0.0
train[is.infinite(train[,16]),16] <- 0.0

test[,15] <- log10(test[,15])/test[,14]
test[!is.numeric(test[,15]),15] <- 0.0
test[is.infinite(test[,15]),15] <- 0.0

train[,14] <- train[,14] /6.21
test[,14] <- test[,14] /6.21


##17:promotion_flag
transform_factor(17,0)


##18:srch_destination_id
removing = c(removing, 18)


##19:srch_length_of_stay
train[train[,19] >= 7,19] <- 7
test[test[,18] >= 7,18] <- 7
train[,19] <- train[,19] /7.0
test[,18] <- test[,18] /7.0


##20:srch_booking_window
train[train[,20] >= 90,20] <- 90
test[test[,19] >= 90,19] <- 90
train[,20] <- train[,20] /90.0
test[,19] <- test[,19] /90.0


##21:srch_adults_count
train[train[,21] >= 4,21] <- 4
test[test[,20] >= 4,20] <- 4
train[,21] <- train[,21] /4.0
test[,20] <- test[,20] /4.0


##22:srch_children_count
test[test[,21] >= 1,21] <- 1
transform_factor(22,0)



##23:srch_room_count
train[train[,23] >= 2,23] <- 2
test[test[,22] >= 2,22] <- 2
transform_factor(23,0)


##24:srch_saturday_night_bool
transform_factor(24,0)


##25:srch_query_affinity_score
train[,25] <- (2 ^ train[,25]) /0.1776
test[,24] <- (2 ^ test[,24]) /0.1776
train[is.na(train[,25]),25] <- 0
test[is.na(test[,24]),24] <- 0

##26:orig_destination_distance
train[,26] <- ifelse(train[,26]<1, 0.0, log10(train[,26]))/4.07
test[,25] <- ifelse(test[,25]<1, 0.0, log10(test[,25]))/4.07
train[is.na(train[,26]),26] <- 0
test[is.na(test[,25]),25] <- 0

##27:random_bool
transform_factor(27,0)


##-------------------------------------------------------
##28:comp1_rate
#mean(is.na(train[28]))
#[1] 0.9762233
removing = c(removing, 28)


##29:comp1_inv
#mean(is.na(train[29]))
#[1] 0.9743451
removing = c(removing, 29)


##30:comp1_rate_percent_diff
#mean(is.na(train[30]))
#[1] 0.9813555
removing = c(removing, 30)


##31:comp2_rate
#mean(is.na(train[31]))
#[1] 0.5925767
transform_factor(31,0)


##32:comp2_inv
#mean(is.na(train[32]))
#[1] 0.5713108
transform_factor(32,0)


##33:comp2_rate_percent_diff
#mean(is.na(train[33]))
#[1] 0.8880924
removing = c(removing, 33)


##34:comp3_rate
#mean(is.na(train[34]))
#0.6915
transform_factor(34,0)


##35:comp3_inv
#mean(is.na(train[35]))
#0.6680
transform_factor(35,0)


##36:comp3_rate_percent_diff
#mean(is.na(train[36]))
#0.9048
removing = c(removing, 36)


##37:comp4_rate
#mean(is.na(train[37]))
#0.9374
removing = c(removing, 37)


##38:comp4_inv
#mean(is.na(train[38]))
#0.9301
removing = c(removing, 38)


##39:comp4_rate_percent_diff
#mean(is.na(train[39]))
#0.9733
removing = c(removing, 39)


##40:comp5_rate
#mean(is.na(train[40]))
#0.5518
transform_factor(40,0)


##41:comp5_inv
#mean(is.na(train[41]))
#0.5239
transform_factor(41,0)


##42:comp5_rate_percent_diff
#mean(is.na(train[42]))
#0.8305
removing = c(removing, 42)


##43:comp6_rate
#mean(is.na(train[43]))
#0.9513
removing = c(removing, 43)


##44:comp6_inv
#mean(is.na(train[44]))
#0.94714
removing = c(removing, 44)


##45:comp6_rate_percent_diff
#mean(is.na(train[45]))
#0.98050
removing = c(removing, 45)


##46:comp7_rate
#mean(is.na(train[46]))
#0.936367
removing = c(removing, 46)


##47:comp7_inv
#mean(is.na(train[47]))
#0.92808
removing = c(removing, 47)


##48:comp7_rate_percent_diff
#mean(is.na(train[48]))
#0.97198
removing = c(removing, 48)


##49:comp8_rate
#mean(is.na(train[49]))
#0.61491
transform_factor(49,0)


##50:comp8_inv
#mean(is.na(train[50]))
#0.6006
transform_factor(50,0)


##51:comp8_rate_percent_diff
#mean(is.na(train[51]))
#0.8764
removing = c(removing, 51)

##-------------------------------------------------------
##52:click_bool
click_bool <- as.factor(train[,52])


##53:gross_bookings_usd
removing = c(removing, 53)


##54:booking_bool
booking_bool <- as.factor(train[,54])


###########################################################################
	# for test data
	removing_test <- ifelse(removing < 14 , removing, removing-1 )
	removing_train <- unique(sort(c(removing, 1, 15, 52, 54)))
	removing_test <- unique(removing_test)
	
	train <- train[,-removing_train]
	test <- test[,-removing_test]

	###########################################################################
	## Make the training set
	training_booking <- cbind(booking_bool, train)
	training_click <- cbind(click_bool, train)
		
	training_set_booking <- sample_inbalance_binary(training_booking)
	training_set_click <- sample_inbalance_binary(training_click)

	if(nrow(training_set_booking) < 100) {
		fold <-round(100.0/nrow(training_set_booking),0)
		fold <- ifelse(fold<2, 2, fold) 
		training_set_booking = do.call("rbind", replicate(fold, training_set_booking, simplify = FALSE))
		training_set_click = do.call("rbind", replicate(fold, training_set_click, simplify = FALSE))
	}
		
	print(summary(training_set_booking))
	print(summary(training_set_click))
	
	###########################################################################
	## Make the Training Model
	set.seed(8660)
	## SVM
	prob <- NULL
	##if (mean(training_set_booking[,1]==1) > 0.1 ) {
	if ( length(levels(factor(training_set_booking[,1])))!=1 ) {
		model_svm_booking <- ksvm(data.matrix(training_set_booking[,-1]), training_set_booking[,1], type="nu-svc", kernel='laplacedot', cross=5, prob.model = T)
		predict_svm_booking <- predict(model_svm_booking, data.matrix(test[,-1]), type="prob")
		prob <- predict_svm_booking[,2] * 5.0 
	}
		
	##if (mean(training_set_click[,1]!=0) > 0 ) {
	model_svm_click <- ksvm(data.matrix(training_set_click[,-1]), training_set_click[,1], type="nu-svc", kernel='laplacedot', cross=5, prob.model = T)
	predict_svm_click   <- predict(model_svm_click, data.matrix(test[,-1]), type="prob")
	if (is.null(prob)) {
		prob <- predict_svm_click[,2] * 1.0
	}
	else {
		prob <- prob + predict_svm_click[,2] * 1.0
	}
	##}	
	##if (is.null(prob)) next
	
	###########################################################################
	result <- cbind(test_srch_id, test_prop_id, prob)
	result <- result[order(result[,1], -result[,3]),-3]
	options(scipen=10)
	write.table(result, file = submission_filename, sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, row.names = FALSE)
}
