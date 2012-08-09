mat2tex <- function(m, vseps=FALSE, hseps=FALSE) {
	# Parameters ok?
	if(!is.matrix(m)) {
		error("m must be a matrix")
	}
	vseps = as.vector(vseps)
	hseps = as.vector(hseps)
	
	height = dim(m)[1]
	width = dim(m)[2]

	# Table header
	cat("\\begin{tabular}{")

	# Extra column for row names?
	rows = rownames(m)
	if(length(rows) > 0) {
		useRowNames = TRUE
		cat("r|")
	} else {
		useRowNames = FALSE
	}

	# Line before first value?
	if((!useRowNames && isTRUE(vseps)) || (!is.logical(vseps) && any(vseps == 0))) {
		cat("|")
	}
	
	# Space for each column
	for(i in 1:width) {
		cat("c")
		
		if(isTRUE(vseps) || (!is.logical(vseps) && any(vseps == i))) {
			cat("|")
		}
	}

	cat("}\n")

	# Header row, if applicable
	cols = colnames(m)
	if(length(cols) > 0) {
		useColNames = TRUE

		if(useRowNames) {
			cat("\t\t")
		} else {
			cat("\t")
		}
		
		# Add extra column for row names?
		if(useRowNames)
			cat("& ")

		for(i in 1:length(cols)) {
			cat(cols[i])

			# Not the last one, right?
			if(i < length(cols)) {
				cat(" & ")
			}
		}

		cat("\\\\\n\t\\hline\n")
	} else {
		useColNames = FALSE
	}


	# Horizontal line before first row?
	if((!useColNames && isTRUE(hseps)) || (!is.logical(hseps) && any(hseps == 0))) {
		cat("\t\\hline\n")
	}
	
	# Each row in the matrix!
	for(r in 1:height) {
		cat("\t")

		# Row name?
		if(useRowNames) {
			cat(rows[r])
			cat(" & ")
		}

		# Values
		for(i in 1:width) {
			v = m[r, i]
			if(!is.na(v))
				cat(v)

			# Not the last one, right?
			if(i < width) {
				cat(" & ")
			}
		}

		cat("\\\\\n")
		
		# Horizontal line?
		if(isTRUE(hseps) || (!is.logical(hseps) && any(hseps == r))) {
			cat("\t\\hline\n")
		}
	}

	# End table
	cat("\\end{tabular}");
}
