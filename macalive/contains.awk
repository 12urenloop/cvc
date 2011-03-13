BEGIN {
	exitval = 1
}

{
	if ($1 == mac) {
		exitval = 0
	}
}

END {
	exit exitval
}
