#!/usr/bin/env bash
#
# A handy script to run the Common Lisp example programs in this directory
#
# $1 should be the name of a CL (.lisp) file in this directory which the user
# wants to run.
#
# Example usage:
#   $ ./run.sh 03-asterisks.lisp

# Declare your common lisp executable and any parameters
CLEXE="/usr/bin/sbcl"
CLPARAMS="--script"

# You shouldn't have to alter anything below here
###############################################################################

# check if the example file specified exists
# if not, exit with error message
if [[ $1 == "" ]]; then
	echo "I need the name of the example file you want to try."
	exit 1
elif [ ! -f "$1" ]; then
    echo "File '$1' does not exist or is not a regular file."
    exit 1
fi
# Finally, just to be picayune, check if the file has a .lisp extension: Don't
# be trying to run anything else
extension="${1##*.}"
if [[ "$extension" != "lisp" ]]; then
    echo "File '$1' is not a .lisp file."
    exit 1
fi

# If we got this far, we should have a valid .lisp file to run the example

# Execute the example script
echo ">> running..."
$CLEXE $CLPARAMS "${1}"

# Get the exit status of the previous run
RUN_RESULT="$?";

# Communicate back how it went
if [[ $RUN_RESULT == 0 ]]; then
	echo -e "\n>> Done!\n>> Return value: $RUN_RESULT"
else
	echo -e "\n>>Run failed with error: $RUN_RESULT"
fi
