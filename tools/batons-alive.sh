#!/bin/bash
#         _  __                 _   _     ____
#        |"|/ /        ___     | \ |"| U /"___|u
#        | ' /        |_"_|   <|  \| |>\| |  _ /
#      U/| . \\u       | |    U| |\  |u | |_| |
#        |_|\_\      U/| |\u   |_| \_|   \____|
#      ,-,>> \\,-..-,_|___|_,-.||   \\,-._)(|_
#       \.)   (_/  \_)-' '-(_/ (_")  (_/(__)__)
#
#    U  ___ u  _____        ____    _   _   __  __
#     \/"_ \/ |" ___|    U /"___|U |"|u| |U|' \/ '|u
#     | | | |U| |_  u    \| | u   \| |\| |\| |\/| |/
# .-,_| |_| |\|  _|/      | |/__   | |_| | | |  | |
#  \_)-\___/  |_|          \____| <<\___/  |_|  |_|
#       \\    )(\\,-      _// \\ (__) )(  <<,-,,-.
#      (__)  (__)(_/     (__)(__)    (__)  (./  \.)
set -o nounset -o errexit -o pipefail

# Find file containing mac addresses
MACS_FILE=""
if [[ -f "macs.csv" ]]; then
    MACS_FILE="macs.csv"
elif [[ -f "../doc/macs.csv" ]]; then
    MACS_FILE="../doc/macs.csv"
fi

# Error if not found
if [[ "$MACS_FILE" = "" ]]; then
    echo "macs.csv was not found"
    exit 1
fi

# Scan for the mac address
HCITOOL_OUT=$(mktemp)
hcitool inq --length=5 > "$HCITOOL_OUT"
while read LINE; do
    BATON_MAC=$(echo "$LINE" | sed 's/^.*,//')
    BATON_NAME=$(echo "$LINE" | sed 's/,.*$//')
    GREP_OUT=$(grep "$BATON_MAC" "$HCITOOL_OUT" || true)
    if [[ "$GREP_OUT" = "" ]]; then
        echo "Baton $BATON_NAME ($BATON_MAC): down!"
    else
        echo "Baton $BATON_NAME ($BATON_MAC): ok."
    fi
done < "$MACS_FILE"
rm "$HCITOOL_OUT"
