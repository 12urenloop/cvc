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

# Check arguments
BATON="$1"
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <baton nr>"
    exit 1
fi

# Find file containing mac addresses
MACS_FILE=""
if [[ -f "macs.csv" ]]; then
    MACS_FILE="macs.csv"
elif [[ -f "../macs.csv" ]]; then
    MACS_FILE="../macs.csv"
fi

# Error if not found
if [[ "$MACS_FILE" = "" ]]; then
    echo "macs.csv was not found"
    exit 1
fi

# Find mac address
MAC=$(grep ",$BATON$" "$MACS_FILE" | sed 's/,.*$//')

# Error if not found
if [[ "$MAC" = "" ]]; then
    echo "Unknown baton: MAC-adress for baton $BATON not found"
    exit 1
fi

# Scan for the mac address
LINE=$(hcitool inq --length=3 | grep "$MAC")
if [[ "$LINE" = "" ]]; then
    echo "Baton $BATON ($MAC): $(tput bold)$(tput setaf 1)down!$(tput sgr0)"
else
    echo "Baton $BATON ($MAC): $(tput bold)$(tput setaf 2)ok.(tput sgr0)"
fi
