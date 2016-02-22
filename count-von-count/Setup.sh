PIPE="INPUTFIFO"
if [[ ! -p $PIPE ]]; then 
  mkfifo $PIPE
fi
cat $PIPE | jq '{who, when}' &
cat $PIPE | jq '{where, when}' &
