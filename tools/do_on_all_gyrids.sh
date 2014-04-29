
for i in 1 2 3 4 5; do
    ssh root@gyrid$i "$1" &
done

