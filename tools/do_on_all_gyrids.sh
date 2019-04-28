
for i in 2 3 4 5 6; do
    ssh zeus@10.0.20.$i "$1" &
done

