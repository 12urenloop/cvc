
for i in 3 5 6; do
    ssh zeus@10.0.20.$i "'echo -e \"CVC_SERVER=$1\nCVC_PORT=2583\"'" &
done

