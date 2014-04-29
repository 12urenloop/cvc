
for i in 1 2 3 4 5; do
    scp "$1" root@gyrid$i:"'$2'"
done

