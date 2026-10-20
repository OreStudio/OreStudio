#!/bin/sh
# Capacity of the marketdata ingest: restart it, inject a burst, time the drain.
# usage: capacity.sh LABEL [TICKS] [RATE]
label=${1:-run}
ticks=${2:-3000}
rate=${3:-1000}
export KEYS=$PWD/build/keys/nats
q() { ./compass.sh sql -- -t -A -c "$1"; }
probe="select count(*) from ores_marketdata_market_observations_tbl where observation_datetime between now() - interval '3 days' and now() and value::numeric = 999"

./compass.sh services restart ores.marketdata.service
sleep 45
echo "[$label] settled, probe rows before: $(q "$probe")"
before=$(q "$probe")
secs=$((ticks / rate + 1))
./tmp/tickprobe/v/bin/python -u tmp/tickprobe/inject.py "$rate" "$secs" 2>&1 | grep sent=
start=$(date +%s)
expected=$((before + ticks * 10))
while :; do
    now=$(date +%s)
    got=$(q "$probe")
    echo "[$label] t=$((now - start))s probe_rows=$((got - before)) of $((ticks * 10))"
    [ "$got" -ge "$expected" ] && break
    [ $((now - start)) -ge 300 ] && break
    sleep 10
done
