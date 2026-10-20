import asyncio, datetime, os, ssl, sys, time
import msgpack
import nats

rate = float(sys.argv[1])
seconds = float(sys.argv[2])
source = sys.argv[3] if len(sys.argv) > 3 else "synthetic.bloomberg.usdtry"
uri = sys.argv[4] if len(sys.argv) > 4 else "oresmd://fx/USD?type=quote&instrument=fx_spot&quote=rate&ccy=TRY"
subject = "ores.dev.brave_hopper.synthetic.v1.ops.tick." + source


def tls():
    k = os.environ["KEYS"]
    ctx = ssl.create_default_context(cafile=k + "/ca.crt")
    ctx.check_hostname = False
    ctx.verify_flags &= ~ssl.VERIFY_X509_STRICT
    ctx.load_cert_chain(k + "/ores.shell.crt", k + "/ores.shell.key")
    return ctx


async def main():
    nc = await nats.connect("tls://localhost:20405", tls=tls())
    sent = 0
    start = time.perf_counter()
    interval = 1.0 / rate
    nxt = start
    while time.perf_counter() - start < seconds:
        now = datetime.datetime.now(datetime.timezone.utc)
        stamp = now.strftime("%Y-%m-%d %H:%M:%S.%f") + "Z"
        payload = msgpack.packb(
            {"oresmd_uri": uri, "value": "999.0", "observation_time": stamp, "source": source}
        )
        await nc.publish(subject, payload)
        sent += 1
        nxt += interval
        delay = nxt - time.perf_counter()
        if delay > 0:
            await asyncio.sleep(delay)
        elif sent % 200 == 0:
            await asyncio.sleep(0)
    await nc.flush()
    elapsed = time.perf_counter() - start
    print(f"sent={sent} in {elapsed:.1f}s = {sent/elapsed:.0f}/s on {source}")
    await nc.close()


asyncio.run(main())
