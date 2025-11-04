import json
import re
import sys


def extract(rpt: str, out: str):
    with open(rpt, 'r') as f:
        rpt = f.read()

    if (
        (best := re.search(r'MIN_LATENCY = "(\d+)"', rpt))
        and (worst := re.search(r'MAX_LATENCY = "(\d+)"', rpt))
        and (avg := re.search(r'AVER_LATENCY = "(\d+)"', rpt))
    ):
        data = {
            'best_latency': int(best.group(1)),
            'worst_latency': int(worst.group(1)),
            'avg_latency': int(avg.group(1)),
        }
    else:
        raise RuntimeError("couldn't parse latency")

    with open(out, 'w') as f:
        json.dump(data, f, indent=2)
        f.write('\n')


if __name__ == '__main__':
    extract(*sys.argv[1:])
