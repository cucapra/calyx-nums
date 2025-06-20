import json
import re
import sys


def dat2json(dat: str, log: str, out: str):
    with open(dat, 'r') as f:
        results = [int(line, 16) for line in f if not line.startswith('//')]

    with open(log, 'r') as f:
        log = f.read()

    if match := re.search(r'min=(\d+) max=(\d+) total=(\d+)', log):
        best, worst, total = match.groups()

        data = {
            'output': results,
            'best_latency': int(best),
            'worst_latency': int(worst),
            'total_latency': int(total),
        }
    else:
        raise RuntimeError("couldn't parse latency")

    with open(out, 'w') as f:
        json.dump(data, f)
        f.write('\n')


if __name__ == '__main__':
    dat2json(*sys.argv[1:])
