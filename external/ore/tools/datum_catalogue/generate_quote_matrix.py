#!/usr/bin/env python3
"""Print every key form ORE accepts with each quote token in turn.

Each accepted line of forms.jsonl gives one key. The key is printed once for
each token in quote_types.txt, with its second token replaced, plus HAZARD_RATE,
the one QuoteType member ORE's parser gives no token. Most of ORE's parser cases
never check the quote type, so this is how the catalogue learns which quote
types each form admits.

Usage: generate_quote_matrix.py <forms.jsonl> <quote_types.txt>
"""

import json
import sys


def main():
    tokens = []
    with open(sys.argv[2]) as quote_types:
        for line in quote_types:
            tokens.extend(line.split()[1:])
    tokens.append("HAZARD_RATE")

    seen = set()
    with open(sys.argv[1]) as forms:
        for line in forms:
            form = json.loads(line)
            if form["accepted"] != "true":
                continue
            parts = form["key"].split("/")
            for token in tokens:
                key = "/".join([parts[0], token] + parts[2:])
                if key not in seen:
                    seen.add(key)
                    print(key)


if __name__ == "__main__":
    main()
