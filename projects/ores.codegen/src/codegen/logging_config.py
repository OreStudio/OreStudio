import logging
import sys


def configure(verbose: bool = False) -> None:
    logging.basicConfig(
        stream=sys.stdout,
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(levelname)s %(message)s",
    )
