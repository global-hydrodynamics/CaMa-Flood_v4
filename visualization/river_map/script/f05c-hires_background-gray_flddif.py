#!/usr/bin/env python3
"""Draw p01-f05c with a gray floodplain elevation-difference background."""

import sys

from f05_river_background import parse_args, render


if __name__ == "__main__":
    sys.argv.extend(("--option", "gray_flddif"))
    render(parse_args())
