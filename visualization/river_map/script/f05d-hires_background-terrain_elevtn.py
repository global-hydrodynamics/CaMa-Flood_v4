#!/usr/bin/env python3
"""Draw p01-f05d with the pale-terrain elevation background and black rivers."""

import sys

from f05_river_background import parse_args, render


if __name__ == "__main__":
    sys.argv.extend(("--option", "terrain_elevtn"))
    render(parse_args())
