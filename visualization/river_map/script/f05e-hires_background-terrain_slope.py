#!/usr/bin/env python3
"""Draw p01-f05e with the pale-terrain slope background and black rivers."""

import sys

from f05_river_background import parse_args, render


if __name__ == "__main__":
    sys.argv.extend(("--option", "terrain_slope"))
    render(parse_args())
