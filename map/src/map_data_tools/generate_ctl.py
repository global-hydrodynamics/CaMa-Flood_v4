"""Generate GrADS CTL files for category 11 core river-network maps."""
from __future__ import annotations

import argparse
import os
from pathlib import Path

try:
    from .map_metadata import (
        MapDataError, check_overwrite, dataset_file, default_registry_path,
        read_core_specs, read_dataset_config, read_params, read_registry,
        validate_binary_sizes, validate_missing_value,
    )
except ImportError:  # direct script execution
    from map_metadata import (
        MapDataError, check_overwrite, dataset_file, default_registry_path,
        read_core_specs, read_dataset_config, read_params, read_registry,
        validate_binary_sizes, validate_missing_value,
    )


def ctl_text(filename: str, entries: list[dict[str, str]], map_dir: Path,
             output_dir: Path, params: dict, config: dict) -> str:
    data_path = dataset_file(map_dir, config, filename)
    relative = Path(os.path.relpath(data_path, output_dir)).as_posix()
    nx, ny = int(params["NX"]), int(params["NY"])
    dx = (float(params["EAST"]) - float(params["WEST"])) / nx
    dy = (float(params["NORTH"]) - float(params["SOUTH"])) / ny
    x0 = float(params["WEST"]) + dx / 2
    y0 = float(params["SOUTH"]) + dy / 2
    sentinel = validate_missing_value(filename, entries[0]["dtype"], config)
    endian = "little_endian" if config["byte_order"] == "little" else "big_endian"
    options = [endian]
    if config["row_order"] == "north_to_south":
        options.insert(0, "yrev")
    lines = [
        f"dset ^{relative}", f"undef {sentinel:g}",
        f"title CaMa-Flood map dataset {config['dataset_id']}",
        "options " + " ".join(options),
        f"xdef {nx} linear {x0:.12g} {dx:.12g}",
        f"ydef {ny} linear {y0:.12g} {dy:.12g}",
        "tdef 1 linear 00Z01jan2000 1yr",
    ]
    if filename == "fldhgt.bin":
        lines.append(f"zdef {int(params['NLFP'])} linear 1 1")
    else:
        lines.append("zdef 1 linear 1 1")
    lines.append(f"vars {len(entries)}")
    for row in sorted(entries, key=lambda item: int(item["record_index"])):
        levels = int(params["NLFP"]) if filename == "fldhgt.bin" else 1
        type_code = "-1,40,4,-1" if row["dtype"] == "int32" else "99"
        description = row["long_name"].replace("\n", " ").replace("\r", " ")
        if row["units"].strip():
            description += f" [{row['units'].strip()}]"
        lines.append(f"{row['name']} {levels} {type_code} {description}")
    lines.append("ENDVARS")
    return "\n".join(lines) + "\n"


def generate(map_dir: Path, output_dir: Path, registry: Path | None,
             config_path: Path | None, overwrite: bool) -> list[Path]:
    map_dir = map_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    config = read_dataset_config(config_path, map_dir)
    params = read_params(map_dir)
    groups = read_core_specs(read_registry(registry))
    outputs = [output_dir / (Path(name).stem + ".ctl") for name in groups]
    check_overwrite(outputs, overwrite)
    # Validate every data input before creating any output file.
    for filename, entries in groups.items():
        validate_missing_value(filename, entries[0]["dtype"], config)
    validate_binary_sizes(map_dir, groups, params, config)
    texts = [ctl_text(filename, entries, map_dir, output_dir, params, config)
             for filename, entries in groups.items()]
    for path, text in zip(outputs, texts):
        path.write_text(text, encoding="ascii")
    return outputs


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--map-dir", type=Path, required=True, help="map directory containing params.txt and category 11 files")
    parser.add_argument("--config", type=Path, help="dataset JSON (default: <map-dir>/map_metadata.json)")
    parser.add_argument("--metadata-csv", type=Path, default=default_registry_path())
    parser.add_argument("--output-dir", type=Path, help="where CTLs are written (default: map directory)")
    parser.add_argument("--overwrite", action="store_true", help="replace existing CTL files")
    args = parser.parse_args(argv)
    try:
        outputs = generate(args.map_dir, args.output_dir or args.map_dir, args.metadata_csv, args.config, args.overwrite)
    except (MapDataError, OSError) as exc:
        parser.error(str(exc))
    for path in outputs:
        print(f"wrote {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
