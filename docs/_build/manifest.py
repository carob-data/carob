#!/usr/bin/env python3
"""Generate `tables.json` for the carob to-do GitHub Pages site.

Usage:
    python3 docs/_build/manifest.py <site_root>
"""
from __future__ import annotations

import csv
import json
import os
import sys

TODO_CSV = "misc/todo/to-do.csv"


def _info(path: str) -> tuple[list[str], int]:
	try:
		with open(path, newline="", encoding="utf-8") as f:
			reader = csv.reader(f)
			header = next(reader, [])
			data_rows = list(reader)
	except UnicodeDecodeError as e:
		raise SystemExit(
			f"manifest: {path}: not valid UTF-8 at byte {e.start}: "
			f"{e.reason}. Re-save the file as UTF-8 (no BOM)."
		) from e
	except Exception as e:
		raise SystemExit(f"manifest: {path}: cannot read: {e}") from e

	keep = []
	for i, h in enumerate(header):
		if (h or "").strip():
			keep.append(True)
			continue
		col_nonempty = any(
			(row[i] if i < len(row) else "").strip() for row in data_rows
		)
		keep.append(col_nonempty)
	header_visible = [h for h, k in zip(header, keep) if k]
	return header_visible, len(data_rows)


def build(site: str) -> list[dict]:
	path = os.path.join(site, TODO_CSV)
	if not os.path.isfile(path):
		raise SystemExit(f"manifest: missing {TODO_CSV} under {site}")
	header, n = _info(path)
	return [{
		"group": "todo",
		"name": "to-do",
		"displayName": "Dataset to-do list",
		"file": TODO_CSV,
		"rows": n,
		"cols": len(header),
	}]


def main(argv: list[str]) -> int:
	site = argv[1] if len(argv) > 1 else "_site"
	if not os.path.isdir(site):
		print(f"manifest: site root '{site}' does not exist", file=sys.stderr)
		return 1
	tables = build(site)
	out = os.path.join(site, "tables.json")
	with open(out, "w", encoding="utf-8") as f:
		json.dump(tables, f, indent=2)
		f.write("\n")
	print(f"manifest: wrote {len(tables)} table(s) to {out}")
	return 0


if __name__ == "__main__":
	raise SystemExit(main(sys.argv))
