#!/usr/bin/env python3
"""Assemble the carob to-do site into `_site/` and serve it locally.

Usage:
    python3 docs/_build/serve.py [PORT]
"""
from __future__ import annotations

import http.server
import shutil
import sys
import time
from functools import partial
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SITE = ROOT / "_site"
TODO_CSV = ROOT / "misc" / "todo" / "to-do.csv"


def _clean_site(retries: int = 5, delay: float = 0.3) -> None:
	SITE.mkdir(exist_ok=True)
	last_err: Exception | None = None
	for _attempt in range(retries):
		last_err = None
		for entry in list(SITE.iterdir()):
			try:
				if entry.is_dir() and not entry.is_symlink():
					shutil.rmtree(entry)
				else:
					entry.unlink()
			except PermissionError as e:
				last_err = e
		if last_err is None:
			return
		time.sleep(delay)
	raise SystemExit(
		"serve: could not clear _site/ (close any open handles and retry).\n"
		f"Last error: {last_err}"
	)


def assemble() -> None:
	_clean_site()

	dest_csv = SITE / "misc" / "todo" / "to-do.csv"
	dest_csv.parent.mkdir(parents=True, exist_ok=True)
	if not TODO_CSV.is_file():
		raise SystemExit(f"serve: missing {TODO_CSV}")
	shutil.copy2(TODO_CSV, dest_csv)

	docs = ROOT / "docs"
	for entry in docs.iterdir():
		if entry.name == "_build":
			continue
		dest = SITE / entry.name
		if entry.is_dir():
			shutil.copytree(entry, dest)
		else:
			shutil.copy2(entry, dest)

	(SITE / ".nojekyll").touch()
	(SITE / "config.js").write_text(
		'window.SITE_CONFIG = { repo: "carob-data/carob", branch: "main" };\n',
		encoding="utf-8",
	)

	sys.path.insert(0, str(ROOT / "docs" / "_build"))
	import manifest  # type: ignore
	manifest.main(["manifest.py", str(SITE)])


def serve(port: int) -> None:
	handler = partial(http.server.SimpleHTTPRequestHandler, directory=str(SITE))
	with http.server.ThreadingHTTPServer(("", port), handler) as httpd:
		print(f"carob to-do preview at http://localhost:{port}/  (Ctrl+C to stop)")
		try:
			httpd.serve_forever()
		except KeyboardInterrupt:
			print("\nbye.")


def main(argv: list[str]) -> int:
	port = int(argv[1]) if len(argv) > 1 else 8000
	assemble()
	serve(port)
	return 0


if __name__ == "__main__":
	raise SystemExit(main(sys.argv))
