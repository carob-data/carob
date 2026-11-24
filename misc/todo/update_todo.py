#!/usr/bin/env python3
"""Drop datasets from misc/todo/to-do.csv once a script exists or a pull request has one.

Same rule as carobiner::update_todo: a row goes when its dataset id is the file name of a
script under scripts/ that is not in a holding folder, or of any script file in an open
pull request. Difficulty is left as it is; that still comes from a local update_todo run.

The kept lines are written back unchanged.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import re
import sys
import urllib.request

CSV_PATH = "misc/todo/to-do.csv"
HOLDING = re.compile(r"(^|/)_(draft|todo|pending|AI)(/|$)")
SCRIPT_FILE = re.compile(r"(?i)^scripts/.+\.R$")
FUNCTIONS = re.compile(r"(?i)_functions\.R$")
DONE_NAME = re.compile(r"(?i)^(doi|hdl).*\.R$")


def simple_id(uri: str) -> str:
	"""Match yuri::simpleURI after the same doi/hdl cleanup update_todo uses."""
	u = (uri or "").strip().lower()
	u = u.replace("https://doi.org/", "doi:")
	u = u.replace("https://hdl.handle.net/", "hdl:")
	u = re.sub(r"^https?://", "", u)
	u = re.sub(r"^www\.", "", u)
	if "dx.doi.org/" in u:
		s = "doi_" + u.split("dx.doi.org/", 1)[1]
	elif "doi.org/" in u:
		s = "doi_" + u.split("doi.org/", 1)[1]
	elif "persistentid=doi:" in u:
		s = "doi_" + u.split("persistentid=doi:", 1)[1]
	elif u.startswith("doi:"):
		s = "doi_" + u[4:]
	elif "persistentid=hdl:" in u:
		s = "hdl_" + u.split("persistentid=hdl:", 1)[1]
	elif u.startswith("hdl:"):
		s = "hdl_" + u[4:]
	elif "hdl.handle.net/" in u:
		s = "hdl_" + u.split("hdl.handle.net/", 1)[1]
	else:
		return ""
	return s.replace("/", "_").replace(" ", "")


def file_id(path: str) -> str:
	name = os.path.basename(path.replace("\\", "/"))
	name = re.sub(r"(?i)\.r$", "", name)
	return name.lower().replace(" ", "")


def pr_script_ids(repo: str, number: int, token: str) -> set[str]:
	ids: set[str] = set()
	page = 1
	while True:
		url = (
			f"https://api.github.com/repos/{repo}/pulls/{number}/files"
			f"?per_page=100&page={page}"
		)
		req = urllib.request.Request(
			url,
			headers={
				"Accept": "application/vnd.github+json",
				"Authorization": f"Bearer {token}",
				"X-GitHub-Api-Version": "2022-11-28",
				"User-Agent": "carob-todo-list",
			},
		)
		with urllib.request.urlopen(req) as resp:
			batch = json.load(resp)
		if not batch:
			break
		for item in batch:
			path = item.get("filename") or ""
			if SCRIPT_FILE.search(path) and not FUNCTIONS.search(path):
				ids.add(file_id(path))
		if len(batch) < 100:
			break
		page += 1
	return ids


def done_ids(scripts_root: str) -> set[str]:
	ids: set[str] = set()
	if not os.path.isdir(scripts_root):
		return ids
	for dirpath, dirnames, filenames in os.walk(scripts_root):
		rel = os.path.relpath(dirpath, scripts_root).replace("\\", "/")
		if rel == ".":
			rel = ""
		dirnames[:] = [
			d for d in dirnames if not HOLDING.search(f"{rel}/{d}" if rel else d)
		]
		if rel and HOLDING.search(rel):
			continue
		for name in filenames:
			if DONE_NAME.search(name):
				ids.add(file_id(name))
	return ids


def drop_rows(text: str, claimed: set[str]) -> tuple[str, list[str]]:
	ends_nl = text.endswith("\n")
	lines = text.splitlines()
	if not lines:
		return text, []
	rows = list(csv.reader(lines))
	header = rows[0]
	try:
		uri_col = [h.strip() for h in header].index("uri")
	except ValueError as e:
		raise SystemExit(f"{CSV_PATH} has no uri column") from e
	kept = [lines[0]]
	removed: list[str] = []
	for line, row in zip(lines[1:], rows[1:]):
		uri = row[uri_col] if uri_col < len(row) else ""
		did = simple_id(uri)
		if did and did in claimed:
			removed.append(uri.strip())
			continue
		kept.append(line)
	out = "\n".join(kept)
	if ends_nl:
		out += "\n"
	return out, removed


def main() -> None:
	parser = argparse.ArgumentParser(description=__doc__)
	parser.add_argument("--csv", default=CSV_PATH)
	parser.add_argument("--scripts", default="scripts")
	parser.add_argument("--repo", default=os.environ.get("REPO", ""))
	parser.add_argument("--pr", type=int, default=0)
	parser.add_argument("--id", action="append", default=[], help="extra dataset id, for a dry run")
	parser.add_argument("--dry-run", action="store_true")
	args = parser.parse_args()

	pr_number = args.pr or int(os.environ.get("PR_NUMBER") or "0")
	claimed = done_ids(args.scripts)
	claimed.update(simple_id(i) or i.strip().lower().replace(" ", "") for i in args.id)

	if pr_number:
		token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN") or ""
		repo = args.repo
		if not token or not repo:
			raise SystemExit("A pull request update needs GH_TOKEN and REPO")
		ids = pr_script_ids(repo, pr_number, token)
		print(f"pull request {pr_number}: {len(ids)} script file(s)")
		claimed.update(ids)

	with open(args.csv, encoding="utf-8") as f:
		text = f.read()
	updated, removed = drop_rows(text, claimed)
	if not removed:
		print("to-do list unchanged")
		return
	for uri in removed:
		print(f"remove {uri}")
	if args.dry_run:
		print(f"dry run: {len(removed)} row(s) would be removed")
		return
	with open(args.csv, "w", encoding="utf-8", newline="\n") as f:
		f.write(updated)
	print(f"removed {len(removed)} row(s)")


if __name__ == "__main__":
	main()
