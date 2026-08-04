(function () {
	"use strict";

	const TABLE_FILE = "misc/todo/to-do.csv";

	const cfg = window.SITE_CONFIG || {};
	const repoUrl = cfg.repo ? `https://github.com/${cfg.repo}` : null;
	const rawBase = cfg.repo ? `https://raw.githubusercontent.com/${cfg.repo}/${cfg.branch || "main"}` : null;

	async function fetchTables() {
		const r = await fetch("tables.json", { cache: "no-cache" });
		if (!r.ok) throw new Error("cannot load tables.json (HTTP " + r.status + ")");
		return r.json();
	}

	function escapeHtml(s) {
		return String(s).replace(/[&<>"']/g, c => ({
			"&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;"
		}[c]));
	}

	function uriHref(text) {
		const t = String(text || "").trim();
		if (!t) return null;
		if (/^https?:\/\//i.test(t)) return t;
		if (/^doi:/i.test(t)) return "https://doi.org/" + t.replace(/^doi:\s*/i, "");
		if (/^hdl:/i.test(t)) return "https://hdl.handle.net/" + t.replace(/^hdl:\s*/i, "");
		return null;
	}

	function formatSource(author, year, provider) {
		author = String(author || "").trim().replace(/,\s*$/, "");
		year = String(year || "").trim();
		provider = String(provider || "").trim();
		let lead = "";
		if (author && year) lead = `${author} (${year})`;
		else if (author) lead = author;
		else if (year) lead = `(${year})`;
		if (lead && provider) return `${lead}, ${provider}`;
		return lead || provider;
	}

	function titleLinkCell(title, uri) {
		title = String(title || "").trim();
		uri = String(uri || "").trim();
		if (!title) return "";
		const href = uriHref(uri);
		if (!href) return escapeHtml(title);
		return gridjs.html(`<a href="${escapeHtml(href)}" target="_blank" rel="noopener">${escapeHtml(title)}</a>`);
	}

	function transformTodoTable(headers, rows) {
		const colKeys = headers.map(h => String(h).trim().toLowerCase());
		const idx = name => colKeys.indexOf(name);

		const records = rows.map(row => ({
			title: idx("title") >= 0 ? String(row[idx("title")] ?? "").trim() : "",
			uri: idx("uri") >= 0 ? String(row[idx("uri")] ?? "").trim() : "",
			source: formatSource(
				idx("author") >= 0 ? row[idx("author")] : "",
				idx("year_pub") >= 0 ? row[idx("year_pub")] : "",
				idx("provider") >= 0 ? row[idx("provider")] : ""
			),
			region: idx("region") >= 0 ? String(row[idx("region")] ?? "").trim() : "",
			crop: idx("crop") >= 0 ? String(row[idx("crop")] ?? "").trim() : "",
			group: idx("group") >= 0 ? String(row[idx("group")] ?? "").trim() : ""
		}));

		return {
			headers: ["title", "source", "region", "crop", "group", "uri"],
			rows: records.map(r => [r.title, r.source, r.region, r.crop, r.group, r.uri])
		};
	}

	async function renderTable() {
		const host = document.getElementById("table-host");
		const subtitle = document.getElementById("subtitle");
		const file = TABLE_FILE;

		document.title = "carob — dataset to-do list";

		const raw = document.getElementById("raw-link");
		if (rawBase) {
			raw.href = `${rawBase}/${file}`;
		} else {
			raw.style.display = "none";
		}

		const repoLink = document.getElementById("repo-link");
		if (repoLink && repoUrl) repoLink.href = repoUrl;

		let manifest = [];
		try { manifest = await fetchTables(); } catch (_) { /* optional */ }

		const meta = manifest.find(t => t.file === file);

		let csvText;
		try {
			const r = await fetch(file, { cache: "no-cache" });
			if (!r.ok) throw new Error("HTTP " + r.status);
			csvText = await r.text();
		} catch (e) {
			host.innerHTML = `<p class="error">Could not load <code>${escapeHtml(file)}</code>: ${escapeHtml(e.message)}</p>`;
			return;
		}

		const parsed = Papa.parse(csvText.replace(/^\uFEFF/, "").trim(), {
			header: false,
			skipEmptyLines: true
		});
		if (!parsed.data || parsed.data.length < 1) {
			host.innerHTML = '<p class="error">Empty file.</p>';
			return;
		}

		let headers = parsed.data[0].map(h => String(h || ""));
		let rows = parsed.data.slice(1).map(row => {
			if (row.length < headers.length) {
				return row.concat(new Array(headers.length - row.length).fill(""));
			}
			return row.slice(0, headers.length);
		});

		({ headers, rows } = dropEmptyColumns(headers, rows));
		const transformed = transformTodoTable(headers, rows);
		headers = transformed.headers;
		rows = transformed.rows;

		if (subtitle) {
			const rowCount = meta ? meta.rows : rows.length;
			subtitle.textContent = `${rowCount.toLocaleString()} rows \u00b7 5 columns`;
		}

		const colKeys = headers.map(h => String(h).trim().toLowerCase());
		const uriColIdx = colKeys.indexOf("uri");

		host.innerHTML = "";
		const initialPageSize = readPageSize();
		applyPageSizeSelect(initialPageSize);
		const grid = new gridjs.Grid({
			columns: headers.map((h, idx) => {
				const key = colKeys[idx];
				const col = {
					name: h,
					sort: true,
					attributes: () => ({ "data-col": key })
				};
				if (key === "uri") {
					col.hidden = true;
				}
				if (key === "title") {
					col.formatter = (cell, row) => {
						const uri = row.cells[uriColIdx]?.data ?? "";
						return titleLinkCell(cell, uri);
					};
				}
				return col;
			}),
			data: rows,
			search: { enabled: true },
			sort: true,
			pagination: paginationConfig(initialPageSize, rows.length),
			resizable: false,
			fixedHeader: false,
			width: "100%",
			language: {
				search: { placeholder: "Search this table…" }
			}
		}).render(host);

		const sizeSel = document.getElementById("page-size");
		if (sizeSel) {
			sizeSel.addEventListener("change", () => {
				const val = sizeSel.value;
				writePageSize(val);
				grid.updateConfig({
					pagination: paginationConfig(val, rows.length)
				}).forceRender();
			});
		}
	}

	const PAGE_SIZE_KEY = "carobTodo.pageSize";
	const DEFAULT_PAGE_SIZE = "100";

	function readPageSize() {
		try {
			const v = localStorage.getItem(PAGE_SIZE_KEY);
			if (v) return v;
		} catch (_) { /* private mode etc. */ }
		return DEFAULT_PAGE_SIZE;
	}

	function writePageSize(v) {
		try { localStorage.setItem(PAGE_SIZE_KEY, String(v)); } catch (_) {}
	}

	function applyPageSizeSelect(v) {
		const sel = document.getElementById("page-size");
		if (!sel) return;
		const known = Array.from(sel.options).some(o => o.value === String(v));
		sel.value = known ? String(v) : DEFAULT_PAGE_SIZE;
	}

	function paginationConfig(size, totalRows) {
		if (String(size).toLowerCase() === "all") {
			return { enabled: true, limit: Math.max(totalRows, 1), summary: true };
		}
		const n = parseInt(size, 10);
		if (!isFinite(n) || n <= 0) {
			return { enabled: true, limit: parseInt(DEFAULT_PAGE_SIZE, 10), summary: true };
		}
		return { enabled: true, limit: n, summary: true };
	}

	function dropEmptyColumns(headers, rows) {
		const keep = headers.map((h, i) => {
			const headerEmpty = !String(h || "").trim();
			if (!headerEmpty) return true;
			return rows.some(r => String(r[i] ?? "").trim() !== "");
		});
		if (keep.every(Boolean)) return { headers, rows };
		return {
			headers: headers.filter((_, i) => keep[i]),
			rows: rows.map(r => r.filter((_, i) => keep[i]))
		};
	}

	window.carobTodo = { renderTable };
})();
