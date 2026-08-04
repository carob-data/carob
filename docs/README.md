# Carob dataset to-do list (GitHub Pages)

Interactive, searchable view of `misc/todo/to-do.csv`.

The site is built and deployed by `.github/workflows/todo-pages.yml` when `misc/todo/to-do.csv` changes (or when site files under `docs/` are updated). Enable **Settings → Pages → Build and deployment → Source: GitHub Actions** on the repository.

### Local preview

```sh
python3 docs/_build/serve.py        # http://localhost:8000
python3 docs/_build/serve.py 8080   # pick a port
```
