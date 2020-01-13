"""
Generate docs/readme.me for GitHub Pages.
"""
import pathlib
import subprocess

root_dir = pathlib.Path(__file__).parent.parent
docs_dir = root_dir.joinpath("docs")

readme_template = """\
# Project Webpage for ISCB Diversity Analysis

<!-- make sure to edit this content in utils/prepare_docs.py and not docs/readme.md -->

More information at <https://github.com/greenelab/iscb-diversity>.

## Notebooks

See the following rendered notebooks:

{notebook_list_md}
"""


def render_jupyter_notebooks():
    ipynb_paths = sorted(root_dir.glob("*.ipynb"))
    ipynb_paths = [path.relative_to(root_dir) for path in ipynb_paths]
    args = [
        "jupyter",
        "nbconvert",
        "--output-dir=docs",
        *ipynb_paths,
    ]
    subprocess.run(args, cwd=root_dir)


def get_notebook_list_md():
    html_paths = sorted(docs_dir.glob("**/*.html"))
    notebook_list_md = ""
    for path in html_paths:
        path = path.relative_to(docs_dir)
        notebook_list_md += f"- [`{path}`]({path})\n"
    return notebook_list_md


if __name__ == "__main__":
    assert docs_dir.is_dir()
    render_jupyter_notebooks()
    html_paths = sorted(docs_dir.glob("**/*.html"))
    notebook_list_md = get_notebook_list_md()
    readme = readme_template.format(notebook_list_md=notebook_list_md)
    docs_dir.joinpath("readme.md").write_text(readme)
