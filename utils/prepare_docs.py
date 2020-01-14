"""
Generate docs directory for GitHub Pages.
"""
import argparse
import pathlib
import subprocess
import sys

root_dir = pathlib.Path(__file__).parent.parent
docs_dir = root_dir.joinpath("docs")

readme_template = """\
# Project Webpage for ISCB Diversity Analysis

<!-- make sure to edit this content in utils/prepare_docs.py and not docs/readme.md -->

More information at <https://github.com/greenelab/iscb-diversity>.
See also the study corresponding to this analysis at <https://greenelab.github.io/iscb-diversity-manuscript/>.

## Notebooks

See the following rendered notebooks:

{notebook_list_md}
"""


def parse_args():
    parser = argparse.ArgumentParser(
        description="Generate docs directory for GitHub Pages."
    )
    parser.add_argument(
        "--nbconvert", action="store_true", help="Convert .ipynb files to docs/*.html"
    )
    parser.add_argument(
        "--readme",
        action="store_true",
        help="Regenerate docs/readme.md (the GitHub Pages homepage)",
    )
    args = parser.parse_args()
    if len(sys.argv) == 1:
        # print help when no arguments are specified
        parser.print_help()
    return args


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
        notebook_list_md += f"- [{path.stem}]({path})\n"
    return notebook_list_md


if __name__ == "__main__":
    args = parse_args()
    assert docs_dir.is_dir()
    if args.nbconvert:
        render_jupyter_notebooks()
    if args.readme:
        notebook_list_md = get_notebook_list_md()
        readme = readme_template.format(notebook_list_md=notebook_list_md)
        docs_dir.joinpath("readme.md").write_text(readme)
