"""
Generate docs/readme.me for GitHub Pages.
"""
import pathlib

readme_template = '''\
# Project Webpage for ISCB Diversity Analysis

<!-- make sure to edit this content in utils/prepare_docs.py and not docs/readme.md -->

More information at <https://github.com/greenelab/iscb-diversity>.

## Notebooks

See the following rendered notebooks:

{notebook_list_md}
'''

if __name__ == "__main__":
    docs_dir = pathlib.Path(__file__).parent.parent.joinpath('docs')
    assert docs_dir.is_dir()
    html_paths = sorted(docs_dir.glob("**/*.html"))
    notebook_list_md = ""
    for path in html_paths:
        path = path.relative_to(docs_dir)
        notebook_list_md += f"- [`{path}`]({path})\n"
    readme = readme_template.format(notebook_list_md=notebook_list_md)
    docs_dir.joinpath("readme.md").write_text(readme)
