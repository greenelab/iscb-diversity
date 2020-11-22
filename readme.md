# Diversity of ISCB Honorees

This is a data analysis repository for the study at <https://greenelab.github.io/iscb-diversity-manuscript/>.

## Datasets

Datasets are stored in the [`data`](data) directory.
This repository uses [Git LFS](https://git-lfs.github.com/) to store large / binary datasets.
Make sure to have Git LFS installed locally before cloning the repository,
if you'd like to download the datasets.
You can also download datasets directly from the GitHub website by clicking "Raw".

In general, we use [XZ](https://tukaani.org/xz/) for compressing files (denoted by an `.xz` extension).
For users that would like a graphical interface for decompressing XZ-compressed files,
options include [PeaZip](https://giorgiotani.github.io/PeaZip/) on Windows (open source) and [The Unarchiver](https://theunarchiver.com/) on macOS (proprietary freeware).
macOS users can install the `xz` command line utility with `brew install xz`.

## Development

This repository has a corresponding Docker image with the required dependencies.
See [`environment`](environment) for the Docker image specification.

Note that the following Docker commands have a `--mount` argument to give the Docker container access to files in this repository.
Therefore, any changes to the repository content created while running the Docker container will persist in this directory after the container is stopped.

The Docker image is automatically built and published by a GitHub Action.
Even though this repository is public, GitHub [requires](https://github.community/t5/GitHub-Actions/docker-pull-from-public-GitHub-Package-Registry-fail-with-quot/td-p/32782) authentication to download from its package registry.
Therefore, you will need a GitHub account to pull the image.

Use the following steps to [authenticate](https://help.github.com/en/packages/using-github-packages-with-your-projects-ecosystem/configuring-docker-for-use-with-github-packages#authenticating-to-github-packages) your local docker with your GitHub.
Go to <https://github.com/settings/tokens> and create a new personal access token, selecting only the `read:packages` scope.
You can name the token anything, for example "docker login read-only token".
Then run the following command, substituting your username and token from above:

```shell
docker login --username USERNAME --password TOKEN docker.pkg.github.com
```

### Interactive

For interactive development in Python notebooks, run the following command:

```shell
# This command must be run with the repository root as your working directory.
# Requires docker version >= 17.06.
docker run \
  --name iscb-diversity \
  --detach --rm \
  --env JUPYTER_TOKEN=ksbegpqzrurktbkikyo \
  --publish 8899:8888 \
  --mount type=bind,source="$(pwd)",target=/user/jupyter \
  docker.pkg.github.com/greenelab/iscb-diversity/iscb-diversity
```
Then navigate to the following URL in your browser:
<http://localhost:8899?token=ksbegpqzrurktbkikyo>

You should see a Jupyter Notebook landing page where you can open, edit, and run any of the notebooks.

When you are done, you shutdown the Jupyter notebook server and remove the Docker container by running ```docker stop iscb-diversity``` in a new terminal.

Similarly, for the R notebooks:
```shell
# This command must be run with the repository root as your working directory.
# Requires docker version >= 17.06.
docker run \
  --name iscb-diversity-r \
  --detach --rm \
  --publish 8787:8787 \
  --env DISABLE_AUTH=true \
  --mount type=bind,source="$(pwd)",target=/home/rstudio/repo \
  iscb-diversity-r
```

Navigate to <http://localhost:8787> and you should be logged into RStudio as the rstudio user.
When you are done, shutdown the RStudio server and remove the Docker container by running ```docker stop iscb-diversity-r```.


## GitHub Pages

The [`docs`](docs) directory is used as the GitHub Pages source for <https://github.com/greenelab/iscb-diversity>.
To regenerate outputs in the `docs` directory, run the following command

```shell
python utils/prepare_docs.py --nbviewer --readme
```

Edit [`utils/prepare_docs.py`](utils/prepare_docs.py) to change the template for `docs/readme.md`.

## License

The entire repository is released under the CC BY 4.0 License available in [`license.md`](license.md).
All code files and snippets are additionally released under the BSD 3-Clause License available in [`license-code.md`](license-code.md).
