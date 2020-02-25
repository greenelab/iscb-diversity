# Environment

![Build & Publish Docker](https://github.com/greenelab/iscb-diversity/workflows/Build%20&%20Publish%20Docker/badge.svg)

[`requirements.txt`](requirements.txt) specifies the Python dependencies for this repository.

GitHub Actions will automatically build and publish to this repo's [package registry](https://github.com/greenelab/iscb-diversity/packages), when the source for the image changes.
Therefore, you do not need to build the image locally.
However, if you'd like to for development, run the following command with this directory as your current working directory:

```shell
# Build the Docker image specified by Dockerfile
docker build --tag docker.pkg.github.com/greenelab/iscb-diversity/iscb-diversity .
```
