# Diversity of ISCB Honorees

## Development

This repository has a corresponding Docker image with the required dependencies.
See [`environment`](environment) for the Docker image specification.

Note that the following Docker commands have a `--mount` argument to give the Docker container access to files in this repository.
Therefore, any changes to the repository content created while running the Docker container will persist in this directory after the container is stopped.

### Interactive

For interactive development, run the following command:

```shell
# This command must be run with the repository root as your working directory.
# Requires docker version >= 17.06.
docker run \
  --name iscb-diversity \
  --detach --rm \
  --env JUPYTER_TOKEN=ksbegpqzrurktbkikyo \
  --publish 8899:8888 \
  --mount type=bind,source="$(pwd)",target=/user/jupyter \
  iscb-diversity
```

Then navigate to the following URL in your browser:
<http://localhost:8899?token=ksbegpqzrurktbkikyo>

You should see a Jupyter Notebook landing page where you can open, edit, and run any of the notebooks.

When you are done, you shutdown the Jupyter notebook server and remove the Docker container by running ```docker stop iscb-diversity``` in a new terminal.

## License

The entire repository is released under the CC BY 4.0 License available in [`license.md`](license.md).
All code files and snippets are additionally released under the BSD 3-Clause License available in [`license-code.md`](license-code.md).
