FROM python:3.7.4-slim

RUN apt-get update && \
    apt-get upgrade --yes && \
    apt-get install --yes git

COPY requirements.txt requirements.txt
RUN pip install --requirement requirements.txt && \
  jupyter nbextension enable --py widgetsnbextension

# jupyter compains if run as root.
RUN useradd --create-home --shell=/bin/bash jupyter
USER jupyter
WORKDIR /user/jupyter

EXPOSE 8888
CMD ["jupyter", "notebook", "--ip=0.0.0.0", "--no-browser"]
