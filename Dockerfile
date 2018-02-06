FROM jupyter/scipy-notebook

RUN pip install pystan
RUN pip install daft

RUN pip install jupyter_contrib_nbextensions
RUN jupyter contrib nbextensions install --sys-prefix
RUN jupyter nbextension enable collapsible_headings/main

USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends graphviz && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
USER $NB_USER
RUN pip install graphviz
