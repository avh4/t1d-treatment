FROM jupyter/scipy-notebook

RUN pip install pystan
RUN pip install daft

RUN pip install jupyter_contrib_nbextensions
RUN jupyter contrib nbextensions install --sys-prefix
RUN jupyter nbextension enable collapsible_headings/main
