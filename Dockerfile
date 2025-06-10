FROM rocker/shiny

RUN mkdir /home/shiny-app

RUN R -e "install.packages(c('rhandsontable'))"

COPY odontograma_icdas_II_long_form.R /home/shiny-app/app.R

EXPOSE 8180

CMD ["Rscript", "/home/shiny-app/app.R"]