FROM opencpu/base

LABEL Pantelis Karatzas <pantelispanka@gmail.com>
RUN apt-get -y install libxml2-dev libcurl4-openssl-dev

RUN R -e "install.packages(c('RCurl', 'jsonlite', 'deSolve'), repos='http://cran.cc.uoc.gr/mirrors/CRAN/')"
COPY PredictPBPK.tar.gz /packages/
USER root
RUN R CMD INSTALL /packages/PredictPBPK.tar.gz --library=/usr/local/lib/R/site-library

CMD /usr/sbin/apache2ctl -D FOREGROUND
