FROM ubuntu:trusty
MAINTAINER Xe <xena@yolo-swag.com>

RUN apt-get update && \
    apt-get -y install curl apache2 libapache2-mod-php5 php5-mysql php5-gd php5-curl php-pear php-apc php5-mcrypt git php5-curl php5-memcache memcached build-essential php5-tidy apache2-dev php5-json php5-dev &&\
    rm -rf /var/lib/apt/lists/* &&\
    sed -i "s/variables_order.*/variables_order = \"EGPCS\"/g" /etc/php5/apache2/php.ini &&\
    curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer &&\
    mkdir -p /app/ && rm -fr /var/www/html && ln -s /app /var/www/html &&\
    pecl install apc &&\
    echo 'extension=apc.so' >> /etc/php5/apache2/php.ini &&\
    echo 'extension=memcache.so' >> /etc/php5/apache2/php.ini &&\
    a2enmod rewrite &&\
    sed -i.bak 's/upload_max_filesize = 2M/upload_max_filesize = 15M/g' /etc/php5/apache2/php.ini &&\
    sed -i.bak 's/post_max_size = 8M/post_max_size = 15M/g' /etc/php5/apache2/php.ini &&\
    a2enmod expires &&\
    a2enmod headers &&\
    php5enmod mcrypt &&\
    rm -rf /etc/apache2/sites-available/* /etc/apache2/sites-enabled/*

ADD 000-default.conf /etc/apache2/sites-available/000-default.conf
RUN ln /etc/apache2/sites-available/000-default.conf /etc/apache2/sites-enabled/000-default.conf

EXPOSE 80

ONBUILD ADD . /app
ONBUILD RUN cd /app && composer install ; true

WORKDIR /app
ADD run.sh /run.sh
CMD ["/run.sh"]
