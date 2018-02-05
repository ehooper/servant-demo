FROM phusion/baseimage
RUN apt-get update && apt-get install -y apache2

# set up SSL
RUN openssl req -x509 -newkey rsa:4086 \
        -subj "/C=XX/ST=XXXX/L=XXXX/O=XXXX/CN=localhost" \
        -keyout "/etc/ssl/demo.key" \
        -out "/etc/ssl/demo.crt" \
        -days 3650 -nodes -sha256

# set up Apache
COPY demo_ssl.conf /etc/apache2/sites-available/
RUN rm /etc/apache2/sites-enabled/* && a2enmod proxy proxy_http ssl && a2ensite demo_ssl.conf

# set up services
COPY config.yml /etc/demo-server/config.yml
COPY servant-demo-exe /app/demo-server-exe
COPY demo-server.sh /etc/service/demo-server/run
COPY apache.sh /etc/service/apache/run

EXPOSE 443
CMD ["/sbin/my_init"]
