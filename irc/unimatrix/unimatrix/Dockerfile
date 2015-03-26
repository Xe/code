# Unimatrix Dockerfile

FROM uggedal/alpine
MAINTAINER Christine Dodrill <xena@yolo-swag.com>

RUN echo "http://repos.lax-noc.com/alpine/edge/testing" >> /etc/apk/repositories

RUN apk update
RUN apk upgrade

# Install python and tools
RUN apk add python py-virtualenv py-pip
RUN apk add py-setuptools

ADD requirements.txt /src/requirements.txt
RUN pip install -r /src/requirements.txt

ADD . /src

EXPOSE 5000

WORKDIR /src

CMD ["python", "/src/runserver.py"]

