FROM centos:7

RUN yum -y install epel-release && yum -y install python-pip
ADD . /app

WORKDIR /app

EXPOSE 5000

RUN pip install -r requirements.txt
CMD "python ./main.py"
