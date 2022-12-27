FROM ubuntu:22.04

RUN apt-get update \
    && apt-get install -y flex bison build-essential csh openjdk-8-jdk libxaw7-dev

RUN apt-get install -y wget tar

RUN mkdir /usr/class \
    && cd /usr/class \
    && wget https://courses.edx.org/asset-v1:StanfordOnline+SOE.YCSCS1+1T2020+type@asset+block@student-dist.tar.gz -O student-dist.tar.gz \
    && tar -xf student-dist.tar.gz \
    && ln -s /usr/class/cs143/cool ~/cool \
    && sed '$aPATH=/usr/class/cs143/cool/bin:$PATH' /root/.profile

