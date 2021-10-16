FROM ubuntu:20.04

RUN apt-get update && apt-get install -y openscad

COPY ./entrypoint.sh /
RUN chmod +x /entrypoint.sh

ENTRYPOINT [ "/entrypoint.sh" ]
