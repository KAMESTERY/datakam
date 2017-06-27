FROM java:8-alpine
MAINTAINER Your Name <you@example.com>

ADD target/slapman-0.0.1-SNAPSHOT-standalone.jar /slapman/app.jar

EXPOSE 8080

CMD ["java", "-jar", "/slapman/app.jar"]
