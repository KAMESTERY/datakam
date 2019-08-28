FROM java:8-alpine
MAINTAINER Your Name <you@example.com>

ADD target/datakam-0.0.1-SNAPSHOT-standalone.jar /datakam/app.jar

EXPOSE 8080

CMD ["java", "-jar", "/datakam/app.jar"]
