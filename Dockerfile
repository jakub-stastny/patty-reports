FROM openjdk:17
COPY target/reports-api.jar /app.jar
CMD ["java", "-jar", "/app.jar"]
