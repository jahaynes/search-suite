FROM mozilla/sbt as build-metacache

WORKDIR /metacache

COPY build.sbt .
COPY src       ./src
COPY project   ./project

RUN sbt compile

RUN sbt assembly

##########################

FROM openjdk:15

WORKDIR /metacache

COPY --from=build-metacache /metacache/target/scala-2.13/metacache-assembly-1.0.jar .

CMD ["java", "-jar", "metacache-assembly-1.0.jar"]