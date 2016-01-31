mvn clean install
scalajsld -d target -o target/nitin-nizhawan-foren.js  target/classes ~/.m2/repository/org/scala-js/scala-parser-combinators_sjs0.6_2.11/1.0.2/scala-parser-combinators_sjs0.6_2.11-1.0.2.jar
cp target/nitin-nizhawan-foren.js app/
