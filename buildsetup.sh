mkdir ~/src/ && cd ~/src/

echo "Publishing locally: SNAPSHOT firrtl2..."
git clone https://github.com/ucb-bar/firrtl2.git
# Note the '+': we have to cross-publish this library, since it is written in Scala 3, but we consume it from Scala 2.13
(cd firrtl2 && sbt '+publishLocal')

echo "Publishing locally: SNAPSHOT chiseltest..."
git clone https://github.com/ucb-bar/chiseltest.git
(cd chiseltest && sbt publishLocal)
