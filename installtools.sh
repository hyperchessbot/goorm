echo "update"
sudo apt-get update -y
echo "upgrade"
sudo apt-get upgrade -y

echo "install nodejs"
sudo apt-get install nodejs -y
sudo apt-get install npm -y

echo "update node"
npm cache clean -f
npm install -g n
n latest
node -v

echo "install yarn"
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt-get update -y
sudo apt-get install yarn -y
yarn --version

echo "install scala"
#sudo apt-get update -y
#sudo apt-get upgrade -y
sudo apt-get install default-jdk -y
wget www.scala-lang.org/files/archive/scala-2.13.0.deb
sudo dpkg -i scala*.deb
scalac -version

echo "install sbt"
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt-get update -y
sudo apt-get install sbt -y
