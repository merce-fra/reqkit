set -ex
cd ../contrib/
wget http://www.lrde.epita.fr/dload/spot/spot-2.12.1.tar.gz
tar -xzvf spot-2.12.1.tar.gz
cd spot-2.12.1
./configure --prefix ~/local/
make
make install
export PATH=$PATH:~/local/bin/
echo "export PATH=$PATH:~/local/bin/" >> ~/.bashrc
source ~/.bashrc
# sudo su
# wget -q -O - https://www.lrde.epita.fr/repo/debian.gpg | apt-key add -
# echo 'deb http://www.lrde.epita.fr/repo/debian/ stable/' >> /etc/apt/sources.list
# exit
# sudo apt-get update
# sudo apt-get install spot libspot-dev spot-doc python3-spot