set -ex
cd ../contrib/
wget http://www.lrde.epita.fr/dload/spot/spot-2.12.1.tar.gz
tar -xzvf spot-2.12.1.tar.gz
cd spot-2.12.1
./configure --prefix ~/local/
make
make install
export PATH=$PATH:~/local/bin/
echo 'export PATH='$PATH:~/local/bin/ >> ~/.bashrc
source ~/.bashrc

# The Debian package can be installed instead of compiling from source on Ubuntu 24.04
#
# wget -q -O - https://www.lrde.epita.fr/repo/debian.gpg | sudo tee /etc/apt/trusted.gpg.d/lrde-debian.gpg
# echo 'deb http://www.lrde.epita.fr/repo/debian/ stable/' | sudo tee /etc/apt/sources.list
# sudo apt-get update
# sudo apt-get install spot libspot-dev spot-doc python3-spot
