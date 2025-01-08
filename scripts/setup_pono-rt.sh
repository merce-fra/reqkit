set -ex
cd ../contrib/
wget https://github.com/osankur/pono-rt/releases/download/v0.1/pono-rt
echo 'export PATH=$PATH:'`pwd` >> ~/.bashrc