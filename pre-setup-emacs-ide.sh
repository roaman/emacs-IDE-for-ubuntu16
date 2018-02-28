### Install git
sudo apt-get install git
# config git
git config --global user.name "roaman"
git config --global user.email liguojun518@gmail.com
git config --global core.editor 'emacs -q'
# git config --global credical
git config --global rerere.enabled true


### common part
#sudo apt-get install ttf-bitstream-vera
#sudo apt-get install fonts-inconsolata
sudo apt-get install dbus-x11

### aspell
sudo apt-get install aspell


# install dependency package
sudo apt-get install gcc
sudo apt-get install libncurses5-dev

### clang
sudo apt-get install clang



## install ggtags  for ubuntu
# 1. download the Global
# https://www.gnu.org/software/global/download.html
wget http://tamacom.com/global/global-6.5.7.tar.gz
# 2. install on OS
tar xvf global-6.5.7.tar.gz
cd global-6.5.7
./configure [--with-exuberant-ctags=/usr/local/bin/ctags]
#If you have ctags installed, add the option --with-exuberant-ctags and supply the installed path:
# 4. compile and install
make
sudo make install
# 5.install emacs frontend in emacs
# refer to init.el

#for Navigate system include path, generate gtags database
#edit .bashrc or .zshrc:
export GTAGSLIBPATH=$HOME/.gtags/

# Create a directory for holding database, since you cannot create a database in your system paths
mkdir ~/.gtags
cd .gtags
# Create symbolic links to your external libraries
ln -s /usr/include usr-include
ln -s /usr/local/include/ usr-local-include
# Generate GNU Global database
gtags -c

### org-mode
## support the image capture and display
# sudo apt-get install scrot

## python
sudo apt-get install python-pip
sudo apt-get install ipython

pip install jedi flake8 importmagic autopep8
## if not use autopep8, you can use yapf
# pip install yapf


# automake
sudo apt-get install autoconf
sudo apt-get install libtool
sudo apt-get install libsysfs-dev 
