# Interactive AOI Data Tool

## Robert Botto
## 2019-03-15

### Prepare Stimuli csv
```
temp.stimuli <- unique(global.eye.data[grep('jpg', global.eye.data$Content), c('Stimulus', 'Content')])
write.csv(temp.stimuli, "stimuli.csv", row.names = FALSE)

```

Now open the csv in an editor and 

1. Remove any stimuli you don't want to analyze.
2. Rename each stimulus with the label you want displayed on the web page.
3. Move the file `stimuli.csv` into the data folder of your app.

### AWS Setup

```
sudo yum install -y R

wget https://download2.rstudio.org/rstudio-server-rhel-1.1.463-x86_64.rpm
sudo yum install -y rstudio-server-rhel-1.1.463-x86_64.rpm
rm rstudio-server-rhel-1.1.463-x86_64.rpm

sudo R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
wget https://download3.rstudio.org/centos6.3/x86_64/shiny-server-1.5.9.923-x86_64.rpm
sudo yum install -y --nogpgcheck shiny-server-1.5.9.923-x86_64.rpm
rm shiny-server-1.5.9.923-x86_64.rpm

sudo yum install -y libjpeg-turbo-devel
sudo R -e "install.packages('jpeg', repos='http://cran.rstudio.com/')"
sudo R -e "install.packages('MASS', repos='http://cran.rstudio.com/')"

sudo useradd botto
sudo passwd botto
```

### Shiny Server Config

```
su - botto
mkdir ShinyApps
exit
sudo /opt/shiny-server/bin/deploy-example user-dirs

```

### SSH Auth from local dev

```
su - botto
mkdir .ssh
chmod 700 .ssh
vi .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
```
