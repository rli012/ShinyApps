# ShinyApps

### Shiny Server on AWS

* Step1: Create an AWS account (12-Month Free Tier)

* Step2: Login to the account, launch an instance
  - Ubuntu Server 18.04 LTS (HVM), SSD Volume Type \[64-bit (x86)\]
  - t2.micro -> 1 CPU, 1G Memory, 20G Storage
  - Download the key pair file: app.perm. Change the permission: *chmod 400 app.pem*
  - IMPORTANT: IPv4 Public IP (e.g., 3.89.45.124) & Public DNS (IPv4)

* Step3: Setup Security Groups
  - In the Security Group, Edit inbound roles: 'Custom TCP, TCP, 3838, 0.0.0.0/0'
  
* Step4: ssh -i app.perm ubuntu@3.89.45.124
  - *sudo apt-get update*
  - *sudo apt-get install git*
  - *sudo apt-get install libcurl4-openssl-dev*
  - *sudo apt-get install libxml2-dev*
  - #*sudo apt-get install r-base*
  
  - *sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9*
  - *sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'*
  - *sudo apt update*
  - *sudo apt install r-base r-base-dev*

* Step5: Install *shiny*
  - *sudo -i*
  - *R*
  - *install.packages(‘shiny’, repos = ‘http://cran.rstudio.com/')*
  
 * Step6: Install *shiny server*
  - *sudo apt-get install gdebi-core*
  - *wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.14.948-amd64.deb*
  - *sudo gdebi shiny-server-1.5.14.948-amd64.deb*
  
  - *netstat -nlpt*
  
  * Step7: Test Shiny Server
   - In browser: 3.89.45.124:3838
   - If not working, *sudo service shiny-server restart*, then try again (**restart anyway**)
   
 * Step8: Configure shiny-server.conf
    - *sudo vi /etc/shiny-server/shiny-server.conf*
    - Add   
  location /CancerMIRNome {  
    app_dir /home/ubuntu/ShinyApps/CancerMIRNome;  
    log_dir /var/log/shiny-server/CancerMIRNome;  
  }  
   
* Step 9: Upload data to EC2
  - *scp -i app.perm -r CancerMIRNome/ ubuntu@3.89.45.124:/home/ubuntu/ShinyApps/*
  
* Step9: Launch the app: https://3.89.45.124:3838/CancerMIRNome/


### RStudio Server on AWS

* [Download RStudio Server for Debian & Ubuntu](https://rstudio.com/products/rstudio/download-server/debian-ubuntu/)
  - #*sudo apt-get install gdebi-core*
  - *wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.3.1073-amd64.deb*
  - *sudo gdebi rstudio-server-1.3.1073-amd64.deb*
  
* [RStudio Server: Managing the Server](https://support.rstudio.com/hc/en-us/articles/200532327-Managing-the-Server)
  - *sudo rstudio-server status*
  - *sudo rstudio-server restart*
  
* Generate an account
  - *sudo useradd rli012*
  - *sudo passwd rli012*
  - *cd /home/*
  - *mkdir rli012*
  - *sudo chown -R rli012 /home/rli012*
  
* Setup Security Groups
  - In the Security Group, add 'Custom TCP, TCP, 8787, 0.0.0.0/0'
  
* Launch RStudio Server: https://3.89.45.124:8787/
  
  
### Install plotly
* Install openssl
  - *sudo apt-get install -y libssl-dev*
  - *R*
  - *install.packages('openssl')
