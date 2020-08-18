# ShinyApps

### Shiny on AWS

* Step1: Create an AWS account (12-Month Free Tier)

* Step2: Login to the account, launch an instance
  - Ubuntu Server 18.04 LTS (HVM), SSD Volume Type \[64-bit (x86)\]
  - t2.micro -> 1 CPU, 1G Memory, 20G Storage
  - Download the key pair file: app.perm. Change the permission: *chmod 400 app.pem*
  - IMPORTANT: IPv4 Public IP (e.g., 3.89.45.124) & Public DNS (IPv4)

* Step3: Setup Security Groups
  - In the Security Group, add 'Custom TCP, TCP, 3838, 0.0.0.0/0'
  
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
