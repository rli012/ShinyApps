# ShinyApps

### Shiny on AWS

* Step1: Create an AWS account (12-Month Free Tier)

* Step2: Login to the account, launch an instance
  - Ubuntu Server 18.04 LTS (HVM), SSD Volume Type \[64-bit (x86)\]
  - t2.micro -> 1 CPU, 1G Memory, 20G Storage

* Step3: Setup Secruity Groups
  - In the Security Group, add 'Custom TCP, TCP, 3838, 0.0.0.0/0'
  
