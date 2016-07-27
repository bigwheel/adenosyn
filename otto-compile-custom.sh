#!/usr/bin/env bash

rm -rf .otto

otto compile
find .otto/ -type f -print | xargs sed -i -e "s|hashicorp/precise64|bento/ubuntu-14.04|g"
find .otto/ -type f -print | xargs sed -i -e 's|otto_output "Installing Gradle 2.8..."|otto_output "Installing Gradle 2.10..."|g'
find .otto/ -type f -print | xargs sed -i -e 's|java_gradle_install "2.8"|java_gradle_install "2.10"|g'
sed -i '9i  config.vm.provider "virtualbox" do |vb|\n    vb.memory = "4096"\n  end\n' .otto/compiled/app/dev/Vagrantfile
