#!/bin/bash
echo "Current global git user set to: "
git config --global user.email
echo
echo "Name of new global git user: ('custom' for custom mail)"

read name

while :
do
	case $name in
		custom)
			echo "Custom email-adress: "
			read customemail
			git config --global user.email $customemail
			echo "Custom name (first name only): "
			read customname
			git config --global user.name $customname
			break
			;;
		edvard)
			git config --global user.email efnarum@stud.ntnu.no
			git config --global user.name "Edvard Narum"
			break
			;;
		johan)
			git config --global user.email johannicbrun@gmail.com
			git config --global user.name "Johan Brun"
			break
			;;
		clear)
			git config --global --unset-all user.email
			git config --global --unset-all user.name
			echo "Git user config cleared"
			break
			;;
		*)
			echo "Unrecognized input. Leaving user as is."
			break
			;;
	esac
done
echo
echo "Global git-user email set to: "
git config --global user.email
echo "Global git-user name set to: "
git config --global user.name
