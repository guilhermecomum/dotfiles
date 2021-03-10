#!/bin/bash
cd /tmp || exit
echo "Installing or updating Zoom ..."
wget -q https://zoom.us/client/latest/zoom_amd64.deb
sudo apt install -f -y  ./zoom_amd64.deb

echo "Done! 🎉"
