#!/bin/bash
#
# Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
sudo apt-get install \
     autoconf \
     autoconf-archive \
     automake \
     fontconfig \
     freeglut3-dev \
     libegl1 \
     libegl1-mesa-dev \
     libglu1-mesa-dev \
     libltdl-dev \
     libtool \
     libx11-xcb-dev \
     '^libxcb.*-dev' \
     libxcursor-dev  \
     libxi-dev \
     libxinerama-dev  \
     libxkbcommon-dev \
     libxkbcommon-x11-dev \
     libxrender-dev \
     mesa-common-dev \
     pkg-config \
     xorg-dev
