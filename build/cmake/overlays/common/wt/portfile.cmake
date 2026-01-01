# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
# Minimal Wt build for ORES - disables PDF, Pango, and OpenGL support
# to reduce dependencies and build time.
#

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO emweb/wt
    REF "${VERSION}"
    SHA512 89754567b823105de694ee1c2f6e8cecd0b6c1231531f2b791ff95f29039567273329ac9ecc612a96e44232cf80371e947a7a424903c2be8e8c14d0d7260e4d5
    HEAD_REF master
    PATCHES
        0005-XML_file_path.patch
        0006-GraphicsMagick.patch
)

string(COMPARE EQUAL "${VCPKG_LIBRARY_LINKAGE}" "dynamic" SHARED_LIBS)

vcpkg_check_features(
    OUT_FEATURE_OPTIONS
    FEATURE_OPTIONS
    FEATURES
        dbo        ENABLE_LIBWTDBO
        postgresql ENABLE_POSTGRES
        sqlite3    ENABLE_SQLITE
        openssl    ENABLE_SSL
)

# Minimal build options - disable PDF, Pango, OpenGL
set(WT_PLATFORM_SPECIFIC_OPTIONS
    -DWT_WRASTERIMAGE_IMPLEMENTATION=none
    -DENABLE_PANGO=OFF
)

if(VCPKG_TARGET_IS_WINDOWS)
    list(APPEND WT_PLATFORM_SPECIFIC_OPTIONS -DCONNECTOR_ISAPI=ON)
else()
    list(APPEND WT_PLATFORM_SPECIFIC_OPTIONS -DCONNECTOR_FCGI=OFF)
endif()

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    GENERATOR Ninja
    OPTIONS
        -DINSTALL_CONFIG_FILE_PATH="${DOWNLOADS}/wt"
        -DSHARED_LIBS=${SHARED_LIBS}
        -DBOOST_DYNAMIC=${SHARED_LIBS}
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTS=OFF

        -DWTHTTP_CONFIGURATION=
        -DCONFIGURATION=

        -DCONNECTOR_HTTP=ON
        -DENABLE_HARU=OFF
        -DENABLE_MYSQL=OFF
        -DENABLE_FIREBIRD=OFF
        -DENABLE_QT4=OFF
        -DENABLE_QT5=OFF
        -DENABLE_LIBWTTEST=ON
        -DENABLE_OPENGL=OFF

        ${FEATURE_OPTIONS}
        ${WT_PLATFORM_SPECIFIC_OPTIONS}

        -DUSE_SYSTEM_SQLITE3=ON

        -DCMAKE_INSTALL_DIR=share
        -DWTHTTP_CONFIGURATION=
        -DCONFIGURATION=

        "-DUSERLIB_PREFIX=${CURRENT_INSTALLED_DIR}"
    MAYBE_UNUSED_VARIABLES
        USE_SYSTEM_SQLITE3
        INSTALL_CONFIG_FILE_PATH
)

vcpkg_cmake_install()
vcpkg_cmake_config_fixup()

# There is no way to suppress installation of the headers and resource files in debug build.
file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include" "${CURRENT_PACKAGES_DIR}/debug/share")

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/var" "${CURRENT_PACKAGES_DIR}/debug/var")

# RUNDIR is only used for wtfcgi what we don't build.
file(READ "${CURRENT_PACKAGES_DIR}/include/Wt/WConfig.h" W_CONFIG_H)
string(REGEX REPLACE "([\r\n])#define RUNDIR[^\r\n]+" "\\1// RUNDIR intentionally unset by vcpkg" W_CONFIG_H "${W_CONFIG_H}")
file(WRITE "${CURRENT_PACKAGES_DIR}/include/Wt/WConfig.h" "${W_CONFIG_H}")

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
vcpkg_copy_pdbs()
