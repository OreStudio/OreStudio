# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# The TypeScript web interface has no compiled targets of its own, so this
# file only wires the npm build into the CMake build: `compass services start`
# runs the built BFF from packages/bff/dist, and the fleet expects it to exist
# after a build, in the same way it expects the C++ binaries in publish/bin.
#
# A build host without npm skips the component with a warning rather than
# failing the configure or the build, because the C++ tree neither links nor
# tests anything here.
option(ORES_WEB_ENABLED "Build the TypeScript web interface (ores.web)" ON)

if(ORES_WEB_ENABLED)
    find_program(ORES_NPM_EXECUTABLE npm)
    if(NOT ORES_NPM_EXECUTABLE)
        message(WARNING
            "npm was not found, so the ores.web TypeScript component will not "
            "be built. Install Node.js 22 or later to build and serve it.")
        return()
    endif()

    set(ORES_WEB_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    set(ORES_WEB_DEPENDENCIES_STAMP
        ${CMAKE_CURRENT_BINARY_DIR}/ores_web_dependencies.stamp)

    # npm ci is the reproducible install, so it runs only when the manifest or
    # the lockfile changes; every other build reuses the installed tree.
    add_custom_command(
        OUTPUT ${ORES_WEB_DEPENDENCIES_STAMP}
        COMMAND ${ORES_NPM_EXECUTABLE} ci --no-audit --no-fund
        COMMAND ${CMAKE_COMMAND} -E touch ${ORES_WEB_DEPENDENCIES_STAMP}
        DEPENDS ${ORES_WEB_DIR}/package.json
                ${ORES_WEB_DIR}/package-lock.json
        WORKING_DIRECTORY ${ORES_WEB_DIR}
        COMMENT "Installing ores.web dependencies"
        VERBATIM)

    add_custom_target(ores_web ALL
        COMMAND ${ORES_NPM_EXECUTABLE} run build
        DEPENDS ${ORES_WEB_DEPENDENCIES_STAMP}
        WORKING_DIRECTORY ${ORES_WEB_DIR}
        COMMENT "Building the ores.web TypeScript component"
        VERBATIM)
endif()
