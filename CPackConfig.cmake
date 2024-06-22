# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be  useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.
#
include(CPack)
include(GNUInstallDirs)
include(InstallRequiredSystemLibraries)

set(CPACK_PACKAGE_NAME "${CMAKE_PROJECT_NAME}")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

set(CPACK_VERBATIM_VARIABLES YES)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "OreStudio - GUI for ORE.")
set(CPACK_PACKAGE_CONTACT "https://github.com/OreStudio/OreStudio")
set(CPACK_PACKAGE_VENDOR "OreStudio")

# note that ${public_headers} should be in quotes
# set_target_properties(${PROJECT_NAME} PROPERTIES PUBLIC_HEADER "${public_headers}")

# set_target_properties(${PROJECT_NAME} PROPERTIES DEBUG_POSTFIX "d")

# install the target and create export-set
# install(
#     TARGETS ${PROJECT_NAME}
#     EXPORT "${PROJECT_NAME}Targets"
#     COMPONENT ${PROJECT_NAME} # must be here, not any line lower
#     # these get default values from GNUInstallDirs, no need to set them
#     #RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} # bin
#     #LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} # lib
#     #ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} # lib
#     # except for public headers, as we want them to be inside a library folder
#     PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME} # include/SomeProject
#     INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} # include
# )

set(CPACK_PACKAGE_INSTALL_DIRECTORY ${CPACK_PACKAGE_NAME})
SET(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_SOURCE_DIR}/_packages")
set(CPACK_STRIP_FILES YES)

set(CPACK_INSTALL_DEFAULT_DIRECTORY_PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE)

# set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/some")#/${CMAKE_PROJECT_VERSION}")

set(CPACK_PACKAGE_CONTACT "marco.craveiro@gmail.com")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Marco Craveiro <${CPACK_PACKAGE_CONTACT}>")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")

# optionally, you can add various meta information to the components defined in INSTALLs
# cpack_add_component(some-application
#     DISPLAY_NAME "Some application"
#     DESCRIPTION "${CPACK_PACKAGE_DESCRIPTION_SUMMARY}"
#     #GROUP group1
# )
# cpack_add_component(SomeLibrary
#     DISPLAY_NAME "Some library"
#     DESCRIPTION "${CPACK_PACKAGE_DESCRIPTION_SUMMARY}"
#     #GROUP group1
# )
# cpack_add_component(AnotherLibrary
#     DISPLAY_NAME "Another library"
#     DESCRIPTION "${CPACK_PACKAGE_DESCRIPTION_SUMMARY}"
#     #GROUP group2
# )
# you can also put them into groups
#cpack_add_component_group(group1)
#cpack_add_component_group(group2)

# can be also set as -DCPACK_COMPONENTS_ALL="AnotherLibrary"
#set(CPACK_COMPONENTS_ALL "AnotherLibrary")
message(STATUS "Components to pack: ${CPACK_COMPONENTS_ALL}")

# if(NOT DEFINED CPACK_SYSTEM_NAME)
#     set(CPACK_SYSTEM_NAME "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
# endif()

#
# Component configuration
#
set(components applications)
set(CPACK_COMPONENTS_ALL ${components})

set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "VisualOre")
set(CPACK_COMPONENT_APPLICATIONS_DESCRIPTION "OreStudio - GUI for ORE.")
set(CPACK_COMPONENT_APPLICATIONS_GROUP "Runtime")
set(CPACK_COMPONENTS_IGNORE_GROUPS TRUE)
set(CPACK_ARCHIVE_COMPONENT_INSTALL TRUE)

#
# platform specific
#
if(WIN32)
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "${PROJECT_NAME}")

    if (MSVC)
        # don't forget to set the WIX path, e.g.:
        # WIX="C:/Program Files (x86)/WiX Toolset v3.10/bin" cpack -G WIX -C Release
        set(CPACK_GENERATOR "WIX")
        set(CPACK_PACKAGE_EXECUTABLES "ores.console" "Console tool")
        set(CPACK_WIX_PRODUCT_GUID "798CF43E-EF4E-1CB4-BFAB-25685E7E0F38")
        set(CPACK_WIX_UPGRADE_GUID "4D54BB3A-9CF6-25F4-AA1B-30E2969BBF36")
        # set(CPACK_WIX_PRODUCT_ICON "${CMAKE_SOURCE_DIR}/doc/images/application_dogen.png")
    else()
        set(CPACK_GENERATOR "NSIS")
        # set(CPACK_SOURCE_GENERATOR "ZIP")
        set(CPACK_PACKAGE_EXECUTABLES "ores.console" "Console tool")
        # set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}\\\\doc\\\\images\\\\application_dogen.png")

        if(CMAKE_CL_64)
            set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
        else()
            set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
        endif()

        # There is a bug in NSI that does not handle full unix paths properly. Make
        # sure there is at least one set of four (4) backslashes.
        # set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}\\\\images\\\\application_dogen.png")
        set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\ores.console.exe")
        set(CPACK_NSIS_DISPLAY_NAME "VisualOre")
        set(CPACK_NSIS_PACKAGE_NAME "VisualOre ${DOGEN_VERSION}")
        set(CPACK_NSIS_HELP_LINK "http:\\\\\\\\https://github.com/DomainDrivenConsulting/dogen/blob/master/doc/manual/manual.org")
        set(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\https://github.com/DomainDrivenConsulting/dogen/blob/master/doc/manual/manual.org")
        set(CPACK_NSIS_CONTACT "")
        set(CPACK_NSIS_MODIFY_PATH ON)
    endif()
elseif(APPLE)
    # set(CPACK_SOURCE_GENERATOR "TGZ")
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "${PROJECT_NAME}")
    # configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Info.plist.in ${stage_dir}/tmp_pkg/macosx/Info.plist)
    # configure_file(${CMAKE_CURRENT_SOURCE_DIR}/start.in ${stage_dir}/tmp_pkg/macosx/start)

    set(CPACK_GENERATOR "Bundle")
    # set(CPACK_BUNDLE_PLIST ${stage_dir}/tmp_pkg/macosx/Info.plist)
    # set(CPACK_BUNDLE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/application_dogen.icns)
    set(CPACK_BUNDLE_NAME "dogen")
    # set(CPACK_BUNDLE_STARTUP_COMMAND ${stage_dir}/tmp_pkg/macosx/start)
    set(CPACK_PACKAGE_EXECUTABLES "ores.console" "Console tool")

    set(CPACK_ALL_INSTALL_TYPES Full Developer)
    set(CPACK_COMPONENT_LIBRARIES_INSTALL_TYPES Developer Full)
    set(CPACK_COMPONENT_HEADERS_INSTALL_TYPES Developer Full)
    set(CPACK_COMPONENT_APPLICATIONS_INSTALL_TYPES Full)

elseif(UNIX)
    set(CPACK_GENERATOR "DEB")
    set(CPACK_STRIP_FILES "")
    set(CPACK_SOURCE_STRIP_FILES "")

    # package name for deb. If set, then instead of some-application-0.9.2-Linux.deb
    # you'll get some-application_0.9.2_amd64.deb (note the underscores too)
    set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
    # that is if you want every group to have its own package,
    # although the same will happen if this is not set (so it defaults to ONE_PER_GROUP)
    # and CPACK_DEB_COMPONENT_INSTALL is set to YES
    set(CPACK_COMPONENTS_GROUPING ALL_COMPONENTS_IN_ONE)#ONE_PER_GROUP)
    # without this you won't be able to pack only specified component
    set(CPACK_DEB_COMPONENT_INSTALL YES)
    # list dependencies
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS YES)

    set(CPACK_DEB_PACKAGE_DEBUG "1")
    set(CPACK_DEB_COMPONENT_INSTALL ON)
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Marco Craveiro")
    # set(CPACK_SOURCE_GENERATOR "TGZ")
    set(CPACK_DEBIAN_PACKAGE_PRIORITY "extra")
    set(CPACK_DEBIAN_PACKAGE_SECTION "devel")
    set(CPACK_DEBIAN_PACKAGE_RECOMMENDS "")
else()
    message(FATAL_ERROR "unknown operating system")
endif()
