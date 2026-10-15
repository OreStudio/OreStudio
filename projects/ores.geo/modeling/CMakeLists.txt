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
set(name "ores.geo")

set(diagram_target generate_${name}_diagram)
add_custom_target(${diagram_target}
    COMMENT "Generating PlantUML diagram for ${name}" VERBATIM
    COMMAND java ${ORES_JAVA_ARGS} -jar ${ORES_PLANTUML_JAR} ${name}.puml
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/)
add_dependencies(make_all_diagrams ${diagram_target})
