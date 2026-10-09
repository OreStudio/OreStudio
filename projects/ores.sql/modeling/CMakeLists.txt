# -*- mode: cmake; tab-width: 4; indent-tabs-mode: nil -*-
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(name "ores_schema")

# The ER diagram is one file per group plus an index over them, so the
# target draws every one. SVG, because the schema has no raster budget left
# at 463 tables, and the images are not committed: PlantUML bakes the
# installed build's layout into them, and the .puml files are the artefacts.
string(REPLACE ";" " " er_java_args "${ORES_JAVA_ARGS}")

set(diagram_target generate_${name}_diagram)
add_custom_target(${diagram_target}
    COMMENT "Generating PlantUML ER diagrams for ${name}" VERBATIM
    COMMAND bash -c
            "java ${er_java_args} -jar ${ORES_PLANTUML_JAR} -tsvg ${name}*.puml"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/)
add_dependencies(make_all_diagrams ${diagram_target})
