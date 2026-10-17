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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# This directory is added from projects/CMakeLists.txt, not from the top level.
# That is deliberate: it inherits that directory's compile options, so the
# benchmark is built with the same warnings, the same -Werror and the same
# build type as the code it measures. A benchmark that quietly compiles at a
# different optimisation level measures a program nobody ships.
#
# There is no add_test here on purpose. The benchmark takes minutes and is not
# a pass or fail, so ctest does not run it. See README.org for how it is run.

set(bench_target_name ores.ore.xml.benchmarks)

add_executable(${bench_target_name} bench.cpp)

target_link_libraries(${bench_target_name}
    PRIVATE
        ores.ore.core.lib
        pugixml::pugixml)
