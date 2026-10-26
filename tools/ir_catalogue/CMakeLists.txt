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
# A development tool, not installed: it writes the generated half of
# projects/ores.sql/populate/marketdata/marketdata_vendor_instruments_populate.sql.
set(tool_name "ores.marketdata.ir-catalogue")

add_executable(${tool_name} main.cpp)
set_target_properties(${tool_name} PROPERTIES OUTPUT_NAME ${tool_name})

target_link_libraries(${tool_name}
    PRIVATE
        ores.marketdata.core.lib
        ores.synthetic.api.lib
        ores.openssl_cleanup
        Threads::Threads)
