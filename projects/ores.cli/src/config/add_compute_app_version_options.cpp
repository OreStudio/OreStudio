/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.cli/config/add_compute_app_version_options.hpp"

#include <ostream>
#include <numeric>

namespace ores::cli::config {

std::ostream& operator<<(std::ostream& s, const add_compute_app_version_options& v) {
    s << "{ app_id: " << v.app_id
      << ", wrapper_version: " << v.wrapper_version
      << ", engine_version: " << v.engine_version
      << ", platforms: [" << std::accumulate(v.platforms.begin(), v.platforms.end(),
             std::string{}, [](const std::string& a, const std::string& b) {
                 return a.empty() ? b : a + ", " + b; }) << "]"
      << ", modified_by: " << v.modified_by;

    if (v.package_uri) s << ", package_uri: " << *v.package_uri;
    if (v.min_ram_mb) s << ", min_ram_mb: " << *v.min_ram_mb;

    s << " }";
    return s;
}

}
