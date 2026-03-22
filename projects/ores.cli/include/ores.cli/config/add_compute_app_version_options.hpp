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
#ifndef ORES_CLI_CONFIG_ADD_COMPUTE_APP_VERSION_OPTIONS_HPP
#define ORES_CLI_CONFIG_ADD_COMPUTE_APP_VERSION_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <vector>
#include <optional>

namespace ores::cli::config {

/**
 * @brief Configuration for adding a compute app_version entity via command-line arguments.
 */
struct add_compute_app_version_options final {
    std::string app_id;
    std::string wrapper_version;
    std::string engine_version;
    std::vector<std::string> platforms;
    std::string modified_by;
    std::optional<std::string> package_uri;
    std::optional<int> min_ram_mb;
};

std::ostream& operator<<(std::ostream& s, const add_compute_app_version_options& v);

}

#endif
