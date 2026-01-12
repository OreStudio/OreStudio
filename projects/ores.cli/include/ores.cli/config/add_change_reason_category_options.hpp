/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_CLI_CONFIG_ADD_CHANGE_REASON_CATEGORY_OPTIONS_HPP
#define ORES_CLI_CONFIG_ADD_CHANGE_REASON_CATEGORY_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <optional>

namespace ores::cli::config {

/**
 * @brief Configuration for adding a change reason category entity via command-line arguments.
 */
struct add_change_reason_category_options final {
    std::string code;
    std::string description;
    std::string recorded_by;
    std::optional<std::string> change_commentary;
};

std::ostream& operator<<(std::ostream& s, const add_change_reason_category_options& v);

}

#endif
