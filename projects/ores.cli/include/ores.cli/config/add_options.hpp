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
#ifndef ORES_CLI_CONFIG_ADD_OPTIONS_HPP
#define ORES_CLI_CONFIG_ADD_OPTIONS_HPP

#include <iosfwd>
#include <variant>
#include "ores.cli/config/add_currency_options.hpp"
#include "ores.cli/config/add_account_options.hpp"
#include "ores.cli/config/add_feature_flag_options.hpp"
#include "ores.cli/config/add_login_info_options.hpp"

namespace ores::cli::config {

/**
 * @brief Variant type holding entity-specific add options.
 *
 * This design allows each entity type to have its own dedicated options struct
 * with strongly-typed required fields, while maintaining a unified interface
 * for the add operation. Use std::visit to dispatch to entity-specific handlers.
 */
using add_options = std::variant<
    add_currency_options,
    add_account_options,
    add_feature_flag_options,
    add_login_info_options
>;

std::ostream& operator<<(std::ostream& s, const add_options& v);

}

#endif
