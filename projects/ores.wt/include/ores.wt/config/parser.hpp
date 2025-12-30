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
#ifndef ORES_WT_CONFIG_PARSER_HPP
#define ORES_WT_CONFIG_PARSER_HPP

#include <iosfwd>
#include <vector>
#include <string>
#include <optional>
#include "ores.wt/config/options.hpp"

namespace ores::wt::config {

/**
 * @brief Command-line parser for ores.wt using boost program options.
 *
 * Parses database and logging options. Wt-specific options (--docroot,
 * --http-address, etc.) are passed through to the Wt framework.
 */
class parser final {
public:
    struct parse_result {
        std::optional<options> opts;
        std::vector<std::string> wt_args;
    };

    parse_result parse(const std::vector<std::string>& arguments,
        std::ostream& info, std::ostream& error) const;
};

}

#endif
