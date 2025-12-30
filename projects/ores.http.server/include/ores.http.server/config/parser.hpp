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
#ifndef ORES_HTTP_SERVER_CONFIG_PARSER_HPP
#define ORES_HTTP_SERVER_CONFIG_PARSER_HPP

#include <vector>
#include <string>
#include <optional>
#include <ostream>
#include "ores.http.server/config/options.hpp"

namespace ores::http_server::config {

/**
 * @brief Parses command line options for the HTTP server.
 */
class parser final {
public:
    /**
     * @brief Parses the command line arguments.
     * @return Configuration options if successful, empty optional if help/version shown.
     */
    std::optional<options> parse(const std::vector<std::string>& arguments,
        std::ostream& info, std::ostream& err) const;
};

}

#endif
