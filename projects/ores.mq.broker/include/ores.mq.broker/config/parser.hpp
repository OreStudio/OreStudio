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
#ifndef ORES_MQ_BROKER_CONFIG_PARSER_HPP
#define ORES_MQ_BROKER_CONFIG_PARSER_HPP

#include <iosfwd>
#include <vector>
#include <string>
#include <optional>
#include "ores.mq.broker/config/options.hpp"

namespace ores::mq::broker::config {

/**
 * @brief Command-line parser for the broker process.
 *
 * Note on logging: the logger is only initialised after options are parsed,
 * so this class writes to ostreams directly rather than using the logger.
 */
class parser final {
public:
    std::optional<options>
    parse(const std::vector<std::string>& arguments, std::ostream& info,
        std::ostream& error) const;
};

}

#endif
