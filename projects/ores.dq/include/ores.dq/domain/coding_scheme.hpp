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
#ifndef ORES_DQ_DOMAIN_CODING_SCHEME_HPP
#define ORES_DQ_DOMAIN_CODING_SCHEME_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Defines a coding or identification standard used to identify entities.
 *
 * A scheme defines a coding or identification standard used to identify
 * entities such as parties, currencies, or other domain objects. Based on
 * the FPML coding-scheme concept.
 */
struct coding_scheme final {
    int version = 0;
    std::string code;
    std::string name;
    std::string authority_type;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> uri;
    std::string description;
    std::string recorded_by;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
