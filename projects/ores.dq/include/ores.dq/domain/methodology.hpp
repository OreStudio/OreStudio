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
#ifndef ORES_DQ_DOMAIN_METHODOLOGY_HPP
#define ORES_DQ_DOMAIN_METHODOLOGY_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::dq::domain {

/**
 * @brief Describes a methodology for data processing or transformation.
 */
struct methodology final {
    int version = 0;
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    std::optional<std::string> logic_reference;
    std::optional<std::string> implementation_details;
    std::string recorded_by;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
