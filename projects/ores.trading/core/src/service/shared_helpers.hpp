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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TRADING_CORE_SRC_SERVICE_SHARED_HELPERS_HPP_
#define ORES_TRADING_CORE_SRC_SERVICE_SHARED_HELPERS_HPP_

#include <string>
#include <vector>

namespace ores::trading::service {

inline std::string to_uuid_array(const std::vector<std::string>& ids) {
    std::string r = "{";
    for (std::size_t i = 0; i < ids.size(); ++i) {
        if (i != 0)
            r += ',';
        r += ids[i];
    }
    return r + "}";
}

}

#endif
