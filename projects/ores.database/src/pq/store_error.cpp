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
#include "ores.database/pq/store_error.hpp"

namespace ores::database::pq {

error_kind classify(std::string_view sqlstate) {
    if (sqlstate.size() != 5)
        return error_kind::refused;
    const auto cls = sqlstate.substr(0, 2);
    if (cls == "08" || cls == "40")
        return error_kind::retryable;
    if (cls == "53")
        return sqlstate == "53100" ? error_kind::refused : error_kind::retryable;
    if (sqlstate == "57P01" || sqlstate == "57P02" || sqlstate == "57P03" || sqlstate == "55P03" ||
        sqlstate == "25006")
        return error_kind::retryable;
    return error_kind::refused;
}

store_error::store_error(std::string message, std::string sqlstate, error_kind kind)
    : std::runtime_error(std::move(message))
    , sqlstate_(std::move(sqlstate))
    , kind_(kind) {}

}
