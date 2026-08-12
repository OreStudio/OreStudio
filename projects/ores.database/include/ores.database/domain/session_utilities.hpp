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
#ifndef ORES_DATABASE_DOMAIN_SESSION_UTILITIES_HPP
#define ORES_DATABASE_DOMAIN_SESSION_UTILITIES_HPP

#include <sqlgen/Result.hpp>
#include <string>

namespace ores::database::domain {

/**
 * @brief Forces a UTC session TimeZone on any connection or session.
 *
 * Every application DB session must be UTC: the bitemporal sentinel's
 * naive-literal condition (=valid_to = '9999-12-31 23:59:59'=) only
 * folds to the sentinel instant in a UTC session, and the sentinel
 * function's explicit =+00= offset must not be provable equivalent to
 * a differently-folded literal (task 3828BF82). One helper for every
 * post-connect site (the tenant-aware pool at acquisition, the
 * listener service's dedicated connection) so a future connection
 * site cannot forget the forcing.
 *
 * @param _executor anything exposing execute(const std::string&) --
 *        sqlgen Connection or Session.
 */
template <class T>
sqlgen::Result<sqlgen::Nothing> force_session_utc(T& _executor) noexcept {
    auto result = _executor.execute("SELECT set_config('TimeZone', 'UTC', false)");
    if (!result) {
        return sqlgen::error("Failed to set session timezone to UTC: " +
                             std::string(result.error().what()));
    }
    return sqlgen::Nothing{};
}

}

#endif
