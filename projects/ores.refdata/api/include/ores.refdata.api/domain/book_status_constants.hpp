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

#ifndef ORES_REFDATA_API_DOMAIN_BOOK_STATUS_CONSTANTS_HPP
#define ORES_REFDATA_API_DOMAIN_BOOK_STATUS_CONSTANTS_HPP

#include <array>
#include <string_view>

namespace ores::refdata::domain::book_status_constants {

/**
 * @brief Book status codes used throughout the system.
 *
 * These codes must match entries in the ores_refdata_book_statuses_tbl
 * table (see refdata_book_statuses_populate.sql).
 */
namespace codes {

constexpr std::string_view active = "Active";
constexpr std::string_view closed = "Closed";
constexpr std::string_view frozen = "Frozen";

} // namespace codes

constexpr std::array<std::string_view, 3> all = {
    codes::active,
    codes::closed,
    codes::frozen,
};

} // namespace ores::refdata::domain::book_status_constants

#endif
