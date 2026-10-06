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
#ifndef ORES_DATABASE_REPOSITORY_VALID_AT_HPP
#define ORES_DATABASE_REPOSITORY_VALID_AT_HPP

#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include <optional>
#include <string>

namespace ores::database::repository {

/**
 * @brief A row whose validity window holds the instant @p as_of.
 *
 * With an instant, the row valid then: valid_from at or before it and valid_to
 * after it, so a version closed at that very instant is not the one read. With
 * none, the current row, whose valid_to is the open-ended maximum. The instant
 * is compared by the database, so it must be a timestamp it reads; callers
 * parse it first.
 */
inline sqlgen::dynamic::Condition valid_at(const std::optional<std::string>& as_of) {
    if (!as_of)
        return equals("valid_to", filter_value(std::string(MAX_TIMESTAMP)));
    return detail::both(
        {.val =
             sqlgen::dynamic::Condition::LesserEqual{.op1 = detail::column("valid_from"),
                                                     .op2 = detail::value(filter_value(*as_of))}},
        {.val = sqlgen::dynamic::Condition::GreaterThan{
             .op1 = detail::column("valid_to"), .op2 = detail::value(filter_value(*as_of))}});
}

}

#endif
