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
#ifndef ORES_HTTP_ROUTES_REFDATA_CURVE_AMEND_RULES_HPP
#define ORES_HTTP_ROUTES_REFDATA_CURVE_AMEND_RULES_HPP

#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http/routes/refdata/curve_document_cells.hpp"
#include <optional>
#include <string>

namespace ores::http::routes::refdata::amend {

/**
 * @brief Why the leaf would not take the value, or nothing when it would.
 *
 * The value set tables are the ORE parser's own lists, so a value outside them is
 * one ORE refuses to read. The store's guard does not cover the curve tables yet,
 * so this boundary is where the refusal happens.
 *
 * The sentence names the leaf's words rather than its column, because the person
 * who reads it reads the screen and not the table.
 */
[[nodiscard]] std::optional<std::string> refusal_of(const ores::http::routes::value_fields& fields,
                                                    const std::string& label,
                                                    bool required,
                                                    const document::cell& value);

/**
 * @brief Whether a service's refusal says the row moved on since the caller read
 * it.
 *
 * The store states a version it will not write with words rather than a code, so
 * the three spellings it uses are read here instead of leaving every amendment
 * that loses a race as a bad request.
 */
[[nodiscard]] bool is_stale(const std::string& message);

}

#endif
