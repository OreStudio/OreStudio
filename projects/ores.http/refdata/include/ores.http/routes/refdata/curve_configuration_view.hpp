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
#ifndef ORES_HTTP_ROUTES_REFDATA_CURVE_CONFIGURATION_VIEW_HPP
#define ORES_HTTP_ROUTES_REFDATA_CURVE_CONFIGURATION_VIEW_HPP

#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http/routes/refdata/curve_document_cells.hpp"
#include "ores.refdata.api/domain/curve_section.hpp"
#include "ores.reporting.api/domain/ore_parameter.hpp"
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata::curve_view {

/**
 * @brief The ORE parameter contract: the leaves of one schema, by the path the
 * wiring names.
 */
using contract =
    std::map<std::string, const ores::reporting::domain::ore_parameter*, std::less<>>;

/**
 * @brief What a document is drawn against, besides its own rows.
 *
 * A leaf's words and the values it accepts are the contract's and the value
 * sets', and a segment's element is its type's kind. None of the three belongs
 * to the document, so a caller reads them once and every section draws from
 * them.
 */
struct vocabulary final {
    const contract& parameters;
    const ores::http::routes::value_set_reader& values;

    /// The ORE element each segment type code is written as, such as Simple.
    const std::map<std::string, std::string>& segment_kinds;
};

/**
 * @brief One curve configuration document, as the screen draws it, as one JSON
 * object.
 *
 * The sections are the curve section table's, in the order the schema declares
 * them, so a section no wiring maps is still listed and says so. An entry is one
 * definition row and the rows of its own section's tables, and its leaves are
 * the wiring's columns joined to the contract's words and the value sets' values.
 *
 * @param sections The curve section rows, in any order: the order is the
 * wiring's.
 */
[[nodiscard]] std::string
view_json_of(const document::cells& document,
             std::string_view configuration_id,
             const std::vector<ores::refdata::domain::curve_section>& sections,
             const vocabulary& words);

}

#endif
