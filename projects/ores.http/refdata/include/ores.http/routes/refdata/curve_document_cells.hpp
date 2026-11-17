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
#ifndef ORES_HTTP_ROUTES_REFDATA_CURVE_DOCUMENT_CELLS_HPP
#define ORES_HTTP_ROUTES_REFDATA_CURVE_DOCUMENT_CELLS_HPP

#include "ores.http/routes/refdata/wire_cells.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata::document {

/**
 * @brief The curve configuration document, flattened into the rows the screen
 * joins.
 *
 * The document arrives typed, so a row's columns are its members. The wiring
 * names a column as text, and the screen joins that column to the contract entry
 * of the leaf it holds. This is where the two meet: every row of every vector
 * becomes cells the wiring reads by name, and a column a row does not carry stays
 * absent rather than becoming empty text.
 */
class cells final {
public:
    explicit cells(const ores::refdata::messaging::curve_configuration_document& document);

    /// The header row, which is the document's own configuration row.
    [[nodiscard]] const row& header() const noexcept {
        return header_;
    }

    /**
     * @brief The rows of one document vector, or nothing for a name this
     * document does not carry.
     */
    [[nodiscard]] const std::vector<row>* find(std::string_view vector) const;

    /// The rows of one vector, empty for a name the document does not carry.
    [[nodiscard]] const std::vector<row>& at(std::string_view vector) const;

private:
    row header_;
    std::map<std::string, std::vector<row>, std::less<>> rows_;
};

}

#endif
