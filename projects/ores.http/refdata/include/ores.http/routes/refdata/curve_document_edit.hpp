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
#ifndef ORES_HTTP_ROUTES_REFDATA_CURVE_DOCUMENT_EDIT_HPP
#define ORES_HTTP_ROUTES_REFDATA_CURVE_DOCUMENT_EDIT_HPP

#include "ores.http/routes/refdata/curve_document_cells.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include <string_view>

namespace ores::http::routes::refdata::document {

/**
 * @brief Writes one column of the row an entity and a key name, and answers
 * whether the document holds that row.
 *
 * A change names a leaf by the table that holds it, the entry it belongs to and
 * the column, so the caller finds the row the same way the screen does. The text
 * is read into the member's own type, and a text that is not one is a refusal:
 *
 * @throws std::invalid_argument when the text is not a value the column takes.
 */
[[nodiscard]] bool write_column(ores::refdata::messaging::curve_configuration_document& document,
                                std::string_view vector,
                                std::string_view key_column,
                                std::string_view key,
                                std::string_view column,
                                const cell& value);

/**
 * @brief Copies the rows of one document vector into another document.
 *
 * A save writes every family the document carries, so the families the screen
 * did not change are written empty and keep their versions. The caller copies
 * the ones it touched.
 */
[[nodiscard]] bool copy_vector(const ores::refdata::messaging::curve_configuration_document& from,
                               std::string_view vector,
                               ores::refdata::messaging::curve_configuration_document& into);

}

#endif
