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
#ifndef ORES_HTTP_ROUTES_REFDATA_CONVENTION_TERMS_HPP
#define ORES_HTTP_ROUTES_REFDATA_CONVENTION_TERMS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http/routes/refdata/wire_cells.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata::terms {

/**
 * @brief One term of one convention kind, as a screen draws it.
 *
 * A term is a column of the row, joined to the ORE parameter contract entry its
 * schema and XPath name, and to the value set that entry links. A term the
 * contract holds no entry for is still a term, marked undocumented, so the gap
 * shows rather than the column disappearing.
 */
struct term_view final {
    std::string entity;
    std::string column;
    std::string scope;
    std::string ore;
    std::string xpath;
    document::cell value;
    bool documented = false;
    bool required = false;
    std::string label;
    std::string help;
    std::string description;
    std::string default_text;
    std::string format;
    std::string introduced_in;
    std::string removed_in;
    ores::http::routes::value_fields fields;
};

/**
 * @brief Every term of one kind that the row's form holds.
 *
 * A kind states its terms once, but they live on its parts: a tenor basis swap
 * holds either the pay and receive form or the long and short one, and two terms
 * are named the same on both. A term of a form the row does not hold is not the
 * row's term, so the parts the row holds choose the terms. A row that holds no
 * part keeps every term the kind states.
 *
 * @param row_json The row, flattened, as the wire wrote it.
 * @param held The entities of the composite the row holds, empty for a family
 * with no parts.
 */
[[nodiscard]] std::vector<term_view> read(const ores::http::domain::http_request& req,
                                          ores::nats::service::nats_client& session,
                                          std::string_view kind_code,
                                          std::string_view row_json,
                                          const std::set<std::string, std::less<>>& held);

/// One term as the JSON object the screen reads.
[[nodiscard]] std::string json_of(const term_view& term);

/// The terms as the JSON array the screen reads.
[[nodiscard]] std::string json_of(const std::vector<term_view>& terms);

/**
 * @brief Puts a refusal on the term it names.
 *
 * The store refuses a term's value with the term's scope and name, as in
 * "Invalid value X for Swap.FixedFrequency: ...", and names no field. The
 * sentence is the one the validation function raises, so the term it names is
 * found by that wording and the refusal is put on the term's column; a refusal
 * that names no term keeps no field and stays on the card.
 */
void name_term(ores::utility::domain::result& result, const std::vector<term_view>& terms);

}

#endif
