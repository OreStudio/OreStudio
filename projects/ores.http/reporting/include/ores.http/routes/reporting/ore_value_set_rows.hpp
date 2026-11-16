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
#ifndef ORES_HTTP_ROUTES_REPORTING_ORE_VALUE_SET_ROWS_HPP
#define ORES_HTTP_ROUTES_REPORTING_ORE_VALUE_SET_ROWS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting::value_set_rows {

/**
 * @brief The choices of one catalogue relation, read from the collection that
 * holds it.
 *
 * A relation names a view in another component, so the gateway states which
 * relations it reads, in the part that answers the routes naming them. The three
 * the value set data names are read here; any other is refused with a sentence
 * that names it, rather than drawn as a list that is empty.
 */
[[nodiscard]] std::vector<ores::http::routes::value_choice>
catalogue_choices_of(const ores::http::domain::http_request& req,
                     ores::nats::service::nats_client& session,
                     std::string_view relation);

}

#endif
