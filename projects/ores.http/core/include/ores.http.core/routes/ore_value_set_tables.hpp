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
#ifndef ORES_HTTP_CORE_ROUTES_ORE_VALUE_SET_TABLES_HPP
#define ORES_HTTP_CORE_ROUTES_ORE_VALUE_SET_TABLES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.core/export.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace ores::http::routes {

/**
 * @brief The four value set tables, read through the caller's session.
 *
 * The reader takes rows rather than a session, because each part of the gateway
 * reads the value sets for its own routes. This is where those rows come from,
 * so every part hands the reader the same four tables.
 */
[[nodiscard]] ORES_HTTP_CORE_EXPORT value_set_tables
read_value_set_tables(const ores::http::domain::http_request& req,
                      ores::nats::service::nats_client& session);

/**
 * @brief The allowed values of one setting, as the JSON members a screen reads.
 *
 * One setting's rule is written one way whatever route states it, so a form
 * checks a value against the same words the documentation page shows.
 */
[[nodiscard]] ORES_HTTP_CORE_EXPORT std::vector<std::string>
value_fields_members(const value_fields& fields);

}

#endif
