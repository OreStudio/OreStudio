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
#ifndef ORES_HTTP_ROUTES_REFDATA_CATALOGUE_CHOICES_HPP
#define ORES_HTTP_ROUTES_REFDATA_CATALOGUE_CHOICES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata::catalogue {

/**
 * @brief The choices one catalogue relation offers, read from the collection
 * that holds it.
 *
 * A relation names a view in another component, so the part that answers a route
 * states which relations it reads. The value set data names two relations for
 * the reference data screens, the tenors and the currencies; a row naming another
 * one is a seed error, and it is refused with a sentence rather than drawn as an
 * empty list.
 *
 * @throws ores::http::routes::value_set_error when no reader knows the relation.
 */
[[nodiscard]] std::vector<ores::http::routes::value_choice>
choices_of(const ores::http::domain::http_request& req,
           ores::nats::service::nats_client& session,
           std::string_view relation);

}

#endif
