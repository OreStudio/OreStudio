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
#ifndef ORES_HTTP_ROUTES_REPORTING_REPORTING_CONFIGURATION_ROWS_HPP
#define ORES_HTTP_ROUTES_REPORTING_REPORTING_CONFIGURATION_ROWS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::reporting::reporting_configuration_rows {

/**
 * @brief A screen's write states its reason, or nothing when the form sends none.
 */
ores::utility::domain::change_intent
intent_or_none(const ores::http::net::record_routes::stated_intent& stated);

/**
 * @brief The party that holds a new configuration: the one the body names, else
 * the one the caller's token states.
 *
 * @param when_none The sentence a caller reads when neither names one, which
 * says what the caller can do about it.
 */
boost::uuids::uuid party_for_new(const ores::http::domain::http_request& req,
                                 const std::optional<std::string>& asked,
                                 std::string_view when_none);

/**
 * @brief The reporting configuration a create registers its record as.
 *
 * A reader finds the record by this configuration, and a report binds it
 * through it, so the two are written in this order and the document second: a
 * create whose second write is refused removes the configuration again.
 */
boost::uuids::uuid register_configuration(const ores::http::domain::http_request& req,
                                          ores::nats::service::nats_client& session,
                                          const std::string& name,
                                          std::string_view type_code,
                                          std::string_view owning_component,
                                          const ores::utility::domain::change_intent& intent);

/**
 * @brief Removes the configuration a failed create wrote, stating no failure of
 * its own.
 *
 * The create's own refusal is the one the caller reads, so a refusal here is
 * logged rather than answered.
 */
void remove_configuration(const ores::http::domain::http_request& req,
                          ores::nats::service::nats_client& session,
                          const std::string& name,
                          const ores::utility::domain::change_intent& intent);

}

#endif
