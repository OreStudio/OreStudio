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
#include "ores.http/routes/refdata/refdata_routes.hpp"
#include "ores.http/routes/refdata/business_centre_routes.hpp"
#include "ores.http/routes/refdata/business_unit_routes.hpp"
#include "ores.http/routes/refdata/business_unit_type_routes.hpp"
#include "ores.http/routes/refdata/calendar_event_routes.hpp"
#include "ores.http/routes/refdata/calendar_exception_routes.hpp"
#include "ores.http/routes/refdata/calendar_routes.hpp"
#include "ores.http/routes/refdata/calendar_rule_routes.hpp"
#include "ores.http/routes/refdata/counterparty_routes.hpp"
#include "ores.http/routes/refdata/country_routes.hpp"
#include "ores.http/routes/refdata/currency_calendar_routes.hpp"
#include "ores.http/routes/refdata/currency_country_routes.hpp"
#include "ores.http/routes/refdata/currency_currency_group_routes.hpp"
#include "ores.http/routes/refdata/currency_group_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_convention_calendar_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_convention_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_routes.hpp"
#include "ores.http/routes/refdata/currency_routes.hpp"
#include "ores.http/routes/refdata/ir_curve_bootstrap_config_routes.hpp"
#include "ores.http/routes/refdata/ir_curve_bootstrap_pillar_routes.hpp"
#include "ores.http/routes/refdata/party_contact_information_routes.hpp"
#include "ores.http/routes/refdata/party_counterparty_routes.hpp"
#include "ores.http/routes/refdata/party_country_routes.hpp"
#include "ores.http/routes/refdata/party_currency_routes.hpp"
#include "ores.http/routes/refdata/party_id_scheme_routes.hpp"
#include "ores.http/routes/refdata/party_identifier_routes.hpp"
#include "ores.http/routes/refdata/party_routes.hpp"

namespace ores::http::routes::refdata {

void refdata_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {

    business_centre_routes::register_routes(router, registry, session);
    business_unit_routes::register_routes(router, registry, session);
    business_unit_type_routes::register_routes(router, registry, session);
    calendar_routes::register_routes(router, registry, session);
    calendar_event_routes::register_routes(router, registry, session);
    calendar_exception_routes::register_routes(router, registry, session);
    calendar_rule_routes::register_routes(router, registry, session);
    counterparty_routes::register_routes(router, registry, session);
    country_routes::register_routes(router, registry, session);
    currency_routes::register_routes(router, registry, session);
    currency_calendar_routes::register_routes(router, registry, session);
    currency_country_routes::register_routes(router, registry, session);
    currency_currency_group_routes::register_routes(router, registry, session);
    currency_group_routes::register_routes(router, registry, session);
    currency_pair_routes::register_routes(router, registry, session);
    currency_pair_convention_routes::register_routes(router, registry, session);
    currency_pair_convention_calendar_routes::register_routes(router, registry, session);
    ir_curve_bootstrap_config_routes::register_routes(router, registry, session);
    ir_curve_bootstrap_pillar_routes::register_routes(router, registry, session);
    party_routes::register_routes(router, registry, session);
    party_contact_information_routes::register_routes(router, registry, session);
    party_counterparty_routes::register_routes(router, registry, session);
    party_country_routes::register_routes(router, registry, session);
    party_currency_routes::register_routes(router, registry, session);
    party_id_scheme_routes::register_routes(router, registry, session);
    party_identifier_routes::register_routes(router, registry, session);
}

}
