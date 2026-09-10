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
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.trading.core/messaging/lifecycle_event_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/lifecycle_event_registrar.hpp"
#include "ores.trading.core/messaging/party_role_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/party_role_type_registrar.hpp"
#include "ores.trading.core/messaging/registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/trade_id_type_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_id_type_registrar.hpp"
#include "ores.trading.core/messaging/trade_identifier_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_identifier_registrar.hpp"
#include "ores.trading.core/messaging/trade_party_role_history_provider_registrar.hpp"
#include "ores.trading.core/messaging/trade_party_role_registrar.hpp"
#include "ores.trading.core/messaging/trade_type_history_provider_registrar.hpp"

namespace ores::trading::messaging {

namespace {

constexpr std::string_view queue_group = "ores.trading.service";

// The dispatch registry is process-static: the history handler's
// subscription outlives this function, so the registry it references
// must outlive the subscription too.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::string http_base_url) {

    auto subs = detail::register_trade_handlers(nats, ctx, verifier, http_base_url);
    // Capacity for the ~250 subscriptions the masters below return.
    subs.reserve(256);

    const auto append = [&subs](auto vec) {
        subs.insert(
            subs.end(), std::make_move_iterator(vec.begin()), std::make_move_iterator(vec.end()));
    };

    append(detail::register_rates_handlers(nats, ctx, verifier));
    append(detail::register_fx_handlers(nats, ctx, verifier));
    append(detail::register_bond_handlers(nats, ctx, verifier));
    append(detail::register_credit_handlers(nats, ctx, verifier));
    append(detail::register_equity_handlers(nats, ctx, verifier));
    append(detail::register_commodity_handlers(nats, ctx, verifier));
    append(detail::register_composite_handlers(nats, ctx, verifier));
    append(detail::register_scripted_handlers(nats, ctx, verifier));
    append(register_party_role_type_handlers(nats, ctx, verifier));
    append(register_trade_id_type_handlers(nats, ctx, verifier));
    append(register_lifecycle_event_handlers(nats, ctx, verifier));
    append(register_trade_identifier_handlers(nats, ctx, verifier));
    append(register_trade_party_role_handlers(nats, ctx, verifier));

    auto& hist_registry = history_registry();
    register_party_role_type_history_provider(hist_registry);
    register_trade_id_type_history_provider(hist_registry);
    register_lifecycle_event_history_provider(hist_registry);
    register_trade_identifier_history_provider(hist_registry);
    register_trade_party_role_history_provider(hist_registry);
    register_trade_type_history_provider(hist_registry);
    subs.push_back(ores::history::messaging::register_history_handlers(
        nats, hist_registry, "trading", queue_group, ctx, verifier));

    return subs;
}

}
