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
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/trade_registrar.hpp"
#include "ores.trading.core/messaging/trade_type_registrar.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_trade_handlers(ores::nats::service::client& nats,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    // The trade entity's own verbs come from its generated registrar.
    auto subs = ores::trading::messaging::register_trade_handlers(nats, ctx, verifier);

    // Instrument reference data — floating index types and leg types moved
    // to ores.refdata (see ores.refdata.core/messaging/registrar.cpp); trade
    // types are handled by the entity-shaped trade_type handler stack.
    auto trade_type_subs = register_trade_type_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(trade_type_subs.begin()),
                std::make_move_iterator(trade_type_subs.end()));

    return subs;
}

}
