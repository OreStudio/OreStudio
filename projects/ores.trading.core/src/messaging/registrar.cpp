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
#include "ores.trading.core/messaging/registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"

namespace ores::trading::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier,
    std::string http_base_url) {

    auto subs = detail::register_trade_handlers(nats, ctx, verifier, http_base_url);
    subs.reserve(113);

    auto rates = detail::register_rates_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
        std::make_move_iterator(rates.begin()),
        std::make_move_iterator(rates.end()));

    auto fx = detail::register_fx_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
        std::make_move_iterator(fx.begin()),
        std::make_move_iterator(fx.end()));

    auto other = detail::register_other_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
        std::make_move_iterator(other.begin()),
        std::make_move_iterator(other.end()));

    return subs;
}

}
