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
#ifndef ORES_TRADING_MESSAGING_REGISTRAR_DETAIL_HPP
#define ORES_TRADING_MESSAGING_REGISTRAR_DETAIL_HPP

#include <optional>
#include <string>
#include <vector>
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.trading.core/export.hpp"

namespace ores::trading::messaging::detail {

// Each function registers one instrument family's NATS handlers and returns
// the resulting subscriptions. Split by domain family for clarity and to allow
// parallel incremental builds across instrument families.

inline constexpr auto queue_name = "ores.trading.service";

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_trade_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier,
    const std::string& http_base_url);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_rates_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_fx_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_bond_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_credit_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_equity_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_commodity_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_composite_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

ORES_TRADING_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_scripted_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier);

}

#endif
