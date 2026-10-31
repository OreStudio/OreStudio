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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_MARKET_STATE_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_MARKET_STATE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& market_state_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.market_state_handler");
    return instance;
}
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Keeps the market state a run priced against.
 *
 * A cube is far too large to travel in a message, so the caller names the run's
 * output archives and lends the token that reads them. This downloads each
 * archive, unpacks it and stores every market-state file it holds as one
 * compressed payload with an index of what it covers.
 */
class ORES_MARKETDATA_CORE_EXPORT market_state_handler {
public:
    market_state_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Stores the payloads of one run.
     *
     * @param msg       the encoded request.
     * @param http_base_url where the storage service answers.
     */
    void store(ores::nats::message msg, const std::string& http_base_url);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
