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
#ifndef ORES_HISTORY_MESSAGING_HISTORY_HANDLER_HPP
#define ORES_HISTORY_MESSAGING_HISTORY_HANDLER_HPP

#include "ores.history/export.hpp"
#include "ores.history/messaging/history_protocol.hpp"
#include "ores.history/service/dispatch_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <functional>
#include <optional>
#include <string>

namespace ores::history::messaging {

namespace {

inline auto& history_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.history.messaging.history_handler");
    return instance;
}

} // namespace

/**
 * @brief Resolves an inbound history request's opaque caller_context
 * (see dispatch_registry::history_provider) from the raw NATS
 * message — typically validating a JWT and packing tenant_id
 * (and, where relevant, party_id/actor) into a provider-agreed
 * string. Returns nullopt to reject the request as unauthorized.
 *
 * Supplied by the composing service, which — unlike this
 * dependency-free leaf — has ores.security/ores.database available
 * to do the actual validation.
 */
using caller_context_resolver = std::function<std::optional<std::string>(const ores::nats::message&)>;

/**
 * @brief NATS handler for the one generic history subject.
 *
 * Decodes a get_entity_history_request, resolves the caller's opaque
 * context via the injected resolver, delegates to the injected
 * dispatch_registry (which never throws — see its dispatch() doc), and
 * replies with the resulting response. The registry itself owns all
 * per-entity dispatch logic; this class is purely the NATS decode/
 * resolve/reply glue shared by every entity, replacing what would
 * otherwise be N near-identical per-entity history() methods.
 */
class ORES_HISTORY_EXPORT history_handler final {
public:
    history_handler(ores::nats::service::client& nats,
                    const service::dispatch_registry& registry,
                    caller_context_resolver resolve_context)
        : nats_(nats), registry_(registry), resolve_context_(std::move(resolve_context)) {}

    void history(ores::nats::message msg) const {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::log_handler_entry;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        [[maybe_unused]] const auto correlation_id = log_handler_entry(history_handler_lg(), msg);
        auto req = decode<get_entity_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(history_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        auto caller_context = resolve_context_(msg);
        if (!caller_context) {
            BOOST_LOG_SEV(history_handler_lg(), warn) << "Unauthorized: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::unauthorized);
            return;
        }
        const auto resp = registry_.dispatch(*req, *caller_context);
        BOOST_LOG_SEV(history_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    const service::dispatch_registry& registry_;
    caller_context_resolver resolve_context_;
};

}

#endif
