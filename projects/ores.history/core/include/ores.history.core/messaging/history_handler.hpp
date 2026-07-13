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

#include "ores.database/domain/context.hpp"
#include "ores.history.core/export.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::history::messaging {

namespace {

inline auto& history_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.history.messaging.history_handler");
    return instance;
}

} // namespace

/**
 * @brief NATS handler for the one generic history subject.
 *
 * Resolves the caller's request-scoped context exactly like every
 * other handler in the codebase (make_request_context), decodes a
 * get_entity_history_request, delegates to the injected
 * dispatch_registry (which never throws — see its dispatch() doc), and
 * replies with the resulting response. The registry itself owns all
 * per-entity dispatch logic; this class is purely the NATS
 * auth/decode/reply glue shared by every entity, replacing what would
 * otherwise be N near-identical per-entity history() methods.
 */
class ORES_HISTORY_CORE_EXPORT history_handler final {
public:
    history_handler(ores::nats::service::client& nats,
                    const service::dispatch_registry& registry,
                    ores::database::context ctx,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), registry_(registry), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void history(ores::nats::message msg) const {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::log_handler_entry;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        [[maybe_unused]] const auto correlation_id = log_handler_entry(history_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            BOOST_LOG_SEV(history_handler_lg(), warn) << "Unauthorized: " << msg.subject;
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        auto req = decode<get_entity_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(history_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto resp = registry_.dispatch(*req, *req_ctx_expected);
        BOOST_LOG_SEV(history_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    const service::dispatch_registry& registry_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
