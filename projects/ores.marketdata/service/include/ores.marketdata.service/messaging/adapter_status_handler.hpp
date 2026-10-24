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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_ADAPTER_STATUS_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_ADAPTER_STATUS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.service/app/adapter_supervisor.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <memory>
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& adapter_status_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.adapter_status_handler");
    return instance;
}
}

/**
 * @brief NATS request/reply handler for list_adapter_status_request: the state of every adapter
 * that the supervisor tracks, with the ids it accepted and rejected in its last reply.
 *
 * Reading the status needs the permission to read market data sources, because an adapter is
 * the running side of a source.
 */
class adapter_status_handler {
public:
    adapter_status_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier,
        std::shared_ptr<ores::marketdata::service::app::adapter_supervisor> supervisor)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , supervisor_(std::move(supervisor)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            ores::service::messaging::log_handler_entry(adapter_status_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            ores::service::messaging::error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx_expected,
                                                      "marketdata::market_data_sources:read")) {
            ores::service::messaging::error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        list_adapter_status_response resp;
        resp.adapters = supervisor_->status();
        resp.success = true;
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::shared_ptr<ores::marketdata::service::app::adapter_supervisor> supervisor_;
};

}

#endif
