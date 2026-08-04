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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_OBSERVATION_LINEAGE_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_OBSERVATION_LINEAGE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/observation_lineage_protocol.hpp"
#include "ores.marketdata.core/service/observation_lineage_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& observation_lineage_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.observation_lineage_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for observation lineage operations.
 */
class observation_lineage_handler {
public:
    observation_lineage_handler(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(observation_lineage_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::observation_lineage_service svc(req_ctx);
        get_observation_lineages_response resp;
        if (auto req = decode<get_observation_lineages_request>(msg)) {
            try {
                resp.observation_lineages = svc.list_observation_lineages(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_observation_lineages());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(observation_lineage_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(observation_lineage_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(observation_lineage_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(observation_lineage_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::observation_lineages:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::observation_lineage_service svc(req_ctx);
        if (auto req = decode<save_observation_lineage_request>(msg)) {
            try {
                svc.save_observation_lineage(req->data);
                BOOST_LOG_SEV(observation_lineage_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, save_observation_lineage_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(observation_lineage_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      save_observation_lineage_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(observation_lineage_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(observation_lineage_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::observation_lineage_service svc(req_ctx);
        if (auto req = decode<get_observation_lineage_history_request>(msg)) {
            try {
                auto hist = svc.get_observation_lineage_history(req->id);
                BOOST_LOG_SEV(observation_lineage_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_observation_lineage_history_response{.history = std::move(hist),
                                                               .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(observation_lineage_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_observation_lineage_history_response{.success = false,
                                                               .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(observation_lineage_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(observation_lineage_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::observation_lineages:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::observation_lineage_service svc(req_ctx);
        if (auto req = decode<delete_observation_lineage_request>(msg)) {
            try {
                svc.delete_observation_lineages(req->ids);
                BOOST_LOG_SEV(observation_lineage_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, delete_observation_lineage_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(observation_lineage_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      delete_observation_lineage_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(observation_lineage_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::marketdata::messaging

#endif
