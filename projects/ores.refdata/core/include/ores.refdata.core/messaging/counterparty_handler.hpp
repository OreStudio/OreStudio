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
#ifndef ORES_REFDATA_CORE_MESSAGING_COUNTERPARTY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_COUNTERPARTY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.core/service/counterparty_contact_information_service.hpp"
#include "ores.refdata.core/service/counterparty_identifier_service.hpp"
#include "ores.refdata.core/service/counterparty_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <chrono>
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& counterparty_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.refdata.messaging.counterparty_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for counterparty operations.
 */
class counterparty_handler {
public:
    counterparty_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::counterparty_service svc(req_ctx);
        get_counterparties_response resp;
        if (auto req = decode<get_counterparties_request>(msg)) {
            try {
                resp.counterparties = svc.list_counterparties(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_counterparties());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::counterparty_service svc(req_ctx);
        if (auto req = decode<save_counterparty_request>(msg)) {
            try {
                svc.save_counterparty(req->data);
                BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, save_counterparty_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(
                    nats_, msg, save_counterparty_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::counterparty_service svc(req_ctx);
        if (auto req = decode<get_counterparty_history_request>(msg)) {
            try {
                auto hist = svc.get_counterparty_history(req->id);
                BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
                reply(
                    nats_,
                    msg,
                    get_counterparty_history_response{.history = std::move(hist), .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_counterparty_history_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::counterparties:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::counterparty_service svc(req_ctx);
        if (auto req = decode<delete_counterparty_request>(msg)) {
            try {
                svc.delete_counterparties(req->ids);
                BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, delete_counterparty_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      delete_counterparty_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void hierarchy(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::counterparty_service svc(req_ctx);
        if (auto req = decode<get_counterparty_hierarchy_request>(msg)) {
            try {
                boost::uuids::string_generator gen;
                auto roots = svc.get_hierarchy(gen(req->root_id), req->from_root);
                BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_counterparty_hierarchy_response{.success = true,
                                                          .roots = std::move(roots)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(counterparty_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_counterparty_hierarchy_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void composite_as_of(ores::nats::message msg) {
        BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_counterparty_composite_as_of_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(counterparty_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            service::counterparty_service cpty_svc(req_ctx);
            const auto version = static_cast<std::uint32_t>(req->version);
            auto current = cpty_svc.get_counterparty_at_version(req->id, version);
            if (!current) {
                reply(nats_,
                      msg,
                      get_counterparty_composite_as_of_response{
                          .success = false,
                          .message = "No such counterparty version: " + req->id + " v" +
                                     std::to_string(version)});
                return;
            }

            // See party_handler::composite_as_of — windows are contiguous
            // by construction; the domain object doesn't surface valid_to.
            auto next = cpty_svc.get_counterparty_at_version(req->id, version + 1);
            const auto window_end =
                next ? next->recorded_at :
                       std::chrono::system_clock::now() + std::chrono::hours(24 * 365 * 100);

            boost::uuids::string_generator gen;
            service::counterparty_identifier_service identifier_svc(req_ctx);
            auto identifiers =
                identifier_svc.list_counterparty_identifiers_by_counterparty_id_as_of(
                    req->id, current->recorded_at, window_end);

            service::counterparty_contact_information_service contact_svc(req_ctx);
            auto contacts =
                contact_svc.list_counterparty_contact_informations_by_counterparty_as_of(
                    gen(req->id), current->recorded_at, window_end);

            BOOST_LOG_SEV(counterparty_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_counterparty_composite_as_of_response{.success = true,
                                                            .counterparty = std::move(*current),
                                                            .identifiers = std::move(identifiers),
                                                            .contacts = std::move(contacts)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(counterparty_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  get_counterparty_composite_as_of_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging

#endif
