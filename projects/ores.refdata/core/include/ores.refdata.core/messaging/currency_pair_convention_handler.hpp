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
#ifndef ORES_REFDATA_CORE_MESSAGING_CURRENCY_PAIR_CONVENTION_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_CURRENCY_PAIR_CONVENTION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/currency_pair_convention_protocol.hpp"
#include "ores.refdata.core/service/currency_pair_convention_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <limits>
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& currency_pair_convention_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.refdata.messaging.currency_pair_convention_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for currency pair convention operations.
 */
class currency_pair_convention_handler {
public:
    currency_pair_convention_handler(ores::nats::service::client& nats,
                                     ores::database::context ctx,
                                     std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::currency_pair_convention_service svc(req_ctx);
        get_currency_pair_conventions_response resp;
        if (auto req = decode<get_currency_pair_conventions_request>(msg)) {
            try {
                resp.conventions = svc.list_conventions(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_conventions());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(currency_pair_convention_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::currency_pair_conventions:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::currency_pair_convention_service svc(req_ctx);
        if (auto req = decode<save_currency_pair_convention_request>(msg)) {
            try {
                svc.save_convention(req->data);
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, save_currency_pair_convention_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(
                    nats_,
                    msg,
                    save_currency_pair_convention_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_pair_convention_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::currency_pair_convention_service svc(req_ctx);
        if (auto req = decode<get_currency_pair_convention_history_request>(msg)) {
            try {
                auto hist = svc.get_convention_history(req->pair_code);
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_currency_pair_convention_history_response{.history = std::move(hist),
                                                                    .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_currency_pair_convention_history_response{.success = false,
                                                                    .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_pair_convention_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::currency_pair_conventions:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::currency_pair_convention_service svc(req_ctx);
        if (auto req = decode<delete_currency_pair_convention_request>(msg)) {
            try {
                svc.delete_conventions(req->pair_codes);
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, delete_currency_pair_convention_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      delete_currency_pair_convention_response{.success = false,
                                                               .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_pair_convention_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void read_for_cache(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug) << "Handling " << msg.subject;
        // Authentication-only, deliberately not tenant-scoped: this proves
        // the caller holds *a* valid signed JWT, but does not check that
        // token's own tenant against req->tenant_id below (the tenant a
        // cache-warming service account reads is unrelated to any tenant
        // its own token carries). Do not copy this method as a template
        // for a tenant-authorized endpoint.
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        if (auto req = decode<read_currency_pair_conventions_for_cache_request>(msg)) {
            try {
                using ores::database::service::tenant_context;
                auto tctx = tenant_context::with_tenant(ctx_, req->tenant_id);
                service::currency_pair_convention_service svc(tctx);
                // No dedicated unpaginated list method is generated; reuse
                // the paginated one with an unbounded limit.
                auto conventions =
                    svc.list_conventions(0, std::numeric_limits<std::uint32_t>::max());
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), debug)
                    << "Completed " << msg.subject << " (tenant=" << req->tenant_id
                    << ", count=" << conventions.size() << ")";
                reply(nats_,
                      msg,
                      read_currency_pair_conventions_for_cache_response{
                          .success = true, .conventions = std::move(conventions)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_pair_convention_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      read_currency_pair_conventions_for_cache_response{.success = false,
                                                                        .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_pair_convention_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging

#endif
