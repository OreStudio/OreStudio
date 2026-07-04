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
#ifndef ORES_REFDATA_CORE_MESSAGING_CURRENCY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_CURRENCY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/domain/currency_version.hpp"
#include "ores.refdata.api/messaging/currency_history_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.core/presentation/currency_field_mapper.hpp"
#include "ores.refdata.core/service/currency_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& currency_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.refdata.messaging.currency_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for currency operations.
 */
class currency_handler {
public:
    currency_handler(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::currency_service svc(req_ctx);
        get_currencies_response resp;
        if (auto req = decode<get_currencies_request>(msg)) {
            try {
                resp.currencies = svc.list_currencies(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_currencies());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(currency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(currency_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::currencies:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::currency_service svc(req_ctx);
        if (auto req = decode<save_currency_request>(msg)) {
            try {
                svc.save_currency(req->data);
                BOOST_LOG_SEV(currency_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, save_currency_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_currency_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "refdata::currencies:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::currency_service svc(req_ctx);
        if (auto req = decode<delete_currency_request>(msg)) {
            try {
                svc.delete_currencies(req->iso_codes);
                BOOST_LOG_SEV(currency_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, delete_currency_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_currency_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    // Custom: currency's history response carries per-version field diffs
    // (currency_version_history via currency_field_mapper), richer than the
    // plain-history shape the qt/nats-handler templates generate. No
    // template equivalent exists yet — see the Qt reconciliation story
    // (395AAFAD-9013-4DF8-BD60-E5B50ADA6385) for the related UI feature.
    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(currency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::currency_service svc(req_ctx);
        if (auto req = decode<get_currency_history_request>(msg)) {
            try {
                auto h = svc.get_currency_history(req->iso_code);
                currency_version_history cvh;
                cvh.versions = presentation::currency_field_mapper::build_versions(h);
                BOOST_LOG_SEV(currency_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_currency_history_response{.success = true, .history = std::move(cvh)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(currency_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_currency_history_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(currency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
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
