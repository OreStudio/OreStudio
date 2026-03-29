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
#ifndef ORES_DQ_CORE_MESSAGING_BADGE_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_BADGE_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"
#include "ores.dq.core/service/badge_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

namespace {
inline auto& badge_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.badge_handler");
    return instance;
}
} // namespace

class badge_handler {
public:
    badge_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    // =========================================================================
    // Badge Severity
    // =========================================================================

    void list_severities(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_badge_severities_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto items = svc.list_severities();
            get_badge_severities_response resp;
            resp.badge_severities = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_badge_severities_response{});
        }
    }

    void save_severity(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_badge_severity_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        if (!has_permission(*ctx_expected, "dq::badges:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            stamp(req->data, *ctx_expected);
            svc.save_severity(req->data);
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_badge_severity_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_badge_severity_response{false, e.what()});
        }
    }

    void delete_severities(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_badge_severity_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        if (!has_permission(*ctx_expected, "dq::badges:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            svc.remove_severities(req->codes);
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_badge_severity_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_badge_severity_response{false, e.what()});
        }
    }

    void severity_history(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_badge_severity_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto history = svc.get_severity_history(req->code);
            get_badge_severity_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_badge_severity_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Code Domain
    // =========================================================================

    void list_code_domains(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_code_domains_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto items = svc.list_code_domains();
            get_code_domains_response resp;
            resp.code_domains = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_code_domains_response{});
        }
    }

    void save_code_domain(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_code_domain_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        if (!has_permission(*ctx_expected, "dq::badges:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            stamp(req->data, *ctx_expected);
            svc.save_code_domain(req->data);
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_code_domain_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_code_domain_response{false, e.what()});
        }
    }

    void delete_code_domains(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_code_domain_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        if (!has_permission(*ctx_expected, "dq::badges:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            svc.remove_code_domains(req->codes);
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_code_domain_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_code_domain_response{false, e.what()});
        }
    }

    void code_domain_history(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_code_domain_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto history = svc.get_code_domain_history(req->code);
            get_code_domain_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_code_domain_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Badge Definition
    // =========================================================================

    void list_definitions(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_badge_definitions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto items = svc.list_definitions();
            get_badge_definitions_response resp;
            resp.definitions = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_badge_definitions_response{});
        }
    }

    void save_definition(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_badge_definition_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        if (!has_permission(*ctx_expected, "dq::badges:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            stamp(req->data, *ctx_expected);
            svc.save_definition(req->data);
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_badge_definition_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_badge_definition_response{false, e.what()});
        }
    }

    void delete_definitions(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_badge_definition_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        if (!has_permission(*ctx_expected, "dq::badges:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::badge_service svc(*ctx_expected);
        try {
            svc.remove_definitions(req->codes);
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_badge_definition_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_badge_definition_response{false, e.what()});
        }
    }

    void definition_history(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_badge_definition_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto history = svc.get_definition_history(req->code);
            get_badge_definition_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_badge_definition_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Badge Mapping (read-only)
    // =========================================================================

    void list_mappings(ores::nats::message msg) {
        BOOST_LOG_SEV(badge_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_badge_mappings_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(badge_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) { error_reply(nats_, msg, ctx_expected.error()); return; }
        service::badge_service svc(*ctx_expected);
        try {
            const auto items = svc.list_mappings();
            get_badge_mappings_response resp;
            resp.mappings = items;
            BOOST_LOG_SEV(badge_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(badge_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_badge_mappings_response{});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
