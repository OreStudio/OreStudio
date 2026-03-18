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
#ifndef ORES_DQ_MESSAGING_DIMENSION_HANDLER_HPP
#define ORES_DQ_MESSAGING_DIMENSION_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.dq/service/dimension_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

namespace {
inline auto& dimension_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.dimension_handler");
    return instance;
}
} // namespace

class dimension_handler {
public:
    dimension_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    // =========================================================================
    // Nature Dimensions
    // =========================================================================

    void list_nature_dimensions(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_nature_dimensions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            const auto items = svc.list_nature_dimensions();
            get_nature_dimensions_response resp;
            resp.nature_dimensions = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_nature_dimensions_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_nature_dimension(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_nature_dimension_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_nature_dimension(req->data);
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_nature_dimension_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_nature_dimension_response{false, e.what()});
        }
    }

    void delete_nature_dimensions(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_nature_dimension_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            svc.remove_nature_dimensions(req->codes);
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_nature_dimension_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                delete_nature_dimension_response{false, e.what()});
        }
    }

    void nature_dimension_history(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_nature_dimension_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            const auto history = svc.get_nature_dimension_history(req->code);
            get_nature_dimension_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_nature_dimension_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Origin Dimensions
    // =========================================================================

    void list_origin_dimensions(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_origin_dimensions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            const auto items = svc.list_origin_dimensions();
            get_origin_dimensions_response resp;
            resp.origin_dimensions = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_origin_dimensions_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_origin_dimension(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_origin_dimension_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_origin_dimension(req->data);
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_origin_dimension_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_origin_dimension_response{false, e.what()});
        }
    }

    void delete_origin_dimensions(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_origin_dimension_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            svc.remove_origin_dimensions(req->codes);
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_origin_dimension_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                delete_origin_dimension_response{false, e.what()});
        }
    }

    void origin_dimension_history(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_origin_dimension_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            const auto history = svc.get_origin_dimension_history(req->code);
            get_origin_dimension_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_origin_dimension_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Treatment Dimensions
    // =========================================================================

    void list_treatment_dimensions(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_treatment_dimensions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            const auto items = svc.list_treatment_dimensions();
            get_treatment_dimensions_response resp;
            resp.treatment_dimensions = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_treatment_dimensions_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_treatment_dimension(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_treatment_dimension_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_treatment_dimension(req->data);
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_treatment_dimension_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                save_treatment_dimension_response{false, e.what()});
        }
    }

    void delete_treatment_dimensions(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_treatment_dimension_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            svc.remove_treatment_dimensions(req->codes);
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_treatment_dimension_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                delete_treatment_dimension_response{false, e.what()});
        }
    }

    void treatment_dimension_history(ores::nats::message msg) {
        BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_treatment_dimension_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dimension_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dimension_service svc(ctx);
        try {
            const auto history =
                svc.get_treatment_dimension_history(req->code);
            get_treatment_dimension_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(dimension_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dimension_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_treatment_dimension_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

private:

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
