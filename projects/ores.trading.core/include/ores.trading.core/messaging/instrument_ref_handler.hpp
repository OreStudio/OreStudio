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
#ifndef ORES_TRADING_MESSAGING_INSTRUMENT_REF_HANDLER_HPP
#define ORES_TRADING_MESSAGING_INSTRUMENT_REF_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/day_count_fraction_type_protocol.hpp"
#include "ores.trading.api/messaging/business_day_convention_type_protocol.hpp"
#include "ores.trading.api/messaging/floating_index_type_protocol.hpp"
#include "ores.trading.api/messaging/payment_frequency_type_protocol.hpp"
#include "ores.trading.api/messaging/leg_type_protocol.hpp"
#include "ores.trading.core/service/day_count_fraction_type_service.hpp"
#include "ores.trading.core/service/business_day_convention_type_service.hpp"
#include "ores.trading.core/service/floating_index_type_service.hpp"
#include "ores.trading.core/service/payment_frequency_type_service.hpp"
#include "ores.trading.core/service/leg_type_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& instrument_ref_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.instrument_ref_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class instrument_ref_handler {
public:
    instrument_ref_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

private:
    template<typename Svc, typename Req, typename Resp>
    void list_impl(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_ref_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        const auto sys_ctx = req_ctx.with_tenant(
            ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        Svc svc(sys_ctx);
        Resp resp;
        try {
            resp.types = svc.list_types();
            resp.total_available_count = static_cast<int>(resp.types.size());
        } catch (...) {}
        BOOST_LOG_SEV(instrument_ref_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    template<typename Svc, typename Req, typename Resp>
    void save_impl(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_ref_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        Svc svc(req_ctx);
        if (auto req = decode<Req>(msg)) {
            try {
                svc.save_type(req->data);
                BOOST_LOG_SEV(instrument_ref_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, Resp{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(instrument_ref_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, Resp{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(instrument_ref_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    template<typename Svc, typename Req, typename Resp>
    void delete_impl(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_ref_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        Svc svc(req_ctx);
        if (auto req = decode<Req>(msg)) {
            try {
                svc.remove_types(req->codes);
                BOOST_LOG_SEV(instrument_ref_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, Resp{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(instrument_ref_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, Resp{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(instrument_ref_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    template<typename Svc, typename Req, typename Resp>
    void history_impl(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_ref_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        const auto sys_ctx = req_ctx.with_tenant(
            ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        Svc svc(sys_ctx);
        if (auto req = decode<Req>(msg)) {
            try {
                auto hist = svc.get_type_history(req->code);
                BOOST_LOG_SEV(instrument_ref_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, Resp{.success = true, .history = std::move(hist)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(instrument_ref_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, Resp{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(instrument_ref_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

public:
    // Day count fraction type
    void list_day_count_fraction_types(ores::nats::message msg) {
        list_impl<service::day_count_fraction_type_service,
                  get_day_count_fraction_types_request,
                  get_day_count_fraction_types_response>(std::move(msg));
    }
    void save_day_count_fraction_type(ores::nats::message msg) {
        save_impl<service::day_count_fraction_type_service,
                  save_day_count_fraction_type_request,
                  save_day_count_fraction_type_response>(std::move(msg));
    }
    void delete_day_count_fraction_type(ores::nats::message msg) {
        delete_impl<service::day_count_fraction_type_service,
                    delete_day_count_fraction_type_request,
                    delete_day_count_fraction_type_response>(std::move(msg));
    }
    void history_day_count_fraction_type(ores::nats::message msg) {
        history_impl<service::day_count_fraction_type_service,
                     get_day_count_fraction_type_history_request,
                     get_day_count_fraction_type_history_response>(std::move(msg));
    }

    // Business day convention type
    void list_business_day_convention_types(ores::nats::message msg) {
        list_impl<service::business_day_convention_type_service,
                  get_business_day_convention_types_request,
                  get_business_day_convention_types_response>(std::move(msg));
    }
    void save_business_day_convention_type(ores::nats::message msg) {
        save_impl<service::business_day_convention_type_service,
                  save_business_day_convention_type_request,
                  save_business_day_convention_type_response>(std::move(msg));
    }
    void delete_business_day_convention_type(ores::nats::message msg) {
        delete_impl<service::business_day_convention_type_service,
                    delete_business_day_convention_type_request,
                    delete_business_day_convention_type_response>(std::move(msg));
    }
    void history_business_day_convention_type(ores::nats::message msg) {
        history_impl<service::business_day_convention_type_service,
                     get_business_day_convention_type_history_request,
                     get_business_day_convention_type_history_response>(std::move(msg));
    }

    // Floating index type
    void list_floating_index_types(ores::nats::message msg) {
        list_impl<service::floating_index_type_service,
                  get_floating_index_types_request,
                  get_floating_index_types_response>(std::move(msg));
    }
    void save_floating_index_type(ores::nats::message msg) {
        save_impl<service::floating_index_type_service,
                  save_floating_index_type_request,
                  save_floating_index_type_response>(std::move(msg));
    }
    void delete_floating_index_type(ores::nats::message msg) {
        delete_impl<service::floating_index_type_service,
                    delete_floating_index_type_request,
                    delete_floating_index_type_response>(std::move(msg));
    }
    void history_floating_index_type(ores::nats::message msg) {
        history_impl<service::floating_index_type_service,
                     get_floating_index_type_history_request,
                     get_floating_index_type_history_response>(std::move(msg));
    }

    // Payment frequency type
    void list_payment_frequency_types(ores::nats::message msg) {
        list_impl<service::payment_frequency_type_service,
                  get_payment_frequency_types_request,
                  get_payment_frequency_types_response>(std::move(msg));
    }
    void save_payment_frequency_type(ores::nats::message msg) {
        save_impl<service::payment_frequency_type_service,
                  save_payment_frequency_type_request,
                  save_payment_frequency_type_response>(std::move(msg));
    }
    void delete_payment_frequency_type(ores::nats::message msg) {
        delete_impl<service::payment_frequency_type_service,
                    delete_payment_frequency_type_request,
                    delete_payment_frequency_type_response>(std::move(msg));
    }
    void history_payment_frequency_type(ores::nats::message msg) {
        history_impl<service::payment_frequency_type_service,
                     get_payment_frequency_type_history_request,
                     get_payment_frequency_type_history_response>(std::move(msg));
    }

    // Leg type
    void list_leg_types(ores::nats::message msg) {
        list_impl<service::leg_type_service,
                  get_leg_types_request,
                  get_leg_types_response>(std::move(msg));
    }
    void save_leg_type(ores::nats::message msg) {
        save_impl<service::leg_type_service,
                  save_leg_type_request,
                  save_leg_type_response>(std::move(msg));
    }
    void delete_leg_type(ores::nats::message msg) {
        delete_impl<service::leg_type_service,
                    delete_leg_type_request,
                    delete_leg_type_response>(std::move(msg));
    }
    void history_leg_type(ores::nats::message msg) {
        history_impl<service::leg_type_service,
                     get_leg_type_history_request,
                     get_leg_type_history_response>(std::move(msg));
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::trading::messaging

#endif
