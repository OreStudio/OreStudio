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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_CRM_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_CRM_HANDLER_HPP

#include "ores.analytics.quant/domain/crm_rate_view.hpp"
#include "ores.analytics.quant/domain/derived_rate.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <chrono>
#include <format>
#include <memory>
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& crm_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.marketdata.messaging.crm_handler");
    return instance;
}

inline std::string rate_status_to_string(ores::analytics::quant::domain::rate_status s) {
    using ores::analytics::quant::domain::rate_status;
    switch (s) {
        case rate_status::fresh:
            return "fresh";
        case rate_status::stale:
            return "stale";
        case rate_status::unavailable:
            return "unavailable";
    }
    return "unavailable";
}

inline std::string as_of_to_string(std::chrono::system_clock::time_point tp) {
    if (tp == std::chrono::system_clock::time_point{})
        return {};
    return std::format("{:%FT%TZ}", std::chrono::floor<std::chrono::seconds>(tp));
}

inline crm_rate_item to_item(const std::string& crm_name,
                             const ores::analytics::quant::domain::derived_rate& r) {
    return crm_rate_item{.crm_name = crm_name,
                         .base_currency_code = r.base_code,
                         .quote_currency_code = r.quote_code,
                         .rate = r.rate,
                         .status = rate_status_to_string(r.status),
                         .as_of = as_of_to_string(r.as_of)};
}

inline crm_rate_item to_item(const std::string& crm_name,
                             const ores::analytics::quant::domain::crm_rate_view& v) {
    return crm_rate_item{.crm_name = crm_name,
                         .base_currency_code = v.base_code,
                         .quote_currency_code = v.quote_code,
                         .rate = v.rate,
                         .status = rate_status_to_string(v.status),
                         .as_of = as_of_to_string(v.as_of),
                         .inverted = v.inverted,
                         .delta_pct = v.delta_pct};
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

/**
 * @brief Handles the two CRM request/reply endpoints -- both pull-only,
 * computed on demand from crm_ingest_bridge's live per-party rate_engine.
 * See the CRM story's architecture decision to never broadcast the full
 * derived set as ticks.
 */
class ORES_MARKETDATA_SERVICE_EXPORT crm_handler {
public:
    crm_handler(ores::nats::service::client& nats,
                ores::database::context ctx,
                std::optional<ores::security::jwt::jwt_authenticator> verifier,
                std::shared_ptr<service::app::crm_ingest_bridge> bridge)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , bridge_(std::move(bridge)) {}

    void rate(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(crm_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "marketdata::crm:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_crm_rate_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(crm_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }

        if (req->crm_name.empty()) {
            reply(nats_,
                  msg,
                  get_crm_rate_response{
                      .success = false, .message = "crm_name is required", .rate = {}});
            return;
        }

        const auto tenant_id_str = ctx.tenant_id().to_string();
        const auto result = bridge_->rate(tenant_id_str,
                                          req->party_id,
                                          req->crm_name,
                                          req->base_currency_code,
                                          req->quote_currency_code);
        if (!result) {
            reply(nats_,
                  msg,
                  get_crm_rate_response{.success = false,
                                        .message = "No such CRM configured for this party",
                                        .rate = {}});
            return;
        }
        reply(nats_,
              msg,
              get_crm_rate_response{
                  .success = true, .message = {}, .rate = to_item(req->crm_name, *result)});
    }

    void rates(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(crm_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "marketdata::crm:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_crm_rates_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(crm_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }

        const auto tenant_id_str = ctx.tenant_id().to_string();

        get_crm_rates_response resp;
        resp.success = true;
        if (req->crm_name.empty()) {
            // No CRM selected -- every enabled CRM the party has, tagged.
            const auto results = bridge_->resolved_rates(tenant_id_str, req->party_id, req->inverted);
            resp.rates.reserve(results.size());
            for (const auto& r : results)
                resp.rates.push_back(to_item(r.crm_name, r.view));
        } else {
            const auto results =
                bridge_->resolved_rates(tenant_id_str, req->party_id, req->crm_name, req->inverted);
            resp.rates.reserve(results.size());
            for (const auto& v : results)
                resp.rates.push_back(to_item(req->crm_name, v));
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    std::shared_ptr<service::app::crm_ingest_bridge> bridge_;
};

} // namespace ores::marketdata::messaging
#endif
