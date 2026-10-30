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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_BILL_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_BILL_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/billing_operation_protocol.hpp"
#include "ores.marketdata.core/repository/delivery_count_repository.hpp"
#include "ores.marketdata.core/repository/demand_interval_repository.hpp"
#include "ores.marketdata.core/repository/feed_repository.hpp"
#include "ores.marketdata.core/repository/rate_card_component_repository.hpp"
#include "ores.marketdata.core/repository/subscription_interval_repository.hpp"
#include "ores.marketdata.service/app/billing.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& bill_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.marketdata.messaging.bill_handler");
    return instance;
}
}

/**
 * @brief NATS request/reply handler for compute_bill_request: the bills of a period.
 *
 * The bills are computed from the usage events of every tenant, because the cost of a source is
 * shared among all the parties that use it. A session of the system tenant reads every bill. A
 * session of another tenant reads the charges of its own tenant and nothing else.
 */
class bill_handler {
public:
    bill_handler(ores::nats::service::client& nats,
                 ores::database::context ctx,
                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void compute(ores::nats::message msg) {
        using namespace ores::service::messaging;
        [[maybe_unused]] const auto correlation_id = log_handler_entry(bill_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::bills:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<compute_bill_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        compute_bill_response resp;
        if (req->period_to <= req->period_from) {
            resp.message = "The period must end after it starts.";
            reply(nats_, msg, resp);
            return;
        }

        try {
            const auto system_ctx =
                ores::database::service::tenant_context::with_system_tenant(ctx_);
            const auto sources =
                repository::feed_repository{}.read_latest(system_ctx);
            const auto card = repository::rate_card_component_repository{}.read_latest(system_ctx);
            ores::marketdata::service::app::usage_events events;
            events.subscriptions =
                repository::subscription_interval_repository{}.read_latest_all_tenants(ctx_);
            events.demands = repository::demand_interval_repository{}.read_latest_all_tenants(ctx_);
            events.deliveries =
                repository::delivery_count_repository{}.read_latest_all_tenants(ctx_);

            const auto report = ores::marketdata::service::app::make_report(
                sources, card, events, {req->period_from, req->period_to}, req->source_name);

            const bool system = req_ctx.tenant_id() == ores::utility::uuid::tenant_id::system();
            if (system) {
                for (const auto& b : report.bills) {
                    source_bill_view view;
                    view.feed_id = b.feed_id;
                    view.source_name = b.source_name;
                    view.currency = b.currency;
                    view.shadow = b.shadow;
                    view.total_micros = b.total_micros;
                    for (const auto& l : b.lines)
                        view.lines.push_back({l.driver,
                                              l.licence_right,
                                              l.quantity,
                                              l.charge_micros,
                                              l.total_micros});
                    resp.source_bills.push_back(std::move(view));
                }
                resp.unallocated_micros = report.unallocated_micros;
            }
            for (const auto& c : report.charges) {
                if (!system && c.tenant_id != req_ctx.tenant_id())
                    continue;
                resp.party_charges.push_back({c.tenant_id.to_string(),
                                              c.party_id,
                                              c.feed_id,
                                              c.driver,
                                              c.licence_right,
                                              c.total_micros});
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bill_handler_lg(), ores::logging::error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
