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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_PUBLISH_DUE_CURVES_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_PUBLISH_DUE_CURVES_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/messaging/approval_gated_curve_publish_handler.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& publish_due_curves_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.publish_due_curves_handler");
    return instance;
}
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler for publish_due_curves_request: the scheduled sweep.
 *
 * A scheduler job fires this on its cadence. The sweep lists the current
 * tenant's IR curve bootstrap configs and publishes each one at a single
 * instant, through @c approval_gated_curve_publish_handler::answer(), the same
 * path a person's publish takes. In the system tenant the publish auto-approves,
 * so a firing publishes; in any other tenant the raise waits for a decider and
 * the response says so.
 *
 * Each publish supersedes the previous one. A curve whose bootstrap or publish
 * fails keeps its previous publish, and the response names it. The log names
 * every curve, its as-of, how long it took and whether it published.
 */
class publish_due_curves_handler {
public:
    publish_due_curves_handler(ores::nats::service::client& nats,
                               const ores::database::context& ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void publish_due(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(publish_due_curves_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::curve_bootstrap:publish")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<publish_due_curves_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        publish_due_curves_response resp;
        try {
            resp = publish_all(req_ctx);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(publish_due_curves_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(publish_due_curves_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    /**
     * @brief The sweep itself, without the message: the entry a test drives.
     *
     * Lists the current tenant's IR curve bootstrap configs and publishes each
     * one at @p as_of through the person's publish path. The firing states the
     * instant for every curve it publishes.
     */
    static publish_due_curves_response
    publish_all(const ores::database::context& req_ctx,
                std::chrono::system_clock::time_point as_of = std::chrono::system_clock::now()) {
        publish_due_curves_response resp;
        constexpr std::string_view reason =
            "The scheduled curve republish keeps the system's published curves on the market of "
            "the day.";

        ores::refdata::repository::ir_curve_bootstrap_config_repository repo;
        const auto configs = repo.read_latest(req_ctx);

        std::size_t published = 0;
        for (const auto& config : configs) {
            curve_republish_outcome outcome;
            outcome.bootstrap_config_id = boost::uuids::to_string(config.id);
            outcome.name = config.name;

            // One instant for every curve of the firing, so the published sets
            // and the as-of they are recorded at cannot disagree.
            outcome.as_of = as_of;
            const auto started = std::chrono::steady_clock::now();
            try {
                publish_curve_request publish;
                publish.bootstrap_config_id = outcome.bootstrap_config_id;
                publish.as_of = outcome.as_of;
                publish.reason = std::string(reason);

                publish_curve_response answer;
                approval_gated_curve_publish_handler::answer(req_ctx, publish, answer);
                outcome.published = answer.success && answer.published_at.has_value();
                outcome.message = answer.message;
                if (answer.success && !answer.published_at)
                    outcome.message = "Raised; it waits on a decision. " + answer.message;
            } catch (const std::exception& e) {
                outcome.message = e.what();
            }
            outcome.duration_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                                      std::chrono::steady_clock::now() - started)
                                      .count();
            if (outcome.published)
                ++published;

            BOOST_LOG_SEV(publish_due_curves_handler_lg(), info)
                << "Curve republish: name='" << outcome.name << "' as_of='"
                << ores::platform::time::datetime::to_iso8601_utc(outcome.as_of)
                << "' duration=" << outcome.duration_ms
                << "ms published=" << (outcome.published ? "yes" : "no")
                << (outcome.message.empty() ? "" : " reason='" + outcome.message + "'");
            resp.curves.push_back(std::move(outcome));
        }

        // The sweep succeeded only when every curve it visited published. A
        // curve that failed is named in its own outcome, so the response can
        // say that the firing did not do what it was asked.
        resp.success = published == configs.size();
        resp.message = "published " + std::to_string(published) + " of " +
                       std::to_string(configs.size()) + " curve(s)";
        BOOST_LOG_SEV(publish_due_curves_handler_lg(), info)
            << "Curve republish sweep: " << resp.message;
        return resp;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
