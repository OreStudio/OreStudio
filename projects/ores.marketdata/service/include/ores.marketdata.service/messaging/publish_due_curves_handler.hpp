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
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/curve_build_planner.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.marketdata.service/messaging/approval_gated_curve_publish_handler.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

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
using ores::marketdata::service::curve_build_node;
using ores::marketdata::service::curve_build_refusal;
using ores::marketdata::service::curve_build_refusal_kind;
using ores::marketdata::service::plan_curve_builds;
using namespace ores::logging;

/**
 * @brief NATS handler for publish_due_curves_request: the scheduled sweep.
 *
 * A scheduler job fires this on its cadence. The sweep lists the current
 * tenant's IR curve bootstrap configs, orders them by the inputs they declare,
 * and publishes each one at a single instant through
 * @c approval_gated_curve_publish_handler::answer(), the same path a person's
 * publish takes. In the system tenant the publish auto-approves, so a firing
 * publishes; in any other tenant the raise waits for a decider and the response
 * says so.
 *
 * The order is the plan's, not the listing's, so a projection curve is visited
 * after the funding curve it discounts off: one firing builds both. A curve
 * whose declared input no curve in the firing builds is deferred unvisited, and
 * a curve whose inputs are ready but whose build breaks is failed. The sweep
 * reports the three counts apart, because a read of the log must be able to tell
 * an ordering from a break.
 *
 * Each publish supersedes the previous one, and a curve that cannot build keeps
 * the publish it had.
 */
class publish_due_curves_handler {
public:
    /// The curves one firing holds, by the identifier a plan names them with.
    using config_by_id =
        std::unordered_map<std::string, const ores::refdata::domain::ir_curve_bootstrap_config*>;

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
            ORES_LOG_SEV(publish_due_curves_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        ORES_LOG_SEV(publish_due_curves_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    /**
     * @brief The sweep itself, without the message: the entry a test drives.
     *
     * Lists the current tenant's IR curve bootstrap configs, plans the order
     * their declared inputs allow, and publishes each one at @p as_of through
     * the person's publish path. The firing states the instant for every curve
     * it publishes.
     */
    static publish_due_curves_response
    publish_all(const ores::database::context& req_ctx,
                std::chrono::system_clock::time_point as_of = std::chrono::system_clock::now()) {
        using namespace service::app;

        publish_due_curves_response resp;
        constexpr std::string_view reason =
            "The scheduled curve republish keeps the system's published curves on the market of "
            "the day.";

        ores::refdata::repository::ir_curve_bootstrap_config_repository repo;
        const auto configs = repo.read_latest(req_ctx);

        config_by_id configs_by_id;
        std::vector<curve_build_node> nodes;
        configs_by_id.reserve(configs.size());
        nodes.reserve(configs.size());
        for (const auto& config : configs) {
            auto id = boost::uuids::to_string(config.id);
            configs_by_id.emplace(id, &config);

            curve_build_node node;
            node.id = id;
            if (config.discount_curve_config_id)
                node.inputs.push_back(boost::uuids::to_string(*config.discount_curve_config_id));
            nodes.push_back(std::move(node));
        }

        const auto planned = plan_curve_builds(nodes);
        if (!planned) {
            resp.success = false;
            resp.message = refusal_message(planned.error(), configs_by_id);
            ORES_LOG_SEV(publish_due_curves_handler_lg(), error)
                << "Curve republish refused: " << resp.message;
            return resp;
        }

        std::size_t published = 0;
        std::size_t deferred = 0;
        std::size_t failed = 0;
        std::size_t raised = 0;

        const auto record = [&](curve_republish_outcome outcome) {
            if (outcome.status == publish_status_published)
                ++published;
            else if (outcome.status == publish_status_deferred)
                ++deferred;
            else if (outcome.status == publish_status_raised)
                ++raised;
            else
                ++failed;

            ORES_LOG_SEV(publish_due_curves_handler_lg(), info)
                << "Curve republish: name='" << outcome.name << "' as_of='"
                << ores::platform::time::datetime::to_iso8601_utc(outcome.as_of)
                << "' duration=" << outcome.duration_ms << "ms status=" << outcome.status
                << (outcome.message.empty() ? "" : " reason='" + outcome.message + "'");
            resp.curves.push_back(std::move(outcome));
        };

        // The curves no level holds are named before the ones that build, because
        // they are the firing's reason to be re-read rather than its outcome.
        for (const auto& edge : planned->unbuildable) {
            curve_republish_outcome outcome;
            outcome.bootstrap_config_id = edge.node_id;
            outcome.name = name_of(edge.node_id, configs_by_id);
            outcome.as_of = as_of;
            outcome.status = std::string(publish_status_deferred);
            outcome.message = "It reads '" + name_of(edge.input_id, configs_by_id) +
                              "', which this firing does not build.";
            record(std::move(outcome));
        }

        for (const auto& level : planned->levels) {
            for (const auto& config_id : level) {
                curve_republish_outcome outcome;
                outcome.bootstrap_config_id = config_id;
                outcome.name = name_of(config_id, configs_by_id);

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
                    // The publish path answers in the same words the sweep reports, so
                    // a curve's fate is never restated in a second vocabulary.
                    outcome.status =
                        answer.status.empty() ? std::string(publish_status_failed) : answer.status;
                    outcome.message = answer.message;
                } catch (const std::exception& e) {
                    outcome.status = std::string(publish_status_failed);
                    outcome.message = e.what();
                }
                outcome.duration_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                                          std::chrono::steady_clock::now() - started)
                                          .count();
                record(std::move(outcome));
            }
        }

        // The sweep succeeded when nothing broke. A deferred curve is one whose inputs
        // have not arrived, which the next firing tries again; a raised one is a publish
        // the approval gate took, which is somebody else's to answer. Calling either a
        // failure would hide an ordering, or an approval queue, behind a break.
        resp.success = failed == 0;
        resp.message = "published " + std::to_string(published) + " of " +
                       std::to_string(configs.size()) + " curve(s): deferred " +
                       std::to_string(deferred) + ", failed " + std::to_string(failed) +
                       ", raised " + std::to_string(raised);
        ORES_LOG_SEV(publish_due_curves_handler_lg(), info)
            << "Curve republish sweep: " << resp.message;
        return resp;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;

private:
    /// The name a curve is known by, or the identifier when the firing does not
    /// hold the curve the edge names.
    static std::string name_of(const std::string& config_id, const config_by_id& configs_by_id) {
        const auto it = configs_by_id.find(config_id);
        return it == configs_by_id.end() ? config_id : it->second->name;
    }

    /// Why the firing could not order its curves, naming them so the fault is
    /// read from the log rather than looked up by re-running the sweep.
    static std::string refusal_message(const curve_build_refusal& refusal,
                                       const config_by_id& configs_by_id) {
        std::string names;
        for (const auto& id : refusal.node_ids)
            names += (names.empty() ? "" : ", ") + std::string("'") + name_of(id, configs_by_id) +
                     std::string("'");
        const auto what = refusal.kind == curve_build_refusal_kind::cycle ?
                              "the curves discount off each other in a cycle" :
                              "a curve is configured more than once";
        return "The curve build cannot be planned: " + std::string(what) + ": " + names;
    }
};

}

#endif
