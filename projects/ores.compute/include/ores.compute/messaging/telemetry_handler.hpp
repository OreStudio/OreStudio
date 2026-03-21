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
#ifndef ORES_COMPUTE_MESSAGING_TELEMETRY_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_TELEMETRY_HANDLER_HPP

#include <chrono>
#include <format>
#include <optional>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.compute/messaging/telemetry_protocol.hpp"
#include "ores.compute/repository/compute_telemetry_repository.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& telemetry_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.telemetry_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using ores::service::messaging::decode;
using namespace ores::logging;

class telemetry_handler {
public:
    telemetry_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    /**
     * @brief Handle compute.v1.telemetry.get_grid_stats (request/reply).
     *
     * Returns the most recent grid sample and per-node summaries from
     * TimescaleDB. No live aggregation is performed.
     */
    void get_grid_stats(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;

        get_grid_stats_response resp;
        try {
            repository::compute_telemetry_repository repo;
            const auto grid = repo.latest_grid_sample(ctx);
            if (grid) {
                resp.total_hosts          = grid->total_hosts;
                resp.online_hosts         = grid->online_hosts;
                resp.idle_hosts           = grid->idle_hosts;
                resp.results_inactive     = grid->results_inactive;
                resp.results_unsent       = grid->results_unsent;
                resp.results_in_progress  = grid->results_in_progress;
                resp.results_done         = grid->results_done;
                resp.total_workunits      = grid->total_workunits;
                resp.total_batches        = grid->total_batches;
                resp.active_batches       = grid->active_batches;
                resp.outcomes_success     = grid->outcomes_success;
                resp.outcomes_client_error = grid->outcomes_client_error;
                resp.outcomes_no_reply    = grid->outcomes_no_reply;

                resp.sampled_at = std::format("{:%Y-%m-%dT%H:%M:%SZ}",
                    grid->sampled_at);
            }

            const auto nodes = repo.latest_node_samples(ctx);
            resp.node_summaries.reserve(nodes.size());
            for (const auto& n : nodes) {
                node_stats_summary s;
                s.host_id               = boost::uuids::to_string(n.host_id);
                s.tasks_completed       = n.tasks_completed;
                s.tasks_since_last      = n.tasks_since_last;
                s.avg_task_duration_ms  = n.avg_task_duration_ms;
                s.input_bytes_fetched   = n.input_bytes_fetched;
                s.output_bytes_uploaded = n.output_bytes_uploaded;
                s.seconds_since_hb      = n.seconds_since_hb;
                resp.node_summaries.push_back(std::move(s));
            }

            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(telemetry_handler_lg(), error)
                << "get_grid_stats failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    /**
     * @brief Handle compute.v1.telemetry.node_samples (fire-and-forget
     * publish from wrapper nodes).
     *
     * Deserialises the node_sample_message and persists it to the
     * ores_compute_node_samples_tbl hypertable.
     */
    void ingest_node_sample(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), trace)
            << "Ingesting node sample from: " << msg.subject;

        const std::string_view data(
            reinterpret_cast<const char*>(msg.data.data()),
            msg.data.size());

        const auto parsed = rfl::json::read<node_sample_message>(data);
        if (!parsed) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn)
                << "Failed to decode node_sample_message: "
                << parsed.error().what();
            return;
        }

        try {
            domain::node_sample s;
            s.sampled_at           = std::chrono::system_clock::now();
            s.host_id              = boost::lexical_cast<boost::uuids::uuid>(
                parsed->host_id);
            s.tasks_completed      = parsed->tasks_completed;
            s.tasks_failed         = parsed->tasks_failed;
            s.tasks_since_last     = parsed->tasks_since_last;
            s.avg_task_duration_ms = parsed->avg_task_duration_ms;
            s.max_task_duration_ms = parsed->max_task_duration_ms;
            s.input_bytes_fetched  = parsed->input_bytes_fetched;
            s.output_bytes_uploaded = parsed->output_bytes_uploaded;
            s.seconds_since_hb     = parsed->seconds_since_hb;

            repository::compute_telemetry_repository repo;

            // The wrapper's --tenant-id is a NATS routing label (e.g.
            // "ores.dev.local1"), not a database UUID.  Attempt to parse it as
            // a UUID; if that fails, fall back to the service's own context.
            const auto tid = utility::uuid::tenant_id::from_string(
                parsed->tenant_id);
            if (tid) {
                s.tenant_id = *tid;
                auto write_ctx = ctx_.with_tenant(*tid, "telemetry_handler");
                repo.insert_node_sample(write_ctx, s);
            } else {
                s.tenant_id = ctx_.tenant_id();
                repo.insert_node_sample(ctx_, s);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(telemetry_handler_lg(), error)
                << "Failed to persist node sample: " << e.what();
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
