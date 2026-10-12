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

#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"
#include "ores.compute.core/repository/grid_sample_repository.hpp"
#include "ores.compute.core/repository/node_sample_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <optional>
#include <rfl/json.hpp>

namespace ores::compute::messaging {

namespace {
inline auto& telemetry_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.compute.messaging.telemetry_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using ores::service::messaging::decode;
using namespace ores::logging;

class ORES_COMPUTE_CORE_EXPORT telemetry_handler {
public:
    telemetry_handler(ores::nats::service::client& nats,
                      ores::database::context ctx,
                      std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Handle compute.v1.ops.get_grid_stats (request/reply).
     *
     * Returns the most recent grid sample and per-node summaries from
     * TimescaleDB. No live aggregation is performed.
     */
    void get_grid_stats(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!ores::service::messaging::has_permission(ctx, "compute::grid_samples:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        get_grid_stats_response resp;
        try {
            // The newest stored sample, which the entity model generates as
            // read_newest: a paged read walks the key from its oldest end.
            repository::grid_sample_repository grid_repo;
            const auto grid = grid_repo.read_newest(ctx);
            repository::compute_telemetry_repository repo;
            if (grid) {
                resp.total_hosts = grid->total_hosts;
                resp.online_hosts = grid->online_hosts;
                resp.idle_hosts = grid->idle_hosts;
                resp.results_inactive = grid->results_inactive;
                resp.results_unsent = grid->results_unsent;
                resp.results_in_progress = grid->results_in_progress;
                resp.results_done = grid->results_done;
                resp.total_workunits = grid->total_workunits;
                resp.total_batches = grid->total_batches;
                resp.active_batches = grid->active_batches;
                resp.outcomes_success = grid->outcomes_success;
                resp.outcomes_client_error = grid->outcomes_client_error;
                resp.outcomes_no_reply = grid->outcomes_no_reply;

                resp.sampled_at = std::format("{:%Y-%m-%dT%H:%M:%SZ}", grid->sampled_at);
            }

            const auto nodes = repo.latest_node_samples(ctx);
            resp.node_summaries.reserve(nodes.size());
            for (const auto& n : nodes) {
                node_stats_summary s;
                s.host_id = boost::uuids::to_string(n.host_id);
                s.machine = n.machine;
                s.tasks_completed = n.tasks_completed;
                s.tasks_failed = n.tasks_failed;
                s.tasks_since_last = n.tasks_since_last;
                s.avg_task_duration_ms = n.avg_task_duration_ms;
                s.max_task_duration_ms = n.max_task_duration_ms;
                s.input_bytes_fetched = n.input_bytes_fetched;
                s.output_bytes_uploaded = n.output_bytes_uploaded;
                s.seconds_since_hb = n.seconds_since_hb;
                resp.node_summaries.push_back(std::move(s));
            }

            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(telemetry_handler_lg(), error) << "get_grid_stats failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Completed " << msg.subject;
    }

    /**
     * @brief Handle compute.v1.ops.get_grid_series (request/reply).
     *
     * Returns the stored grid samples of a time range as columns on a regular
     * time axis. The read serves the whole installation and needs the same
     * permission as the newest sample. A range the repository refuses, such as
     * one that needs more than max_grid_series_buckets buckets, answers with
     * the reason and no data.
     */
    void get_grid_series(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!ores::service::messaging::has_permission(ctx, "compute::grid_samples:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        get_grid_series_response resp;
        const auto req = decode<get_grid_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            resp.message = "Failed to decode request";
            reply(nats_, msg, resp);
            return;
        }

        try {
            using ores::platform::time::datetime;
            repository::compute_telemetry_repository repo;
            auto series = repo.grid_series_in_range(ctx,
                                                    datetime::from_iso8601_utc(req->start_time),
                                                    datetime::from_iso8601_utc(req->end_time),
                                                    req->bucket_seconds);
            resp.start_time = datetime::to_iso8601_utc(series.start);
            resp.bucket_seconds = series.bucket_seconds;
            resp.bucket_count = series.bucket_count;
            resp.samples = std::move(series.samples);
            resp.total_hosts = std::move(series.total_hosts);
            resp.online_hosts = std::move(series.online_hosts);
            resp.idle_hosts = std::move(series.idle_hosts);
            resp.results_inactive = std::move(series.results_inactive);
            resp.results_unsent = std::move(series.results_unsent);
            resp.results_in_progress = std::move(series.results_in_progress);
            resp.results_done = std::move(series.results_done);
            resp.total_workunits = std::move(series.total_workunits);
            resp.total_batches = std::move(series.total_batches);
            resp.active_batches = std::move(series.active_batches);
            resp.outcomes_success = std::move(series.outcomes_success);
            resp.outcomes_client_error = std::move(series.outcomes_client_error);
            resp.outcomes_no_reply = std::move(series.outcomes_no_reply);
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn) << "get_grid_series refused: " << e.what();
            resp = get_grid_series_response{};
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Completed " << msg.subject;
    }

    /**
     * @brief Handle compute.v1.ops.get_grid_usage (request/reply).
     *
     * Returns the work of the grid counted by group. The session decides the
     * scope: the system tenant reads every tenant and gets a group for each
     * tenant, and any other session gets a group for each party it may read.
     * The read needs the permission to view batches.
     */
    void get_grid_usage(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!ores::service::messaging::has_permission(ctx, "compute::batches:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        get_grid_usage_response resp;
        try {
            repository::compute_telemetry_repository repo;
            auto usage = repo.grid_usage_by_scope(ctx);
            resp.group_kind = std::move(usage.group_kind);
            resp.keys = std::move(usage.keys);
            resp.batches = std::move(usage.batches);
            resp.workunits = std::move(usage.workunits);
            resp.results = std::move(usage.results);
            resp.results_unsent = std::move(usage.results_unsent);
            resp.results_in_progress = std::move(usage.results_in_progress);
            resp.results_done = std::move(usage.results_done);
            resp.outcomes_success = std::move(usage.outcomes_success);
            resp.outcomes_failed = std::move(usage.outcomes_failed);
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn) << "get_grid_usage failed: " << e.what();
            resp = get_grid_usage_response{};
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Completed " << msg.subject;
    }

    /**
     * @brief Handle compute.v1.ops.get_node_series (request/reply).
     *
     * Returns the stored node samples of a time range as columns on a regular
     * time axis, one row of buckets per node. It follows get_grid_series: the
     * same axis, the same permission, and a range the repository refuses answers
     * with the reason and no data.
     */
    void get_node_series(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!ores::service::messaging::has_permission(ctx, "compute::grid_samples:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        get_node_series_response resp;
        const auto req = decode<get_node_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            resp.message = "Failed to decode request";
            reply(nats_, msg, resp);
            return;
        }

        try {
            using ores::platform::time::datetime;
            repository::compute_telemetry_repository repo;
            auto series = repo.node_series_in_range(ctx,
                                                    datetime::from_iso8601_utc(req->start_time),
                                                    datetime::from_iso8601_utc(req->end_time),
                                                    req->bucket_seconds);
            resp.start_time = datetime::to_iso8601_utc(series.start);
            resp.bucket_seconds = series.bucket_seconds;
            resp.bucket_count = series.bucket_count;
            resp.nodes.reserve(series.nodes.size());
            for (auto& node : series.nodes)
                resp.nodes.push_back({std::move(node.host_id), std::move(node.machine)});
            resp.samples = std::move(series.samples);
            resp.tasks_completed = std::move(series.tasks_completed);
            resp.tasks_failed = std::move(series.tasks_failed);
            resp.tasks_since_last = std::move(series.tasks_since_last);
            resp.avg_task_duration_ms = std::move(series.avg_task_duration_ms);
            resp.max_task_duration_ms = std::move(series.max_task_duration_ms);
            resp.input_bytes_fetched = std::move(series.input_bytes_fetched);
            resp.output_bytes_uploaded = std::move(series.output_bytes_uploaded);
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn) << "get_node_series refused: " << e.what();
            resp = get_node_series_response{};
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(telemetry_handler_lg(), debug) << "Completed " << msg.subject;
    }

    /**
     * @brief Handle compute.v1.ops.node_sample (fire-and-forget
     * publish from wrapper nodes).
     *
     * Deserialises the node_sample_message and persists it to the
     * ores_compute_node_samples_tbl hypertable.
     */
    void ingest_node_sample(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), trace)
            << "Ingesting node sample from: " << msg.subject;

        const auto parsed = decode<node_sample_message>(msg);
        if (!parsed) {
            return;
        }

        try {
            domain::node_sample s;
            s.id = boost::uuids::random_generator()();
            s.sampled_at = std::chrono::system_clock::now();
            s.host_id = boost::lexical_cast<boost::uuids::uuid>(parsed->host_id);
            s.machine = parsed->machine;
            s.tasks_completed = parsed->tasks_completed;
            s.tasks_failed = parsed->tasks_failed;
            s.tasks_since_last = parsed->tasks_since_last;
            s.avg_task_duration_ms = parsed->avg_task_duration_ms;
            s.max_task_duration_ms = parsed->max_task_duration_ms;
            s.input_bytes_fetched = parsed->input_bytes_fetched;
            s.output_bytes_uploaded = parsed->output_bytes_uploaded;
            s.seconds_since_hb = parsed->seconds_since_hb;

            repository::node_sample_repository node_repo;

            // The wrapper's --tenant-id is a NATS routing label (e.g.
            // "ores.dev.local1"), not a database UUID.  Attempt to parse it as
            // a UUID; if that fails, fall back to the service's own context.
            const auto tid = utility::uuid::tenant_id::from_string(parsed->tenant_id);
            if (tid) {
                s.tenant_id = *tid;
                auto write_ctx = ctx_.with_tenant(*tid, "telemetry_handler");
                node_repo.write(write_ctx, s);
            } else {
                s.tenant_id = ctx_.tenant_id();
                node_repo.write(ctx_, s);
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
