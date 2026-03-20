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
#include "ores.compute.wrapper/app/application.hpp"

#include <atomic>
#include <chrono>
#include <span>
#include <thread>
#include <rfl/json.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.compute/messaging/work_protocol.hpp"
#include "ores.compute/messaging/result_protocol.hpp"

namespace ores::compute::wrapper::app {

using namespace ores::logging;

namespace {

/**
 * @brief Sends heartbeat messages on a background thread while a job is running.
 *
 * Publishes to compute.v1.work.heartbeat every heartbeat_interval_seconds.
 * Stops when stop_ is set to true.
 */
class heartbeat_sender {
public:
    heartbeat_sender(ores::nats::service::client& nats,
        const std::string& host_id,
        std::uint32_t interval_seconds)
        : nats_(nats), host_id_(host_id), interval_(interval_seconds) {}

    void start() {
        stop_ = false;
        thread_ = std::thread([this]() { run(); });
    }

    void stop() {
        stop_ = true;
        if (thread_.joinable())
            thread_.join();
    }

private:
    void run() {
        using namespace std::chrono_literals;
        while (!stop_) {
            send_heartbeat();
            // Sleep in 1-second increments so stop_ is checked promptly.
            for (std::uint32_t i = 0; i < interval_ && !stop_; ++i) {
                std::this_thread::sleep_for(1s);
            }
        }
    }

    inline static std::string_view logger_name =
        "ores.compute.wrapper.app.heartbeat_sender";
    static auto& lg() {
        static auto instance = ores::logging::make_logger(logger_name);
        return instance;
    }

    void send_heartbeat() {
        try {
            compute::messaging::heartbeat_message msg;
            msg.host_id = host_id_;
            const auto json = rfl::json::write(msg);
            const auto* p = reinterpret_cast<const std::byte*>(json.data());
            nats_.publish(compute::messaging::heartbeat_message::nats_subject,
                std::span<const std::byte>(p, json.size()));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Heartbeat publish failed: " << e.what();
        }
    }

    ores::nats::service::client& nats_;
    std::string host_id_;
    std::uint32_t interval_;
    std::atomic<bool> stop_{false};
    std::thread thread_;
};

/**
 * @brief Submit a result back to the compute service.
 */
void submit_result(ores::nats::service::client& nats,
    const std::string& result_id,
    const std::string& output_uri,
    int outcome,
    auto& lg) {

    compute::messaging::submit_result_request req;
    req.result_id = result_id;
    req.output_uri = output_uri;
    req.outcome = outcome;

    const auto json = rfl::json::write(req);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());

    const auto reply = nats.request_sync(
        compute::messaging::submit_result_request::nats_subject,
        std::span<const std::byte>(p, json.size()));

    const std::string_view reply_data(
        reinterpret_cast<const char*>(reply.data.data()),
        reply.data.size());

    const auto resp = rfl::json::read<compute::messaging::submit_result_response>(
        reply_data);
    if (!resp || !resp->success) {
        const std::string msg = resp ? resp->message : "failed to deserialize response";
        BOOST_LOG_SEV(lg, error) << "results.submit failed: " << msg;
    } else {
        BOOST_LOG_SEV(lg, info) << "Result submitted: " << result_id;
    }
}

/**
 * @brief Process one work assignment.
 *
 * Downloads inputs, runs engine, uploads output, submits result.
 * TODO: implement HTTP download/upload and subprocess execution.
 */
void process_assignment(ores::nats::service::client& nats,
    const compute::messaging::work_assignment_event& evt,
    const config::options& cfg,
    auto& lg) {

    BOOST_LOG_SEV(lg, info) << "Processing assignment: result=" << evt.result_id
                             << " workunit=" << evt.workunit_id
                             << " app_version=" << evt.app_version_id;

    heartbeat_sender hb(nats, cfg.host_id, cfg.heartbeat_interval_seconds);
    hb.start();

    int outcome = 0;        // 0 = success
    std::string output_uri;

    try {
        // TODO: download package (if not cached), input, config via HTTP
        // TODO: unpack package, read manifest.json
        // TODO: spawn engine subprocess with substituted args
        // TODO: wait for subprocess exit
        // TODO: upload output file via HTTP PUT to evt.output_uri
        output_uri = evt.output_uri;

        BOOST_LOG_SEV(lg, warn) << "Job processing not yet implemented; "
                                 << "submitting stub success for " << evt.result_id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Job failed: " << e.what();
        outcome = 1; // failed
    }

    hb.stop();

    submit_result(nats, evt.result_id, output_uri, outcome, lg);
}

} // namespace

boost::asio::awaitable<void>
application::run(boost::asio::io_context& /*io_ctx*/,
    const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.compute.wrapper", 0, 1);
    BOOST_LOG_SEV(lg(), info) << "Host ID   : " << cfg.host_id;
    BOOST_LOG_SEV(lg(), info) << "Tenant ID : " << cfg.tenant_id;
    BOOST_LOG_SEV(lg(), info) << "Work dir  : " << cfg.work_dir;
    BOOST_LOG_SEV(lg(), info) << "Heartbeat : " << cfg.heartbeat_interval_seconds << "s";

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url;

    // Subscribe to work assignments for our tenant.
    // Uses a durable queue consumer so multiple wrapper instances on the same
    // tenant share the load and survive restarts.
    const std::string work_subject =
        "compute.v1.work.assignments." + cfg.tenant_id;
    const std::string durable_name = "ores.compute.wrapper." + cfg.tenant_id;
    const std::string queue_group  = "ores.compute.wrapper";

    BOOST_LOG_SEV(lg(), info) << "Subscribing to: " << work_subject;

    auto sub = nats.js_queue_subscribe(
        work_subject, durable_name, queue_group,
        [&nats, &cfg, this](ores::nats::message msg) {
            const std::string_view data(
                reinterpret_cast<const char*>(msg.data.data()),
                msg.data.size());

            const auto evt = rfl::json::read<
                compute::messaging::work_assignment_event>(data);

            if (!evt) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to decode work_assignment_event: "
                    << evt.error().what();
                return;
            }

            process_assignment(nats, *evt, cfg, lg());
        });

    BOOST_LOG_SEV(lg(), info) << "Waiting for work assignments...";
    nats.drain();

    co_return;
}

}
