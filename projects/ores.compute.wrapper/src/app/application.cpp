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

#include <algorithm>

#include <atomic>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <span>
#include <thread>
#include <rfl/json.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.compute/messaging/work_protocol.hpp"
#include "ores.compute/messaging/result_protocol.hpp"
#include "ores.compute.wrapper/net/http_client.hpp"

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

namespace fs = std::filesystem;

/**
 * @brief Build a URL from a base URL and a path.
 *
 * If base_url is empty, path is returned unchanged (already a full URL).
 */
std::string make_url(const std::string& base_url, const std::string& path) {
    if (base_url.empty())
        return path;
    if (!path.empty() && path[0] == '/')
        return base_url + path;
    return base_url + "/" + path;
}

/**
 * @brief Engine package manifest — describes how to invoke the engine.
 */
struct manifest {
    std::string executable;
    std::vector<std::string> args;
};

/**
 * @brief Read and parse manifest.json from the unpacked package directory.
 */
manifest read_manifest(const fs::path& package_dir) {
    const auto manifest_path = package_dir / "manifest.json";
    std::ifstream f(manifest_path);
    if (!f)
        throw std::runtime_error("manifest.json not found in " + package_dir.string());
    // Extra parentheses avoid the vexing parse.
    const std::string json((std::istreambuf_iterator<char>(f)),
                            std::istreambuf_iterator<char>());
    const auto m = rfl::json::read<manifest>(json);
    if (!m)
        throw std::runtime_error("Failed to parse manifest.json: " + m.error().what());
    return *m;
}

/**
 * @brief Replace {input}, {config}, {output} placeholders in argument list.
 */
std::vector<std::string> substitute_args(const std::vector<std::string>& tmpl,
    const std::string& input_path,
    const std::string& config_path,
    const std::string& output_path) {

    std::vector<std::string> result;
    result.reserve(tmpl.size());
    for (const auto& arg : tmpl) {
        std::string s = arg;
        auto replace_all = [&](const std::string& from, const std::string& to) {
            std::string::size_type pos = 0;
            while ((pos = s.find(from, pos)) != std::string::npos) {
                s.replace(pos, from.size(), to);
                pos += to.size();
            }
        };
        replace_all("{input}",  input_path);
        replace_all("{config}", config_path);
        replace_all("{output}", output_path);
        result.push_back(std::move(s));
    }
    return result;
}

/**
 * @brief Process one work assignment.
 *
 * 1. Download and cache the engine package (if not already cached).
 * 2. Download input and config files for this job.
 * 3. Spawn the engine subprocess with substituted argument placeholders.
 * 4. Upload the output file on success.
 * 5. Submit the result via NATS.
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

    int outcome = 0;
    std::string output_uri = evt.output_uri;

    try {
        // 1. Download and unpack package if not cached
        const fs::path pkg_cache_dir =
            fs::path(cfg.work_dir) / "packages" / evt.app_version_id;
        if (!fs::exists(pkg_cache_dir / "manifest.json")) {
            BOOST_LOG_SEV(lg, info) << "Downloading package: " << evt.app_version_id;
            const fs::path pkg_archive =
                pkg_cache_dir.parent_path() / (evt.app_version_id + ".tar.gz");
            net::http_client::download(make_url(cfg.http_base_url, evt.package_uri),
                pkg_archive);

            fs::create_directories(pkg_cache_dir);
            const auto tar_cmd = "tar -xzf " + pkg_archive.string() +
                " -C " + pkg_cache_dir.string();
            if (std::system(tar_cmd.c_str()) != 0) // NOLINT(cert-env33-c)
                throw std::runtime_error("Failed to unpack: " + pkg_archive.string());
        }

        const auto mf = read_manifest(pkg_cache_dir);

        // 2. Per-job scratch directory
        const fs::path job_dir =
            fs::path(cfg.work_dir) / "jobs" / evt.result_id;
        fs::create_directories(job_dir);

        const fs::path input_path  = job_dir / "input";
        const fs::path config_path = job_dir / "config";
        const fs::path output_path = job_dir / "output";

        BOOST_LOG_SEV(lg, info) << "Downloading input and config";
        net::http_client::download(make_url(cfg.http_base_url, evt.input_uri), input_path);
        net::http_client::download(make_url(cfg.http_base_url, evt.config_uri), config_path);

        // 3. Spawn engine — build a shell command from the manifest args
        const fs::path exe = pkg_cache_dir / mf.executable;
        const auto args = substitute_args(mf.args,
            input_path.string(), config_path.string(), output_path.string());

        // Build the full command string (simple shell invocation)
        std::string cmd = exe.string();
        for (const auto& arg : args) {
            cmd += ' ';
            cmd += arg; // simple quoting: relies on paths without spaces
        }

        BOOST_LOG_SEV(lg, info) << "Launching engine: " << cmd;
        const int exit_code = std::system(cmd.c_str()); // NOLINT(cert-env33-c)

        if (exit_code != 0) {
            BOOST_LOG_SEV(lg, error) << "Engine exited with code " << exit_code;
            outcome = 1;
        } else {
            // 4. Upload output
            BOOST_LOG_SEV(lg, info) << "Uploading output: " << evt.result_id;
            net::http_client::upload(make_url(cfg.http_base_url, evt.output_uri), output_path);
            BOOST_LOG_SEV(lg, info) << "Job complete: " << evt.result_id;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Job failed: " << e.what();
        outcome = 1;
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
    BOOST_LOG_SEV(lg(), info) << "HTTP Base : " << (cfg.http_base_url.empty() ? "(none)" : cfg.http_base_url);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url;

    // Subscribe to work assignments for our tenant.
    // Uses a durable queue consumer so multiple wrapper instances on the same
    // tenant share the load and survive restarts.
    const std::string work_subject =
        "compute.v1.work.assignments." + cfg.tenant_id;

    // NATS consumer (durable) names cannot contain dots; replace with hyphens.
    std::string sanitised_tenant = cfg.tenant_id;
    std::replace(sanitised_tenant.begin(), sanitised_tenant.end(), '.', '-');
    const std::string durable_name = "ores-compute-wrapper-" + sanitised_tenant;
    const std::string queue_group  = "ores-compute-wrapper";

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
