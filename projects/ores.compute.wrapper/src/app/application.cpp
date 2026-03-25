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
#include <csignal>
#include <format>
#include <filesystem>
#include <fstream>
#include <mutex>
#include <sstream>
#include <span>
#include <thread>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/process/v2/process.hpp>
#include <archive.h>
#include <archive_entry.h>
#include <rfl/json.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.compute.wrapper/net/http_client.hpp"

namespace ores::compute::wrapper::app {

using namespace ores::logging;

namespace {

/**
 * @brief Format a time_point as ISO-8601 UTC string (YYYY-MM-DDTHH:MM:SSZ).
 */
std::string to_iso8601(std::chrono::system_clock::time_point tp) {
    return std::format("{:%Y-%m-%dT%H:%M:%SZ}", tp);
}

/**
 * @brief Accumulates per-node execution metrics and publishes them to NATS
 *        at a fixed interval.
 *
 * Publishes node_sample_message to compute.v1.telemetry.node_samples every
 * telemetry_interval_seconds. Cumulative counters (tasks_completed,
 * tasks_failed) increase monotonically; interval counters (tasks_since_last,
 * durations, bytes) are reset after each publish.
 */
class node_stats_reporter {
public:
    node_stats_reporter(ores::nats::service::client& nats,
        const std::string& host_id,
        const std::string& tenant_id,
        std::uint32_t interval_seconds)
        : nats_(nats), host_id_(host_id), tenant_id_(tenant_id),
          interval_(interval_seconds) {}

    void start() {
        stop_ = false;
        thread_ = std::thread([this]() { run(); });
    }

    void stop() {
        stop_ = true;
        if (thread_.joinable())
            thread_.join();
    }

    ~node_stats_reporter() { stop(); }

    /**
     * @brief Record a successfully completed task.
     *
     * @param duration_ms  Wall-clock duration of engine execution (ms).
     * @param input_bytes  Bytes of input data fetched for this task.
     * @param output_bytes Bytes of output data uploaded for this task.
     */
    void record_task_completed(std::int64_t duration_ms,
                                std::int64_t input_bytes,
                                std::int64_t output_bytes) {
        ++tasks_completed_;
        std::lock_guard lock(interval_mutex_);
        ++tasks_since_last_;
        duration_sum_ += duration_ms;
        if (duration_ms > max_duration_)
            max_duration_ = duration_ms;
        input_bytes_ += input_bytes;
        output_bytes_ += output_bytes;
    }

    /**
     * @brief Record a failed task.
     */
    void record_task_failed() {
        ++tasks_failed_;
    }

    /**
     * @brief Notify that a heartbeat was successfully sent.
     *
     * Used to compute seconds_since_hb in the published sample.
     */
    void notify_heartbeat() {
        std::lock_guard lock(hb_mutex_);
        last_heartbeat_ = std::chrono::steady_clock::now();
        heartbeat_ever_sent_ = true;
    }

private:
    inline static std::string_view logger_name =
        "ores.compute.wrapper.app.node_stats_reporter";
    static auto& lg() {
        static auto instance = ores::logging::make_logger(logger_name);
        return instance;
    }

    void run() {
        using namespace std::chrono_literals;
        while (!stop_) {
            // Sleep in 1-second ticks for responsive shutdown.
            for (std::uint32_t i = 0; i < interval_ && !stop_; ++i) {
                std::this_thread::sleep_for(1s);
            }
            if (!stop_)
                publish_sample();
        }
    }

    void publish_sample() {
        // Snapshot and reset interval accumulators under the lock.
        int tasks_since_last = 0;
        std::int64_t duration_sum = 0;
        std::int64_t max_duration = 0;
        std::int64_t input_bytes = 0;
        std::int64_t output_bytes = 0;
        {
            std::lock_guard lock(interval_mutex_);
            tasks_since_last = tasks_since_last_;
            duration_sum     = duration_sum_;
            max_duration     = max_duration_;
            input_bytes      = input_bytes_;
            output_bytes     = output_bytes_;
            tasks_since_last_ = 0;
            duration_sum_     = 0;
            max_duration_     = 0;
            input_bytes_      = 0;
            output_bytes_     = 0;
        }

        // Compute seconds since last heartbeat.
        int seconds_since_hb = 0;
        {
            std::lock_guard lock(hb_mutex_);
            if (heartbeat_ever_sent_) {
                const auto elapsed =
                    std::chrono::steady_clock::now() - last_heartbeat_;
                seconds_since_hb = static_cast<int>(
                    std::chrono::duration_cast<std::chrono::seconds>(elapsed)
                    .count());
            }
        }

        const std::int64_t avg_ms =
            (tasks_since_last > 0) ? (duration_sum / tasks_since_last) : 0;

        compute::messaging::node_sample_message msg;
        msg.tenant_id            = tenant_id_;
        msg.host_id              = host_id_;
        msg.sampled_at           = to_iso8601(std::chrono::system_clock::now());
        msg.tasks_completed      = tasks_completed_.load();
        msg.tasks_failed         = tasks_failed_.load();
        msg.tasks_since_last     = tasks_since_last;
        msg.avg_task_duration_ms = avg_ms;
        msg.max_task_duration_ms = max_duration;
        msg.input_bytes_fetched  = input_bytes;
        msg.output_bytes_uploaded = output_bytes;
        msg.seconds_since_hb     = seconds_since_hb;

        try {
            const auto json = rfl::json::write(msg);
            const auto* p = reinterpret_cast<const std::byte*>(json.data());
            nats_.publish(compute::messaging::node_sample_message::nats_subject,
                std::span<const std::byte>(p, json.size()));
            BOOST_LOG_SEV(lg(), debug)
                << "Published node telemetry sample: tasks_since_last="
                << tasks_since_last
                << " avg_ms=" << avg_ms
                << " input_bytes=" << input_bytes
                << " output_bytes=" << output_bytes;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Node telemetry publish failed: " << e.what();
        }
    }

    ores::nats::service::client& nats_;
    std::string host_id_;
    std::string tenant_id_;
    std::uint32_t interval_;
    std::atomic<bool> stop_{false};
    std::thread thread_;

    // Cumulative counters (individually atomic — no lock needed).
    std::atomic<int> tasks_completed_{0};
    std::atomic<int> tasks_failed_{0};

    // Per-interval accumulators (protected by interval_mutex_).
    std::mutex interval_mutex_;
    int tasks_since_last_{0};
    std::int64_t duration_sum_{0};
    std::int64_t max_duration_{0};
    std::int64_t input_bytes_{0};
    std::int64_t output_bytes_{0};

    // Last heartbeat tracking (protected by hb_mutex_).
    std::mutex hb_mutex_;
    std::chrono::steady_clock::time_point last_heartbeat_{};
    bool heartbeat_ever_sent_{false};
};

/**
 * @brief Sends heartbeat messages on a background thread while a job is running.
 *
 * Publishes to compute.v1.work.heartbeat every heartbeat_interval_seconds.
 * Optionally notifies a node_stats_reporter each time a heartbeat is sent,
 * so the reporter can track seconds_since_hb accurately.
 * Stops when stop_ is set to true.
 */
class heartbeat_sender {
public:
    heartbeat_sender(ores::nats::service::client& nats,
        const std::string& host_id,
        std::uint32_t interval_seconds,
        node_stats_reporter* reporter = nullptr)
        : nats_(nats), host_id_(host_id), interval_(interval_seconds),
          reporter_(reporter) {}

    void start() {
        stop_ = false;
        thread_ = std::thread([this]() { run(); });
    }

    void stop() {
        stop_ = true;
        if (thread_.joinable())
            thread_.join();
    }

    ~heartbeat_sender() { stop(); }

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
            if (reporter_)
                reporter_->notify_heartbeat();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Heartbeat publish failed: " << e.what();
        }
    }

    ores::nats::service::client& nats_;
    std::string host_id_;
    std::uint32_t interval_;
    node_stats_reporter* reporter_;
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
 * @brief Extract a .tar.gz archive to a destination directory using libarchive.
 *
 * Throws std::runtime_error on failure.
 */
void extract_tar_gz(const fs::path& archive_path, const fs::path& dest_dir) {
    struct archive* a = archive_read_new();
    archive_read_support_filter_gzip(a);
    archive_read_support_format_tar(a);

    struct archive* out = archive_write_disk_new();
    archive_write_disk_set_options(out,
        ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM |
        ARCHIVE_EXTRACT_ACL  | ARCHIVE_EXTRACT_FFLAGS);
    archive_write_disk_set_standard_lookup(out);

#ifdef _WIN32
    const int open_rc = archive_read_open_filename_w(
        a, archive_path.c_str(), 16384);
#else
    const int open_rc = archive_read_open_filename(
        a, archive_path.c_str(), 16384);
#endif
    if (open_rc != ARCHIVE_OK) {
        archive_read_free(a);
        archive_write_free(out);
        throw std::runtime_error(
            "Failed to open archive: " + archive_path.string() +
            " — " + archive_error_string(a));
    }

    struct archive_entry* entry = nullptr;
    while (true) {
        const int r = archive_read_next_header(a, &entry);
        if (r == ARCHIVE_EOF) break;
        if (r < ARCHIVE_OK)
            throw std::runtime_error(
                std::string("archive read error: ") + archive_error_string(a));

        // Rebase entry path under dest_dir
        const std::string entry_path =
            (dest_dir / archive_entry_pathname(entry)).string();
        archive_entry_set_pathname(entry, entry_path.c_str());

        const int wh = archive_write_header(out, entry);
        if (wh < ARCHIVE_OK)
            throw std::runtime_error(
                std::string("archive write header error: ") +
                archive_error_string(out));

        if (archive_entry_size(entry) > 0) {
            const void* buff = nullptr;
            std::size_t size = 0;
            la_int64_t offset = 0;
            while (true) {
                const int rd = archive_read_data_block(a, &buff, &size, &offset);
                if (rd == ARCHIVE_EOF) break;
                if (rd < ARCHIVE_OK)
                    throw std::runtime_error(
                        std::string("archive data read error: ") +
                        archive_error_string(a));
                if (archive_write_data_block(out, buff, size, offset) < ARCHIVE_OK)
                    throw std::runtime_error(
                        std::string("archive data write error: ") +
                        archive_error_string(out));
            }
        }
        archive_write_finish_entry(out);
    }

    archive_read_close(a);
    archive_read_free(a);
    archive_write_close(out);
    archive_write_free(out);
}

/**
 * @brief Process one work assignment.
 *
 * 1. Download and cache the engine package (if not already cached).
 * 2. Download input and config files for this job.
 * 3. Spawn the engine subprocess with substituted argument placeholders.
 * 4. Upload the output file on success.
 * 5. Submit the result via NATS.
 * 6. If a reporter is provided, record the task outcome and timing.
 */
void process_assignment(ores::nats::service::client& nats,
    const compute::messaging::work_assignment_event& evt,
    const config::options& cfg,
    node_stats_reporter* reporter,
    auto& lg) {

    BOOST_LOG_SEV(lg, info) << "Processing assignment: result=" << evt.result_id
                             << " workunit=" << evt.workunit_id
                             << " app_version=" << evt.app_version_id;

    heartbeat_sender hb(nats, cfg.host_id, cfg.heartbeat_interval_seconds, reporter);
    hb.start();

    int outcome = 0;
    std::string output_uri = evt.output_uri;
    std::int64_t input_bytes = 0;
    std::int64_t output_bytes = 0;
    std::int64_t duration_ms = 0;

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
            extract_tar_gz(pkg_archive, pkg_cache_dir);
        }

        const auto mf = read_manifest(pkg_cache_dir);

        // 2. Per-job scratch directory
        const fs::path job_dir =
            fs::path(cfg.work_dir) / "jobs" / evt.result_id;
        fs::create_directories(job_dir);

        const fs::path input_path  = job_dir / "input";
        const fs::path config_path = job_dir / "config";
        const fs::path output_path = job_dir / "output";

        BOOST_LOG_SEV(lg, info) << "Downloading input";
        net::http_client::download(make_url(cfg.http_base_url, evt.input_uri), input_path);
        if (!evt.config_uri.empty()) {
            BOOST_LOG_SEV(lg, info) << "Downloading config";
            net::http_client::download(make_url(cfg.http_base_url, evt.config_uri), config_path);
        }

        // Measure downloaded input bytes for telemetry.
        if (fs::exists(input_path))
            input_bytes = static_cast<std::int64_t>(fs::file_size(input_path));

        // 3. Spawn engine using Boost.Process to avoid shell injection.
        const fs::path exe = pkg_cache_dir / mf.executable;
        const auto args = substitute_args(mf.args,
            input_path.string(), config_path.string(), output_path.string());

        BOOST_LOG_SEV(lg, info) << "Launching engine: " << exe.string();
        namespace bp2 = boost::process::v2;
        boost::asio::io_context proc_ioc;

        const auto engine_start = std::chrono::steady_clock::now();
        bp2::process engine_proc(
            boost::asio::any_io_executor(proc_ioc.get_executor()),
            boost::filesystem::path(exe.string()), args);
        engine_proc.wait();
        const auto engine_end = std::chrono::steady_clock::now();
        duration_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
            engine_end - engine_start).count();

        const int exit_code = engine_proc.exit_code();

        if (exit_code != 0) {
            BOOST_LOG_SEV(lg, error) << "Engine exited with code " << exit_code;
            outcome = 1;
        } else {
            // 4. Upload output
            BOOST_LOG_SEV(lg, info) << "Uploading output: " << evt.result_id;
            if (fs::exists(output_path))
                output_bytes = static_cast<std::int64_t>(fs::file_size(output_path));
            net::http_client::upload(make_url(cfg.http_base_url, evt.output_uri), output_path);
            BOOST_LOG_SEV(lg, info) << "Job complete: " << evt.result_id;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Job failed: " << e.what();
        outcome = 1;
    }

    hb.stop();

    // Record outcome in the telemetry reporter.
    if (reporter) {
        if (outcome == 0)
            reporter->record_task_completed(duration_ms, input_bytes, output_bytes);
        else
            reporter->record_task_failed();
    }

    submit_result(nats, evt.result_id, output_uri, outcome, lg);
}

} // namespace

boost::asio::awaitable<void>
application::run(boost::asio::io_context& io_ctx,
    const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.compute.wrapper", 0, 1);
    BOOST_LOG_SEV(lg(), info) << "Host ID   : " << cfg.host_id;
    BOOST_LOG_SEV(lg(), info) << "Tenant ID : " << cfg.tenant_id;
    BOOST_LOG_SEV(lg(), info) << "Work dir  : " << cfg.work_dir;
    BOOST_LOG_SEV(lg(), info) << "Heartbeat : " << cfg.heartbeat_interval_seconds << "s";
    BOOST_LOG_SEV(lg(), info) << "Telemetry : "
        << (cfg.telemetry_interval_seconds > 0
            ? std::to_string(cfg.telemetry_interval_seconds) + "s"
            : "disabled");
    BOOST_LOG_SEV(lg(), info) << "HTTP Base : " << (cfg.http_base_url.empty() ? "(none)" : cfg.http_base_url);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url;

    // Idle heartbeat — keeps last_rpc_time current even when no job is running.
    heartbeat_sender idle_hb(nats, cfg.host_id, cfg.heartbeat_interval_seconds, nullptr);
    idle_hb.start();
    BOOST_LOG_SEV(lg(), info) << "Idle heartbeat sender started ("
        << cfg.heartbeat_interval_seconds << "s interval).";

    // Start telemetry reporter if enabled.
    std::unique_ptr<node_stats_reporter> reporter;
    if (cfg.telemetry_interval_seconds > 0) {
        reporter = std::make_unique<node_stats_reporter>(
            nats, cfg.host_id, cfg.tenant_id, cfg.telemetry_interval_seconds);
        reporter->start();
        BOOST_LOG_SEV(lg(), info) << "Node telemetry reporter started ("
            << cfg.telemetry_interval_seconds << "s interval).";
    }

    // Subscribe to all work assignments in this environment.
    // The NATS subject prefix already scopes assignments per environment
    // (checkout), so a wildcard here is safe. Dispatcher publishes to
    // compute.v1.work.assignments.{tenant_uuid} and wrappers consume all.
    const std::string work_subject = "compute.v1.work.assignments.>";

    // Durable consumer name is based on the tenant (NATS prefix) to avoid
    // collisions across checkouts. Dots replaced with hyphens for NATS.
    std::string sanitised_tenant = cfg.tenant_id;
    std::replace(sanitised_tenant.begin(), sanitised_tenant.end(), '.', '-');
    const std::string durable_name = "ores-compute-wrapper-" + sanitised_tenant;
    const std::string queue_group  = "ores-compute-wrapper";

    BOOST_LOG_SEV(lg(), info) << "Subscribing to: " << work_subject;

    node_stats_reporter* raw_reporter = reporter.get();
    auto sub = nats.js_queue_subscribe(
        work_subject, durable_name, queue_group,
        [&nats, &cfg, raw_reporter, this](ores::nats::message msg) {
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

            process_assignment(nats, *evt, cfg, raw_reporter, lg());
        });

    BOOST_LOG_SEV(lg(), info) << "Service ready.";
    BOOST_LOG_SEV(lg(), info) << "Waiting for work assignments...";

    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg(), info) << "Shutdown signal received. Draining...";

    idle_hb.stop();

    if (reporter) {
        reporter->stop();
        BOOST_LOG_SEV(lg(), info) << "Node telemetry reporter stopped.";
    }

    nats.drain();

    BOOST_LOG_SEV(lg(), info) << "Service stopped.";

    co_return;
}

}
