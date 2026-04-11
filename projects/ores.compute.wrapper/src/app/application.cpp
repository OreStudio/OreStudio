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
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/connect_pipe.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/read.hpp>
#include <boost/asio/readable_pipe.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/asio/writable_pipe.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/process/v2/process.hpp>
#include <boost/process/v2/start_dir.hpp>
#include <boost/process/v2/stdio.hpp>
#include <rfl/json.hpp>
#include "ores.compute.wrapper/filesystem/archiver.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.compute.wrapper/net/http_client.hpp"
#include "ores.compute.wrapper/app/log_publisher.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"

namespace ores::compute::wrapper::app {

using namespace ores::logging;

namespace {
constexpr std::string_view service_name = "ores.compute.wrapper";
constexpr std::string_view service_version = ORES_VERSION;


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
    const std::string& host_id,
    const std::string& output_uri,
    int outcome,
    const std::string& error_message,
    auto& lg) {

    compute::messaging::submit_result_request req;
    req.result_id = result_id;
    req.host_id = host_id;
    req.output_uri = output_uri;
    req.outcome = outcome;
    req.error_message = error_message;

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
 * @brief Format a command line as a shell-pasteable string for diagnostics.
 *
 * Each token is single-quoted so the output can be pasted verbatim into a
 * shell even if paths contain spaces.
 */
std::string format_cmdline(const std::string& exe,
    const std::vector<std::string>& args) {
    std::string s = "'" + exe + "'";
    for (const auto& a : args)
        s += " '" + a + "'";
    return s;
}

/**
 * @brief Read up to max_lines from a text file, returning them joined by '\n'.
 */
std::string tail_file(const fs::path& path, int max_lines = 50) {
    std::ifstream f(path);
    if (!f) return {};
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(f, line))
        lines.push_back(std::move(line));
    const int start = static_cast<int>(lines.size()) > max_lines
        ? static_cast<int>(lines.size()) - max_lines : 0;
    std::string out;
    for (int i = start; i < static_cast<int>(lines.size()); ++i)
        out += lines[i] + "\n";
    return out;
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
 * 6. If a reporter is provided, record the task outcome and timing.
 */
void process_assignment(ores::nats::service::client& nats,
    const compute::messaging::work_assignment_event& evt,
    const config::options& cfg,
    node_stats_reporter* reporter,
    auto& lg) {

    BOOST_LOG_SEV(lg, debug) << "Processing assignment: result=" << evt.result_id
                              << " workunit=" << evt.workunit_id
                              << " app_version=" << evt.app_version_id;

    heartbeat_sender hb(nats, cfg.host_id, cfg.heartbeat_interval_seconds, reporter);
    hb.start();

    // Outcome codes match the domain: 1=Success, 3=ClientError.
    // Start with ClientError; success path will set it to 1.
    int outcome = 3;
    std::string output_uri = evt.output_uri;
    std::string error_message;
    std::int64_t input_bytes = 0;
    std::int64_t output_bytes = 0;
    std::int64_t duration_ms = 0;

    try {
        // 1. Download and unpack package if not cached
        const fs::path pkg_cache_dir =
            fs::path(cfg.work_dir) / "packages" / evt.app_version_id;
        if (!fs::exists(pkg_cache_dir / "manifest.json")) {
            BOOST_LOG_SEV(lg, debug) << "Downloading package: " << evt.app_version_id;
            const fs::path pkg_archive =
                pkg_cache_dir.parent_path() / (evt.app_version_id + ".tar.gz");
            net::http_client::download(make_url(cfg.http_base_url, evt.package_uri),
                pkg_archive);

            fs::create_directories(pkg_cache_dir);
            ores::compute::wrapper::filesystem::archiver::extract(
                pkg_archive, pkg_cache_dir);
        }

        const auto mf = read_manifest(pkg_cache_dir);

        // 2. Per-job scratch directory — input archive is extracted here.
        //    ORE runs with job_dir as its working directory so that relative
        //    paths in ore.xml (inputs, outputs, log.txt) resolve correctly.
        const fs::path job_dir =
            fs::path(cfg.work_dir) / "jobs" / evt.result_id;
        fs::create_directories(job_dir);

        // Download input archive and unpack into job_dir.
        const fs::path input_archive = job_dir / "input.tar.gz";
        BOOST_LOG_SEV(lg, debug) << "Downloading input";
        net::http_client::download(make_url(cfg.http_base_url, evt.input_uri),
            input_archive);
        input_bytes = static_cast<std::int64_t>(fs::file_size(input_archive));
        BOOST_LOG_SEV(lg, debug) << "Extracting input (" << input_bytes << " bytes)"
            << " to: " << job_dir.string();
        ores::compute::wrapper::filesystem::archiver::extract(
            input_archive, job_dir);

        // 3. Spawn engine using Boost.Process to avoid shell injection.
        //    Args in the manifest are passed verbatim; no placeholder substitution
        //    is needed for engines (like ORE) whose config is fully self-contained
        //    inside the input archive.
        const fs::path exe = fs::canonical(pkg_cache_dir / mf.executable);
        const auto& args = mf.args;

        // Log exact command line so it can be reproduced manually.
        const fs::path engine_log_path = job_dir / "engine.log";
        BOOST_LOG_SEV(lg, debug) << "Engine command: "
            << format_cmdline(exe.string(), args);
        BOOST_LOG_SEV(lg, debug) << "Engine log:     " << engine_log_path.string();
        BOOST_LOG_SEV(lg, debug) << "Input dir:      " << job_dir.string();
        BOOST_LOG_SEV(lg, debug) << "ORE log:        " << (job_dir / "Output" / "log.txt").string();

        namespace bp2 = boost::process::v2;
        boost::asio::io_context proc_ioc;

        // Redirect engine stdout+stderr to engine.log via pipes (cross-platform).
        boost::asio::readable_pipe out_r(proc_ioc), err_r(proc_ioc);
        boost::asio::writable_pipe out_w(proc_ioc), err_w(proc_ioc);
        boost::asio::connect_pipe(out_r, out_w);
        boost::asio::connect_pipe(err_r, err_w);

        const auto engine_start = std::chrono::steady_clock::now();
        bp2::process engine_proc(
            boost::asio::any_io_executor(proc_ioc.get_executor()),
            boost::filesystem::path(exe.string()), args,
            bp2::process_start_dir(job_dir.string()),
            bp2::process_stdio{.in = nullptr, .out = out_w, .err = err_w});

        // Close write ends so readers see EOF when the process exits.
        out_w.close();
        err_w.close();

        // Drain stdout and stderr concurrently to avoid pipe-buffer deadlock.
        std::string out_buf, err_buf;
        boost::system::error_code out_ec, err_ec;
        std::thread t_out([&] {
            boost::asio::read(out_r, boost::asio::dynamic_buffer(out_buf), out_ec);
        });
        std::thread t_err([&] {
            boost::asio::read(err_r, boost::asio::dynamic_buffer(err_buf), err_ec);
        });
        t_out.join();
        t_err.join();

        engine_proc.wait();
        const auto engine_end = std::chrono::steady_clock::now();
        duration_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
            engine_end - engine_start).count();

        // Write combined output to engine.log.
        {
            std::ofstream engine_log(engine_log_path,
                std::ios::out | std::ios::trunc);
            if (engine_log) {
                if (!out_buf.empty()) engine_log << out_buf;
                if (!err_buf.empty()) engine_log << err_buf;
            } else {
                BOOST_LOG_SEV(lg, warn) << "Cannot open engine log: "
                    << engine_log_path.string();
            }
        }

        const int exit_code = engine_proc.exit_code();

        if (exit_code != 0) {
            // Include ORE's own log (log.txt) in the error — it has more
            // detail than stdout/stderr. Fall back to engine.log (our pipe
            // capture) if log.txt wasn't produced.
            const fs::path ore_log_path = job_dir / "Output" / "log.txt";
            const auto ore_log = fs::exists(ore_log_path)
                ? tail_file(ore_log_path) : std::string{};
            const auto engine_output = ore_log.empty()
                ? tail_file(engine_log_path) : ore_log;
            error_message = "Engine exited with code "
                + std::to_string(exit_code)
                + (engine_output.empty() ? "" : "\n" + engine_output);
            BOOST_LOG_SEV(lg, error) << "Engine exited with code " << exit_code;
            if (!engine_output.empty())
                BOOST_LOG_SEV(lg, error) << "Engine output:\n" << engine_output;
            outcome = 3; // ClientError
        } else {
            // 4. Upload output.
            // ORE writes multiple files to Output/ as specified in ore.xml.
            // Pack the whole directory into a tar.gz and upload it as one blob.
            const fs::path output_dir = job_dir / "Output";
            if (!fs::exists(output_dir) || fs::is_empty(output_dir)) {
                BOOST_LOG_SEV(lg, warn) << "No output found in: "
                    << output_dir.string();
            } else {
                const fs::path output_archive = job_dir / "output.tar.gz";
                ores::compute::wrapper::filesystem::archiver::pack(
                    output_dir, output_archive);

                output_bytes = static_cast<std::int64_t>(
                    fs::file_size(output_archive));
                BOOST_LOG_SEV(lg, debug) << "Uploading output archive: "
                    << output_archive.string()
                    << " (" << output_bytes << " bytes)";
                net::http_client::upload(
                    make_url(cfg.http_base_url, evt.output_uri),
                    output_archive);
            }
            BOOST_LOG_SEV(lg, info) << "Job complete: " << evt.result_id;
            outcome = 1; // Success
        }
    } catch (const std::exception& e) {
        error_message = e.what();
        BOOST_LOG_SEV(lg, error) << "Job failed: " << e.what();
        outcome = 3; // ClientError
    }

    hb.stop();

    {
        const fs::path log_job_dir =
            fs::path(cfg.work_dir) / "jobs" / evt.result_id;
        publish_ore_logs(nats, evt.result_id, log_job_dir);
        publish_engine_logs(nats, evt.result_id, log_job_dir);
    }

    // Record outcome in the telemetry reporter.
    if (reporter) {
        if (outcome == 1)
            reporter->record_task_completed(duration_ms, input_bytes, output_bytes);
        else
            reporter->record_task_failed();
    }

    submit_result(nats, evt.result_id, cfg.host_id, output_uri, outcome, error_message, lg);
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
    BOOST_LOG_SEV(lg(), info) << "HTTP Base : "
        << (cfg.http_base_url.empty() ? "(none)" : cfg.http_base_url);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url
                              << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" : cfg.nats.subject_prefix)
                              << "')";

    // Ensure the durable compute assignments stream exists. Idempotent.
    try {
        const auto stream_name = nats.make_stream_name("compute_assignments");
        auto admin = nats.make_admin();
        admin.ensure_stream(stream_name, {
            nats.make_subject("compute.v1.work.assignments.>")
        });
        BOOST_LOG_SEV(lg(), info) << "Compute JetStream stream ready: " << stream_name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure compute stream: " << e.what();
        throw;
    }

    // Idle heartbeat — compute-domain signal; tells the compute service this
    // worker is alive even when no job is running (subject: compute.v1.work.heartbeat).
    heartbeat_sender idle_hb(nats, cfg.host_id, cfg.heartbeat_interval_seconds, nullptr);

    // Node telemetry reporter — publishes per-node metrics to
    // compute.v1.telemetry.node_samples at the configured interval.
    std::unique_ptr<node_stats_reporter> reporter;
    if (cfg.telemetry_interval_seconds > 0) {
        reporter = std::make_unique<node_stats_reporter>(
            nats, cfg.host_id, cfg.tenant_id, cfg.telemetry_interval_seconds);
    }
    node_stats_reporter* raw_reporter = reporter.get();

    // Durable consumer name scoped to this environment to avoid collisions.
    std::string sanitised_tenant = cfg.tenant_id;
    std::replace(sanitised_tenant.begin(), sanitised_tenant.end(), '.', '_');
    const std::string work_subject = "compute.v1.work.assignments.>";
    const std::string durable_name = "compute_wrapper_" + sanitised_tenant;
    const std::string queue_group  = "ores.compute.wrapper";

    co_await ores::service::service::run(
        io_ctx, nats, service_name,
        [&nats, &cfg, raw_reporter, &work_subject, &durable_name, &queue_group,
         this](auto& n, auto /*verifier*/) {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to: " << work_subject;
            auto sub = n.js_queue_subscribe(
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
            std::vector<ores::nats::service::subscription> subs;
            subs.push_back(std::move(sub));
            return subs;
        },
        [&](boost::asio::io_context& ioc) {
            // Service-health heartbeat — standard subject used by the service
            // dashboard (telemetry.v1.services.heartbeat).
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc,
                [hb]() { return hb->run(); },
                boost::asio::detached);

            // Compute-domain idle heartbeat and node telemetry reporter.
            idle_hb.start();
            BOOST_LOG_SEV(lg(), info) << "Idle heartbeat sender started ("
                << cfg.heartbeat_interval_seconds << "s interval).";
            if (reporter) {
                reporter->start();
                BOOST_LOG_SEV(lg(), info) << "Node telemetry reporter started ("
                    << cfg.telemetry_interval_seconds << "s interval).";
            }
        },
        [&]() {
            idle_hb.stop();
            if (reporter) {
                reporter->stop();
                BOOST_LOG_SEV(lg(), info) << "Node telemetry reporter stopped.";
            }
        });
    co_return;
}

}
