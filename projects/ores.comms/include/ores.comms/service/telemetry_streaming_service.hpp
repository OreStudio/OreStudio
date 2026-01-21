/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_SERVICE_TELEMETRY_STREAMING_SERVICE_HPP
#define ORES_COMMS_SERVICE_TELEMETRY_STREAMING_SERVICE_HPP

#include <mutex>
#include <atomic>
#include <thread>
#include <chrono>
#include <string>
#include <vector>
#include <memory>
#include <condition_variable>
#include <boost/log/sinks/basic_sink_backend.hpp>
#include <boost/log/sinks/frontend_requirements.hpp>
#include <boost/log/core.hpp>
#include <boost/log/sinks/sync_frontend.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.telemetry/domain/log_record.hpp"
#include "ores.telemetry/domain/resource.hpp"
#include "ores.telemetry/log/telemetry_sink_backend.hpp"

namespace ores::comms::net {
class client;
}

namespace ores::comms::service {

/**
 * @brief Configuration options for telemetry streaming.
 */
struct telemetry_streaming_options {
    /**
     * @brief Name of the source application.
     *
     * Examples: "ores.qt", "ores.comms.shell"
     */
    std::string source_name;

    /**
     * @brief Version of the source application.
     */
    std::string source_version;

    /**
     * @brief Maximum number of log records per batch.
     *
     * When this threshold is reached, the batch is sent immediately.
     * Default: 50
     */
    std::size_t batch_size = 50;

    /**
     * @brief Maximum time to wait before sending a batch.
     *
     * If this interval elapses without reaching batch_size, the batch
     * is sent anyway. Default: 5 seconds
     */
    std::chrono::seconds flush_interval{5};

    /**
     * @brief Whether to retry failed submissions.
     *
     * If true, failed batches are retried on the next flush cycle.
     * Default: true
     */
    bool retry_on_failure = true;

    /**
     * @brief Maximum number of records to keep pending when disconnected.
     *
     * If this limit is reached, oldest records are dropped.
     * Default: 1000
     */
    std::size_t max_pending_records = 1000;
};

/**
 * @brief Service for streaming telemetry logs to the server.
 *
 * This service provides seamless integration between Boost.Log and the
 * ORE Studio telemetry persistence infrastructure. It:
 *
 * - Captures log records via a Boost.Log sink backend
 * - Batches records for efficient network transmission
 * - Sends batches to the server using the binary protocol
 * - Handles disconnections gracefully with local buffering
 *
 * Usage:
 * @code
 * auto client = std::make_shared<comms::net::client>(options);
 * client->connect_sync();
 *
 * telemetry_streaming_options opts;
 * opts.source_name = "ores.qt";
 * opts.source_version = "0.0.8";
 *
 * telemetry_streaming_service service(client, opts);
 * service.start();
 *
 * // ... application runs, logs are streamed ...
 *
 * service.stop();  // Flushes remaining logs
 * @endcode
 */
class telemetry_streaming_service final {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.telemetry_streaming";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the telemetry streaming service.
     *
     * @param client Shared pointer to the connected client
     * @param options Configuration options for streaming
     */
    explicit telemetry_streaming_service(
        std::shared_ptr<net::client> client,
        telemetry_streaming_options options);

    /**
     * @brief Destructor. Stops the service if running.
     */
    ~telemetry_streaming_service();

    // Non-copyable, non-movable
    telemetry_streaming_service(const telemetry_streaming_service&) = delete;
    telemetry_streaming_service& operator=(const telemetry_streaming_service&) = delete;
    telemetry_streaming_service(telemetry_streaming_service&&) = delete;
    telemetry_streaming_service& operator=(telemetry_streaming_service&&) = delete;

    /**
     * @brief Starts the streaming service.
     *
     * Registers the Boost.Log sink and starts the background flush thread.
     * Safe to call multiple times; subsequent calls are no-ops.
     */
    void start();

    /**
     * @brief Stops the streaming service.
     *
     * Flushes any pending logs, unregisters the sink, and stops the
     * background thread. Safe to call multiple times; subsequent calls
     * are no-ops.
     */
    void stop();

    /**
     * @brief Checks if the service is currently running.
     */
    [[nodiscard]] bool is_running() const noexcept;

    /**
     * @brief Forces an immediate flush of pending logs.
     *
     * Sends any buffered log records to the server immediately,
     * regardless of batch size or flush interval.
     */
    void flush();

    /**
     * @brief Returns the number of log records pending transmission.
     */
    [[nodiscard]] std::size_t pending_count() const;

    /**
     * @brief Returns the total number of records sent successfully.
     */
    [[nodiscard]] std::uint64_t total_sent() const noexcept;

    /**
     * @brief Returns the total number of records dropped due to overflow.
     */
    [[nodiscard]] std::uint64_t total_dropped() const noexcept;

private:
    /**
     * @brief Handler called by the sink backend for each log record.
     */
    void on_log_record(telemetry::domain::log_record record);

    /**
     * @brief Background thread function that periodically flushes batches.
     */
    void flush_thread_func();

    /**
     * @brief Sends a batch of records to the server.
     *
     * @return true if successful, false if send failed
     */
    bool send_batch(std::vector<telemetry::domain::log_record>& records);

    std::shared_ptr<net::client> client_;
    telemetry_streaming_options options_;
    std::shared_ptr<telemetry::domain::resource> resource_;

    // Sink management
    using sink_t = boost::log::sinks::synchronous_sink<
        telemetry::log::telemetry_sink_backend>;
    boost::shared_ptr<sink_t> sink_;

    // Thread-safe record buffer
    mutable std::mutex buffer_mutex_;
    std::vector<telemetry::domain::log_record> buffer_;
    std::condition_variable flush_cv_;

    // Background flush thread
    std::thread flush_thread_;
    std::atomic<bool> running_{false};

    // Statistics
    std::atomic<std::uint64_t> total_sent_{0};
    std::atomic<std::uint64_t> total_dropped_{0};
};

}

#endif
