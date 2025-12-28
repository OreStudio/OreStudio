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
#ifndef ORES_TELEMETRY_EXPORT_HYBRID_LOG_EXPORTER_HPP
#define ORES_TELEMETRY_EXPORT_HYBRID_LOG_EXPORTER_HPP

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>
#include "ores.telemetry/export/log_exporter.hpp"
#include "ores.telemetry/export/telemetry_options.hpp"
#include "ores.telemetry/export/upload_position_tracker.hpp"

namespace ores::telemetry::exp {

/**
 * @brief Callback type for sending batched records to the server.
 *
 * The callback receives a vector of log records to send. It should return
 * true if the records were successfully queued for sending (fire-and-forget),
 * or false if the connection is not available.
 *
 * The callback is expected to be non-blocking. The actual send may happen
 * asynchronously.
 */
using send_records_callback =
    std::function<bool(std::vector<domain::log_record>)>;

/**
 * @brief Hybrid exporter that writes to file and optionally streams to server.
 *
 * This exporter implements a dual-export strategy:
 * 1. All records are always written to a local JSON Lines file
 * 2. When streaming is enabled and the connection is available, records
 *    are batched and sent to the server
 *
 * The exporter maintains durability through:
 * - Immediate file writes (no buffering for file export)
 * - Position tracking for resuming uploads after reconnect
 * - Batching with configurable batch_size and flush_interval
 *
 * Thread-safety: All public methods are thread-safe.
 */
class hybrid_log_exporter final : public log_exporter {
public:
    /**
     * @brief Constructs a hybrid log exporter.
     *
     * @param file_path Path to the JSON Lines output file.
     * @param options Telemetry configuration options.
     * @param send_callback Callback for sending records to the server.
     *                      May be nullptr if streaming is disabled.
     */
    hybrid_log_exporter(const std::filesystem::path& file_path,
                        telemetry_options options,
                        send_records_callback send_callback = nullptr);

    ~hybrid_log_exporter() override;

    hybrid_log_exporter(const hybrid_log_exporter&) = delete;
    hybrid_log_exporter& operator=(const hybrid_log_exporter&) = delete;

    /**
     * @brief Exports a log record.
     *
     * The record is immediately written to the file. If streaming is enabled,
     * it is also added to the pending batch. The batch is sent when it reaches
     * batch_size or when flush_interval has elapsed.
     *
     * @param record The log record to export.
     */
    void export_record(domain::log_record record) override;

    /**
     * @brief Flushes pending records.
     *
     * Forces immediate file flush and sends any pending batch to the server
     * (if streaming is enabled and connected).
     */
    void flush() override;

    /**
     * @brief Shuts down the exporter.
     *
     * Flushes pending records and stops the background flush thread.
     */
    void shutdown() override;

    /**
     * @brief Updates the connection status.
     *
     * When connection becomes available after being disconnected, this
     * triggers upload of any records that accumulated during the disconnect.
     *
     * @param connected true if connection is available.
     */
    void set_connected(bool connected);

    /**
     * @brief Returns the current upload position.
     */
    std::uint64_t upload_position() const;

private:
    /**
     * @brief Writes a record to the JSON Lines file.
     */
    void write_to_file(const domain::log_record& record);

    /**
     * @brief Adds a record to the pending batch.
     */
    void add_to_batch(domain::log_record record);

    /**
     * @brief Sends the pending batch to the server.
     *
     * @return true if batch was sent (or was empty), false if send failed.
     */
    bool send_batch();

    /**
     * @brief Background thread function for flush interval.
     */
    void flush_thread_func();

    // File export
    std::mutex file_mutex_;
    std::ofstream file_;
    std::filesystem::path file_path_;
    std::uint64_t file_position_ = 0;

    // Options
    telemetry_options options_;
    send_records_callback send_callback_;

    // Streaming state
    std::mutex batch_mutex_;
    std::vector<domain::log_record> pending_batch_;
    upload_position_tracker position_tracker_;
    std::atomic<bool> connected_{false};

    // Background flush thread
    std::thread flush_thread_;
    std::condition_variable flush_cv_;
    std::atomic<bool> shutdown_requested_{false};
    std::chrono::steady_clock::time_point last_flush_time_;
};

}

#endif
