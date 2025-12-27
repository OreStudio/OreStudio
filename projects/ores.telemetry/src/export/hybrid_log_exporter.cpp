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
#include "ores.telemetry/export/hybrid_log_exporter.hpp"

#include <fstream>
#include <format>
#include <sstream>
#include <stdexcept>
#include "ores.platform/time/time_utils.hpp"

namespace ores::telemetry::exp {

namespace {

std::string_view severity_to_string(domain::severity_level level) {
    switch (level) {
    case domain::severity_level::trace: return "TRACE";
    case domain::severity_level::debug: return "DEBUG";
    case domain::severity_level::info:  return "INFO";
    case domain::severity_level::warn:  return "WARN";
    case domain::severity_level::error: return "ERROR";
    case domain::severity_level::fatal: return "FATAL";
    default: return "UNKNOWN";
    }
}

std::string format_timestamp(
    const std::chrono::system_clock::time_point& tp) {
    const auto time_t_val = std::chrono::system_clock::to_time_t(tp);
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        tp.time_since_epoch()) % 1000;

    std::tm tm_val{};
    platform::time::time_utils::gmtime_safe(&time_t_val, &tm_val);

    return std::format("{:04d}-{:02d}-{:02d}T{:02d}:{:02d}:{:02d}.{:03d}Z",
        tm_val.tm_year + 1900, tm_val.tm_mon + 1, tm_val.tm_mday,
        tm_val.tm_hour, tm_val.tm_min, tm_val.tm_sec,
        static_cast<int>(ms.count()));
}

std::string escape_json(const std::string& s) {
    std::string result;
    result.reserve(s.size() + 16);

    for (char c : s) {
        switch (c) {
        case '"':  result += "\\\""; break;
        case '\\': result += "\\\\"; break;
        case '\b': result += "\\b"; break;
        case '\f': result += "\\f"; break;
        case '\n': result += "\\n"; break;
        case '\r': result += "\\r"; break;
        case '\t': result += "\\t"; break;
        default:
            if (static_cast<unsigned char>(c) < 0x20) {
                result += std::format("\\u{:04x}", static_cast<unsigned int>(c));
            } else {
                result += c;
            }
        }
    }
    return result;
}

}

hybrid_log_exporter::hybrid_log_exporter(
    const std::filesystem::path& file_path,
    telemetry_options options,
    send_records_callback send_callback)
    : file_(file_path, std::ios::out | std::ios::app)
    , file_path_(file_path)
    , options_(std::move(options))
    , send_callback_(std::move(send_callback))
    , position_tracker_(file_path)
    , last_flush_time_(std::chrono::steady_clock::now()) {

    if (!file_.is_open()) {
        throw std::runtime_error("Failed to open log file: " + file_path.string());
    }

    // Get current file position for tracking
    file_position_ = static_cast<std::uint64_t>(file_.tellp());

    // Reserve space for batch
    pending_batch_.reserve(options_.batch_size);

    // Start background flush thread if streaming is enabled
    if (options_.streaming_enabled && send_callback_) {
        flush_thread_ = std::thread(&hybrid_log_exporter::flush_thread_func, this);
    }
}

hybrid_log_exporter::~hybrid_log_exporter() {
    shutdown();
}

void hybrid_log_exporter::export_record(domain::log_record record) {
    // Always write to file first (for durability)
    write_to_file(record);

    // Add to batch if streaming is enabled
    if (options_.streaming_enabled && send_callback_) {
        add_to_batch(std::move(record));
    }
}

void hybrid_log_exporter::flush() {
    // Flush file
    {
        std::lock_guard<std::mutex> lock(file_mutex_);
        file_.flush();
    }

    // Send pending batch if streaming
    if (options_.streaming_enabled && connected_) {
        send_batch();
    }
}

void hybrid_log_exporter::shutdown() {
    // Signal shutdown
    shutdown_requested_ = true;
    flush_cv_.notify_all();

    // Wait for flush thread
    if (flush_thread_.joinable()) {
        flush_thread_.join();
    }

    // Final flush
    flush();

    // Close file
    {
        std::lock_guard<std::mutex> lock(file_mutex_);
        if (file_.is_open()) {
            file_.close();
        }
    }
}

void hybrid_log_exporter::set_connected(bool connected) {
    bool was_connected = connected_.exchange(connected);

    // If just became connected, wake up flush thread to process backlog
    if (!was_connected && connected) {
        flush_cv_.notify_all();
    }
}

std::uint64_t hybrid_log_exporter::upload_position() const {
    return position_tracker_.get_position();
}

void hybrid_log_exporter::write_to_file(const domain::log_record& record) {
    std::lock_guard<std::mutex> lock(file_mutex_);

    // Build JSON object
    std::ostringstream ss;
    ss << "{";
    ss << "\"timestamp\":\"" << format_timestamp(record.timestamp) << "\"";
    ss << ",\"severity\":\"" << severity_to_string(record.severity) << "\"";
    ss << ",\"body\":\"" << escape_json(record.body) << "\"";

    if (!record.logger_name.empty()) {
        ss << ",\"logger\":\"" << escape_json(record.logger_name) << "\"";
    }

    if (record.trace) {
        ss << ",\"trace_id\":\"" << record.trace->to_hex() << "\"";
    }

    if (record.span) {
        ss << ",\"span_id\":\"" << record.span->to_hex() << "\"";
    }

    if (record.source_resource) {
        auto service_name = record.source_resource->service_name();
        if (service_name) {
            ss << ",\"service\":\"" << escape_json(*service_name) << "\"";
        }
    }

    ss << "}\n";

    std::string line = ss.str();
    file_ << line;
    file_position_ += line.size();
}

void hybrid_log_exporter::add_to_batch(domain::log_record record) {
    std::vector<domain::log_record> batch_to_send;

    {
        std::lock_guard<std::mutex> lock(batch_mutex_);
        pending_batch_.push_back(std::move(record));

        // Check if batch is full and we're connected
        if (pending_batch_.size() >= options_.batch_size && connected_) {
            std::swap(batch_to_send, pending_batch_);
            pending_batch_.reserve(options_.batch_size);
        }
    }

    // Send outside the lock to avoid blocking other threads
    if (!batch_to_send.empty() && send_callback_) {
        if (send_callback_(std::move(batch_to_send))) {
            position_tracker_.set_position(file_position_);
            last_flush_time_ = std::chrono::steady_clock::now();
        }
    }
}

bool hybrid_log_exporter::send_batch() {
    std::vector<domain::log_record> batch;

    {
        std::lock_guard<std::mutex> lock(batch_mutex_);
        if (pending_batch_.empty()) {
            return true;
        }

        std::swap(batch, pending_batch_);
        pending_batch_.reserve(options_.batch_size);
    }

    if (send_callback_ && send_callback_(std::move(batch))) {
        position_tracker_.set_position(file_position_);
        last_flush_time_ = std::chrono::steady_clock::now();
        return true;
    }

    return false;
}

void hybrid_log_exporter::flush_thread_func() {
    while (!shutdown_requested_) {
        std::unique_lock<std::mutex> lock(batch_mutex_);

        // Wait for flush_interval or shutdown
        flush_cv_.wait_for(lock, options_.flush_interval, [this]() {
            return shutdown_requested_.load() || connected_.load();
        });

        if (shutdown_requested_) {
            break;
        }

        // Check if enough time has passed since last flush
        auto now = std::chrono::steady_clock::now();
        if (now - last_flush_time_ >= options_.flush_interval) {
            if (connected_ && !pending_batch_.empty()) {
                lock.unlock();
                send_batch();
            }
        }
    }
}

}
