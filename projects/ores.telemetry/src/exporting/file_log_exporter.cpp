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
#include "ores.telemetry/exporting/file_log_exporter.hpp"

#include <format>
#include <chrono>
#include <stdexcept>
#include "ores.platform/time/time_utils.hpp"

namespace ores::telemetry::exporting {

namespace {

/**
 * @brief Converts severity level to string.
 */
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

/**
 * @brief Formats a time_point as ISO 8601 string.
 */
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

/**
 * @brief Escapes a string for JSON output.
 */
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

file_log_exporter::file_log_exporter(const std::filesystem::path& path)
    : file_(path, std::ios::out | std::ios::app)
    , is_open_(true) {
    if (!file_.is_open()) {
        throw std::runtime_error("Failed to open log file: " + path.string());
    }
}

file_log_exporter::~file_log_exporter() {
    shutdown();
}

void file_log_exporter::export_record(domain::log_record record) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (!is_open_) {
        return;
    }

    // Build JSON object
    file_ << "{";
    file_ << "\"timestamp\":\"" << format_timestamp(record.timestamp) << "\"";
    file_ << ",\"severity\":\"" << severity_to_string(record.severity) << "\"";
    file_ << ",\"body\":\"" << escape_json(record.body) << "\"";

    if (!record.logger_name.empty()) {
        file_ << ",\"logger\":\"" << escape_json(record.logger_name) << "\"";
    }

    if (record.trace) {
        file_ << ",\"trace_id\":\"" << record.trace->to_hex() << "\"";
    }

    if (record.span) {
        file_ << ",\"span_id\":\"" << record.span->to_hex() << "\"";
    }

    if (record.source_resource) {
        auto service_name = record.source_resource->service_name();
        if (service_name) {
            file_ << ",\"service\":\"" << escape_json(*service_name) << "\"";
        }
    }

    file_ << "}\n";
}

void file_log_exporter::flush() {
    std::lock_guard<std::mutex> lock(mutex_);
    if (is_open_) {
        file_.flush();
    }
}

void file_log_exporter::shutdown() {
    std::lock_guard<std::mutex> lock(mutex_);
    if (is_open_) {
        file_.flush();
        file_.close();
        is_open_ = false;
    }
}

}
