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
#include "ores.reporting.core/service/engine_error_log.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"
#include <boost/json.hpp>
#include <algorithm>
#include <filesystem>
#include <format>
#include <fstream>
#include <span>
#include <stdexcept>

namespace ores::reporting::service {

namespace {

namespace fs = std::filesystem;
namespace json = boost::json;

/// The name the engine gives its own account of the errors of a run.
constexpr std::string_view structured_log_entry = "log_structured.json";
constexpr std::string_view text_log_entry = "log.txt";

/// The category the engine writes for an error it continued past.
constexpr std::string_view category_warning = "Warning";

/// The sub-fields that describe the failure itself.
constexpr std::string_view diagnostic_fields[] = {"exceptionType", "warningType"};

bool is_named(std::string_view name, const std::span<const std::string_view> names) {
    return std::ranges::find(names, name) != names.end();
}

std::string read_field(const json::object& obj, std::string_view name) {
    const auto* value = obj.if_contains(name);
    if (value == nullptr || !value->is_string())
        return {};
    return std::string(value->as_string());
}

/**
 * @brief The subject an error of @p scope names, from the sub-fields it carries.
 *
 * The sub-field decides, and not the group, because the engine states the
 * subject once per group today except for Analytics, which names a trade when
 * the failure was that trade's. A sub-field that identifies a narrower subject
 * is therefore read before the group's own, so an Analytics error that names a
 * trade is placed on the trade.
 *
 * @return The sub-field the subject was read from, which is no longer a detail.
 */
std::string
read_subject(const std::string& scope, const json::object& sub_fields, engine_log_error& error) {
    const auto set = [&](std::string subject, std::string_view field) {
        error.subject = std::move(subject);
        error.subject_id = read_field(sub_fields, field);
        return std::string(field);
    };

    if (sub_fields.if_contains("tradeId") != nullptr)
        return set("trade", "tradeId");
    if (scope == "Analytics")
        return set("analytic", "analyticType");
    if (scope == "Curve")
        return set("curve", "curveId");
    if (scope == "Configuration")
        return set("configuration", "configurationType");
    return {};
}

}

namespace {

/**
 * @brief The curve name inside an engine reference such as
 * =[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]=.
 *
 * The engine states a curve as the index it is built from and the curve it
 * names, and a person reads the second. A reference that does not have that
 * shape is returned as it stands, because a name the engine chose is better
 * than no name at all.
 */
std::string curve_name_of(std::string_view reference) {
    const auto marker = reference.find("IndexCurve(");
    if (marker == std::string_view::npos)
        return std::string(reference);
    const auto open = marker + std::string_view("IndexCurve(").size();
    const auto close = reference.find(')', open);
    if (close == std::string_view::npos)
        return std::string(reference);
    const auto inner = reference.substr(open, close - open);
    const auto comma = inner.find(',');
    if (comma == std::string_view::npos)
        return std::string(inner);
    return std::string(inner.substr(comma + 1));
}

/**
 * @brief What the error is about, from the group and the fields it carries.
 *
 * Only a trade's curve has to be read from the message: the engine names the
 * trade's curve nowhere else. Reading it here is the one place that knows the
 * engine's words, so no client has to.
 */
std::string cause_of(const engine_log_error& error, const json::object& sub_fields) {
    if (error.scope == "Curve")
        return curve_name_of(error.subject_id);
    if (error.scope == "Trade") {
        const auto marker = error.message.find("IndexCurve(");
        if (marker != std::string::npos) {
            const auto close = error.message.find(')', marker);
            if (close != std::string::npos)
                return curve_name_of(
                    std::string_view(error.message).substr(marker, close - marker + 1));
        }
        return {};
    }
    if (error.scope == "Analytics") {
        if (error.subject == "analytic")
            return error.subject_id;
        const auto* analytic = sub_fields.if_contains("analyticType");
        return analytic != nullptr && analytic->is_string() ? std::string(analytic->as_string()) :
                                                              std::string{};
    }
    if (error.scope == "Configuration")
        return error.subject_id;
    return {};
}

}

std::optional<engine_log_error> parse_engine_log_line(std::string_view line) {
    if (line.find('{') == std::string_view::npos)
        return std::nullopt;

    boost::system::error_code ec;
    const auto value = json::parse(line, ec);
    if (ec || !value.is_object())
        return std::nullopt;
    const auto& obj = value.as_object();

    const auto* group = obj.if_contains("group");
    const auto* message = obj.if_contains("message");
    if (group == nullptr || !group->is_string() || message == nullptr || !message->is_string())
        return std::nullopt;

    engine_log_error error;
    error.scope = std::string(group->as_string());
    error.message = std::string(message->as_string());
    error.severity = read_field(obj, "category") == category_warning ? "warning" : "error";

    json::object sub_fields;
    const auto* fields = obj.if_contains("sub_fields");
    if (fields != nullptr && fields->is_array()) {
        for (const auto& one : fields->as_array()) {
            if (!one.is_object())
                continue;
            const auto& field = one.as_object();
            const auto name = read_field(field, "name");
            if (name.empty())
                continue;
            sub_fields[name] = read_field(field, "value");
        }
    }

    // The detail is what the engine said about the failure that is not a
    // subject: the exception it named, and the warning it wrote for a run it
    // continued past. Read by name rather than by exclusion, so a sub-field
    // the engine adds later lands in neither the subject nor the detail until
    // this reader has been told what it means, rather than being stored under
    // a meaning nobody has stated.
    read_subject(error.scope, sub_fields, error);
    json::object detail;
    for (const auto& [name, value] : sub_fields) {
        const std::string key(name);
        if (!is_named(key, diagnostic_fields))
            continue;
        detail[key] = value;
    }
    if (!detail.empty())
        error.detail = json::serialize(detail);

    error.cause = cause_of(error, sub_fields);

    return error;
}


engine_log_read read_text_log_errors(const std::filesystem::path& path);

/**
 * @brief Reads the errors an engine wrote into its own text log.
 *
 * The engine's text log carries the structured message as JSON after a marker,
 * on one line:
 *
 * @code
 * ALERT [2026-Oct-27 19:42:07.139446] (ored/utilities/log.cpp:214) :
 *   StructuredMessage { "category": "Error", "group": "Curve", ... }
 * @endcode
 *
 * The JSON is the same the structured log holds, so it is handed to the same
 * parser and the two files cannot disagree about what an error means.
 */
engine_log_read read_text_log_errors(const std::filesystem::path& path) {
    engine_log_read read;
    std::ifstream stream(path, std::ios::binary);
    if (!stream) {
        read.error = std::format("The run's {} could not be opened.", text_log_entry);
        return read;
    }

    constexpr std::string_view marker = "StructuredMessage";
    std::string line;
    while (std::getline(stream, line)) {
        const auto at = line.find(marker);
        if (at == std::string::npos)
            continue;
        const auto start = line.find('{', at);
        if (start == std::string::npos)
            continue;
        if (auto error = parse_engine_log_line(std::string_view(line).substr(start)))
            read.errors.push_back(std::move(*error));
    }
    return read;
}

engine_log_read read_engine_errors_from_archive(std::span<const std::byte> archive_bytes) {
    engine_log_read read;

    const ores::platform::filesystem::scoped_temp_file archive;
    const ores::platform::filesystem::scoped_temp_directory output;
    try {
        {
            std::ofstream stream(archive.path(), std::ios::binary);
            if (!stream)
                throw std::runtime_error("Cannot write the run's output archive to " +
                                         archive.path().string());
            stream.write(reinterpret_cast<const char*>(archive_bytes.data()),
                         static_cast<std::streamsize>(archive_bytes.size()));
        }

        ores::storage::filesystem::archiver::extract(archive.path(), output.path());
    } catch (const std::exception& e) {
        read.error = std::format("The run's output archive could not be read: {}", e.what());
        return read;
    }

    // The engine writes its structured messages into log_structured.json when it
    // is asked for that file, and into its text log when it is not: the same
    // JSON, on a line that starts with the engine's own "StructuredMessage"
    // marker. The text log is what an engine built without the structured file
    // leaves behind, so a run is read from whichever of the two it wrote rather
    // than losing its errors to a setting.
    const auto log = output.path() / structured_log_entry;
    std::error_code ec;
    if (!fs::exists(log, ec)) {
        const auto text_log = output.path() / text_log_entry;
        if (!fs::exists(text_log, ec)) {
            read.error = std::format("The run's output holds neither {} nor {}, so its errors "
                                     "were not recorded by the engine.",
                                     structured_log_entry,
                                     text_log_entry);
            return read;
        }
        return read_text_log_errors(text_log);
    }

    std::ifstream stream(log, std::ios::binary);
    if (!stream) {
        read.error = std::format("The run's {} could not be opened.", structured_log_entry);
        return read;
    }

    std::string line;
    while (std::getline(stream, line)) {
        if (line.find_first_not_of(" \t\r\n") == std::string::npos)
            continue;
        if (auto error = parse_engine_log_line(line))
            read.errors.push_back(std::move(*error));
    }

    return read;
}

}
