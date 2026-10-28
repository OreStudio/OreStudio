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
#include "ores.ore.core/log/engine_log.hpp"

#include <boost/json.hpp>
#include <sstream>
#include <string>
#include <vector>

namespace ores::ore::log {
namespace {

namespace json = boost::json;

std::string word_at_start(std::string_view text) {
    const auto first = text.find_first_not_of(" \t");
    if (first == std::string_view::npos)
        return {};
    const auto end = text.find_first_of(" \t", first);
    return std::string(text.substr(first, end == std::string_view::npos ? std::string_view::npos :
                                                                         end - first));
}

std::string string_field(const json::object& obj, std::string_view name) {
    const auto* value = obj.if_contains(name);
    if (value == nullptr || !value->is_string())
        return {};
    return std::string(value->as_string());
}

std::string sub_field(const json::object& obj, std::string_view name) {
    const auto* fields = obj.if_contains("sub_fields");
    if (fields == nullptr || !fields->is_array())
        return {};
    for (const auto& one : fields->as_array()) {
        if (!one.is_object())
            continue;
        const auto& field = one.as_object();
        if (string_field(field, "name") == name)
            return string_field(field, "value");
    }
    return {};
}

std::string severity_of_category(std::string_view category) {
    if (category == "Warning")
        return "warning";
    if (category == "Error" || category == "Alert")
        return "error";
    if (category == "Info")
        return "info";
    return "notice";
}

std::string severity_of_word(std::string_view word) {
    if (word == "WARNING" || word == "WARN")
        return "warning";
    if (word == "ERROR" || word == "ALERT" || word == "FATAL")
        return "error";
    if (word == "DEBUG")
        return "debug";
    if (word == "INFO")
        return "info";
    return "notice";
}

std::string time_between_brackets(std::string_view line) {
    const auto open = line.find('[');
    if (open == std::string_view::npos)
        return {};
    const auto close = line.find(']', open);
    if (close == std::string_view::npos)
        return {};
    return std::string(line.substr(open + 1, close - open - 1));
}

/**
 * @brief The message after the engine's own separator, which is what the line says.
 *
 * The engine writes the file that logged the line between parentheses and then
 * the separator, so the message is what follows the closed parenthesis. A line
 * that names no file falls back to the first separator, and a line with neither
 * is all message. The message itself may hold a separator -- =group = setup :
 * nThreads = 1= is one message -- so the first separator is only a fallback.
 */
std::string message_after_separator(std::string_view line) {
    const auto file = line.find('(');
    if (file != std::string_view::npos) {
        const auto close = line.find(')', file);
        if (close != std::string_view::npos) {
            const auto after = line.find_first_not_of(' ', close + 1);
            if (after != std::string_view::npos) {
                if (line.substr(after, 1) == ":") {
                    const auto message = line.find_first_not_of(' ', after + 1);
                    return message == std::string_view::npos ? std::string() :
                                                               std::string(line.substr(message));
                }
            }
        }
    }
    const auto separator = line.find(" : ");
    if (separator == std::string_view::npos)
        return std::string(line);
    return std::string(line.substr(separator + 3));
}

/** Applies one structured message, whose JSON starts at @p from, onto @p event. */
bool apply_structured(std::string_view payload, engine_log_event& event) {
    const auto open = payload.find('{');
    if (open == std::string_view::npos)
        return false;
    boost::system::error_code ec;
    const auto value = json::parse(payload.substr(open), ec);
    if (ec || !value.is_object())
        return false;

    const auto& obj = value.as_object();
    event.severity = severity_of_category(string_field(obj, "category"));
    event.scope = string_field(obj, "group");
    event.message = string_field(obj, "message");
    event.cause = sub_field(obj, "curveId");
    event.subject_id = sub_field(obj, "tradeId");
    if (!event.subject_id.empty())
        event.subject = "trade";

    // The fields that are not the subject are the account of why: the exception
    // the engine raised and the analytic it was in. They are kept by name.
    json::object detail;
    if (const auto* fields = obj.if_contains("sub_fields"); fields != nullptr &&
                                                          fields->is_array()) {
        for (const auto& one : fields->as_array()) {
            if (!one.is_object())
                continue;
            const auto& field = one.as_object();
            const auto name = string_field(field, "name");
            if (name.empty() || name == "curveId" || name == "tradeId")
                continue;
            detail[name] = string_field(field, "value");
        }
    }
    if (!detail.empty())
        event.detail = json::serialize(detail);
    return true;
}

constexpr std::string_view structured_marker = "StructuredMessage";

}

std::string canonical_log_csv(const std::vector<engine_log_event>& events) {
    const auto quoted = [](std::string_view field) {
        if (field.find_first_of(",\"\n") == std::string_view::npos)
            return std::string(field);
        std::string out{"\""};
        for (const auto c : field) {
            if (c == '"')
                out.push_back('"');
            out.push_back(c);
        }
        out.push_back('"');
        return out;
    };

    std::ostringstream out;
    out << "ordinal,severity,scope,message,subject,subject_id,cause,event_time,source\n";
    for (const auto& event : events) {
        out << event.ordinal << ',' << quoted(event.severity) << ',' << quoted(event.scope) << ','
            << quoted(event.message) << ',' << quoted(event.subject) << ','
            << quoted(event.subject_id) << ',' << quoted(event.cause) << ','
            << quoted(event.event_time) << ',' << quoted(event.source) << '\n';
    }
    return out.str();
}

std::vector<engine_log_event> parse_engine_log_text(std::string_view text,
                                                   std::string_view source_file) {
    if (text.substr(0, 3) == "\xEF\xBB\xBF")
        text.remove_prefix(3);

    std::vector<engine_log_event> events;
    std::size_t ordinal = 0;
    while (!text.empty()) {
        const auto break_at = text.find('\n');
        auto line = text.substr(0, break_at);
        text = break_at == std::string_view::npos ? std::string_view() : text.substr(break_at + 1);
        if (line.find_first_not_of(" \t\r") == std::string_view::npos)
            continue;
        // A log written on a host that ends its lines with a carriage return is
        // still one line per event, and the carriage return is not part of what
        // the engine said.
        if (!line.empty() && line.back() == '\r')
            line.remove_suffix(1);

        engine_log_event event;
        event.ordinal = ++ordinal;
        event.event_time = time_between_brackets(line);
        event.source = std::string(source_file);
        event.severity = severity_of_word(word_at_start(line));

        const auto payload = message_after_separator(line);
        if (payload.starts_with(structured_marker))
            apply_structured(payload, event);
        else
            event.message = payload;

        if (event.message.empty())
            event.message = payload;
        events.push_back(std::move(event));
    }
    return events;
}

std::vector<engine_log_event> parse_engine_log_json(std::string_view text,
                                                    std::string_view source_file) {
    if (text.substr(0, 3) == "\xEF\xBB\xBF")
        text.remove_prefix(3);

    std::vector<engine_log_event> events;
    std::size_t ordinal = 0;
    while (!text.empty()) {
        const auto break_at = text.find('\n');
        auto line = text.substr(0, break_at);
        text = break_at == std::string_view::npos ? std::string_view() : text.substr(break_at + 1);
        if (line.find_first_not_of(" \t\r") == std::string_view::npos)
            continue;
        // A log written on a host that ends its lines with a carriage return is
        // still one line per event, and the carriage return is not part of what
        // the engine said.
        if (!line.empty() && line.back() == '\r')
            line.remove_suffix(1);

        engine_log_event event;
        event.ordinal = ++ordinal;
        event.source = std::string(source_file);
        if (!apply_structured(line, event)) {
            event.severity = "notice";
            event.message = std::string(line);
        }
        events.push_back(std::move(event));
    }
    return events;
}

}
