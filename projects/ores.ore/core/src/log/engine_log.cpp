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
#include "ores.platform/time/datetime.hpp"
#include <boost/json.hpp>
#include <array>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <sstream>
#include <stdexcept>
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
    return std::string(
        text.substr(first, end == std::string_view::npos ? std::string_view::npos : end - first));
}

std::string string_field(const json::object& obj, std::string_view name) {
    const auto* value = obj.if_contains(name);
    if (value == nullptr || !value->is_string())
        return {};
    return std::string(value->as_string());
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

/** The severity a level word means, in the store's vocabulary. */
std::string severity_of_word(std::string_view word) {
    if (word == "WARNING" || word == "WARN")
        return "warning";
    if (word == "ERROR" || word == "ALERT" || word == "FATAL")
        return "error";
    if (word == "DEBUG" || word == "DATA")
        return "debug";
    if (word == "INFO")
        return "info";
    return "notice";
}

/**
 * @brief Whether the first word of a line is one the engine puts on a line.
 *
 * A line whose first word is not one of these did not come from the engine's
 * logger, so a client that forwards the engine's lines to a log store has
 * nothing to forward.
 */
bool is_engine_level(std::string_view word) {
    return word == "DATA" || word == "NOTICE" || word == "WARNING" || word == "WARN" ||
           word == "ERROR" || word == "ALERT" || word == "DEBUG" || word == "INFO" ||
           word == "FATAL";
}

/** The text the engine wrote in parentheses, or empty when it wrote none. */
std::string location_in_parentheses(std::string_view line) {
    const auto open = line.find('(');
    if (open == std::string_view::npos)
        return {};
    const auto close = line.find(')', open);
    if (close == std::string_view::npos)
        return {};
    return std::string(line.substr(open + 1, close - open - 1));
}

/**
 * @brief The time the engine stated in brackets, as a value in UTC.
 *
 * The engine writes =YYYY-Mon-DD HH:MM:SS= and a fractional second of up to six
 * digits, which it does not always write in full: =123= means three hundred and
 * twenty-three milliseconds, so the digits are scaled rather than read as one
 * number of microseconds.
 */
std::optional<std::chrono::system_clock::time_point> time_as_value(std::string_view stated) {
    if (stated.empty())
        return std::nullopt;

    const auto dot = stated.rfind('.');
    const auto date_time = dot == std::string_view::npos ? stated : stated.substr(0, dot);
    long microseconds = 0;
    if (dot != std::string_view::npos) {
        const auto fraction = stated.substr(dot + 1);
        std::array<char, 7> scaled = {'0', '0', '0', '0', '0', '0', '\0'};
        const auto digits = std::min(fraction.size(), std::size_t{6});
        std::memcpy(scaled.data(), fraction.data(), digits);
        microseconds = std::strtol(scaled.data(), nullptr, 10);
    }

    try {
        return ores::platform::time::datetime::from_utc_format(std::string(date_time),
                                                               engine_timestamp_format.data()) +
               std::chrono::microseconds(microseconds);
    } catch (const std::invalid_argument&) {
        return std::nullopt;
    }
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
 * @brief What a line states after the engine's separator, and whether it had one.
 */
struct stated_message {
    std::string text;
    /**
     * @brief Whether the line carried the separator the engine writes between
     * the location that logged it and what it said.
     */
    bool after_separator = false;
};

/**
 * @brief The message after the engine's own separator, which is what the line says.
 *
 * The engine writes the file that logged the line between parentheses and then
 * the separator, so the message is what follows the closed parenthesis. A line
 * that names no file falls back to the first separator, and a line with neither
 * is all message and states no separator. The message itself may hold a
 * separator -- =group = setup : nThreads = 1= is one message -- so the first
 * separator is only a fallback.
 *
 * A line that names no file but whose message holds a parenthesis is read as if
 * the parenthesis named the file, and the message is cut at the separator after
 * it. The engine states its file on every line it writes, so this is a limit of
 * the shape rather than of a line it writes today.
 */
stated_message message_after_separator(std::string_view line) {
    const auto file = line.find('(');
    if (file != std::string_view::npos) {
        const auto close = line.find(')', file);
        if (close != std::string_view::npos) {
            const auto after = line.find_first_not_of(' ', close + 1);
            if (after != std::string_view::npos) {
                if (line.substr(after, 1) == ":") {
                    const auto message = line.find_first_not_of(' ', after + 1);
                    return {message == std::string_view::npos ? std::string() :
                                                                std::string(line.substr(message)),
                            true};
                }
            }
        }
    }
    const auto separator = line.find(" : ");
    if (separator == std::string_view::npos)
        return {std::string(line), false};
    return {std::string(line.substr(separator + 3)), true};
}

std::string curve_name_of(std::string_view reference);
std::string value_of(const json::object& fields, std::string_view name);
json::object subject_of(const json::object& fields, std::string_view scope);
std::string cause_of(std::string_view scope,
                     std::string_view message,
                     const json::object& fields,
                     std::string_view subject_id);
std::string detail_of(const json::object& fields);

/**
 * @brief Applies one structured message, whose JSON starts at @p payload, onto @p event.
 *
 * @return Whether the payload held a JSON object, which decides whether the line
 * keeps its own text as the message.
 */
bool apply_structured(std::string_view payload, engine_log_event& event) {
    const auto open = payload.find('{');
    if (open == std::string_view::npos)
        return false;
    boost::system::error_code ec;
    const auto value = json::parse(payload.substr(open), ec);
    if (ec || !value.is_object())
        return false;

    const auto& obj = value.as_object();
    const auto category = string_field(obj, "category");
    if (!category.empty())
        event.severity = severity_of_category(category);
    event.scope = string_field(obj, "group");
    event.message = string_field(obj, "message");

    json::object fields;
    if (const auto* stated = obj.if_contains("sub_fields");
        stated != nullptr && stated->is_array()) {
        for (const auto& one : stated->as_array()) {
            if (!one.is_object())
                continue;
            const auto& field = one.as_object();
            const auto name = string_field(field, "name");
            if (name.empty())
                continue;
            fields[name] = string_field(field, "value");
        }
    }
    event.fields = fields.empty() ? std::string() : json::serialize(fields);

    const auto subject = subject_of(fields, event.scope);
    event.subject = value_of(subject, "subject");
    event.subject_id = value_of(subject, "id");
    event.cause = cause_of(event.scope, event.message, fields, event.subject_id);
    event.detail = detail_of(fields);
    return true;
}


/**
 * @brief The curve name inside an engine reference such as
 * =[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]=.
 *
 * The engine states a curve as the index it is built from and the curve it
 * names, and a person reads the second. A reference of another shape is returned
 * as it stands, because a name the engine chose is better than no name at all.
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

/** The value of a named field in the flat object of sub-fields. */
std::string value_of(const json::object& fields, std::string_view name) {
    const auto* value = fields.if_contains(name);
    if (value == nullptr || !value->is_string())
        return {};
    return std::string(value->as_string());
}

/**
 * @brief Where a structured message places the record it is about.
 *
 * The sub-field decides, and not the group, because the engine states the subject
 * once per group today except for Analytics, which names a trade when the failure
 * was that trade's. A field that identifies a narrower subject is therefore read
 * first, so an Analytics message that names a trade is placed on the trade.
 *
 * @return The object said to be the subject, empty when the message names none.
 */
json::object subject_of(const json::object& fields, std::string_view scope) {
    json::object subject;
    const auto put = [&subject](std::string_view kind, std::string value) {
        subject["subject"] = std::string(kind);
        subject["id"] = std::move(value);
    };
    if (const auto trade = value_of(fields, "tradeId"); !trade.empty())
        put("trade", trade);
    else if (scope == "Analytics")
        put("analytic", value_of(fields, "analyticType"));
    else if (scope == "Curve")
        put("curve", value_of(fields, "curveId"));
    else if (scope == "Configuration")
        put("configuration", value_of(fields, "configurationType"));
    return subject;
}

/**
 * @brief What caused the message, from the group and the fields it carries.
 *
 * Only a trade's curve has to be read from the message: the engine names the
 * trade's curve nowhere else. Reading it here is the one place that knows the
 * engine's words, which is what spares every client the lesson.
 */
std::string cause_of(std::string_view scope,
                     std::string_view message,
                     const json::object& fields,
                     std::string_view subject_id) {
    if (scope == "Curve")
        return curve_name_of(value_of(fields, "curveId"));
    if (scope == "Trade") {
        const auto marker = message.find("IndexCurve(");
        if (marker != std::string::npos) {
            const auto close = message.find(')', marker);
            if (close != std::string::npos)
                return curve_name_of(message.substr(marker, close - marker + 1));
        }
        return {};
    }
    if (scope == "Analytics")
        return value_of(fields, "analyticType");
    if (scope == "Configuration")
        return std::string(subject_id);
    return {};
}

/** The fields that describe the failure itself, never the subject. */
std::string detail_of(const json::object& fields) {
    json::object detail;
    for (const auto& name : {"exceptionType", "warningType"}) {
        const auto value = value_of(fields, name);
        if (!value.empty())
            detail[name] = value;
    }
    return detail.empty() ? std::string() : json::serialize(detail);
}

constexpr std::string_view structured_marker = "StructuredMessage";

/**
 * @brief Calls @p visit once for each line of the log that states anything.
 *
 * Both of the engine's logs are one record per line, and both carry a byte order
 * mark on a host that writes one and a carriage return before each newline on a
 * host that does. The rules for finding a line, skipping a blank one and taking
 * the carriage return off are the same for both, so they live here once.
 *
 * @param visit Called with the line and its place in the log, from one.
 */
template <typename Visit>
void for_each_line(std::string_view text, Visit&& visit) {
    if (text.substr(0, 3) == "\xEF\xBB\xBF")
        text.remove_prefix(3);

    std::size_t ordinal = 0;
    while (!text.empty()) {
        const auto break_at = text.find('\n');
        auto line = text.substr(0, break_at);
        text = break_at == std::string_view::npos ? std::string_view() : text.substr(break_at + 1);
        if (line.find_first_not_of(" \t\r") == std::string_view::npos)
            continue;
        if (!line.empty() && line.back() == '\r')
            line.remove_suffix(1);
        visit(line, ++ordinal);
    }
}

}

std::string field_of(const engine_log_event& event, std::string_view name) {
    if (event.fields.empty())
        return {};
    boost::system::error_code ec;
    const auto value = json::parse(event.fields, ec);
    if (ec || !value.is_object())
        return {};
    return string_field(value.as_object(), name);
}

bool is_memory_report(const engine_log_event& event) {
    return event.severity == "warning" && event.message.ends_with(engine_memory_report_suffix);
}

std::string canonical_log_csv(const std::vector<engine_log_event>& events) {
    const auto quoted = [](std::string_view field) {
        if (field.find_first_of(",\"\n\r") == std::string_view::npos)
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
    std::vector<engine_log_event> events;
    for_each_line(text, [&](std::string_view line, std::size_t ordinal) {
        engine_log_event event;
        event.ordinal = ordinal;
        event.level_word = word_at_start(line);
        event.severity = severity_of_word(event.level_word);
        event.event_time = time_between_brackets(line);
        event.occurred_at = time_as_value(event.event_time);
        event.location = location_in_parentheses(line);
        event.source = std::string(source_file);

        const auto payload = message_after_separator(line);
        event.engine_format = is_engine_level(event.level_word) && event.occurred_at &&
                              !event.location.empty() && payload.after_separator;
        if (payload.text.starts_with(structured_marker) && apply_structured(payload.text, event)) {
            if (event.message.empty())
                event.message = payload.text;
        } else {
            event.message = payload.text;
        }
        events.push_back(std::move(event));
    });
    return events;
}

std::vector<engine_log_event> parse_engine_log_json(std::string_view text,
                                                    std::string_view source_file) {
    std::vector<engine_log_event> events;
    for_each_line(text, [&](std::string_view line, std::size_t ordinal) {
        engine_log_event event;
        event.ordinal = ordinal;
        event.source = std::string(source_file);
        if (!apply_structured(line, event)) {
            event.severity = "notice";
            event.message = std::string(line);
        } else {
            event.engine_format = true;
            if (event.message.empty())
                event.message = std::string(line);
        }
        events.push_back(std::move(event));
    });
    return events;
}

}
