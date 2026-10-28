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
#include "ores.ore.core/log/engine_run_verdict.hpp"
#include "ores.ore.core/log/engine_log.hpp"
#include <algorithm>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::log {

namespace {

/** The engine's log, which sits beside its results rather than among them. */
constexpr std::string_view engine_log_name = "log.txt";

/**
 * @brief How many of the engine's errors a failure names.
 *
 * A run can log the same error once per trade. The message has to stay
 * readable, so it carries the last few and says how many it left out.
 */
constexpr std::size_t max_reported_errors = 5;

bool is_analytics(const std::string& file_name) {
    return !file_name.empty() && file_name != engine_log_name;
}

/**
 * @brief The marker ORE writes when the analytic it was asked for did not run.
 *
 * ORE logs it at the end of a failed analytics run and still exits 0.
 */
constexpr std::string_view fatal_marker = "Error in ORE analytics";

/**
 * @brief The lines the engine wrote in its own shape, in the order it wrote them.
 *
 * A line that states less than the engine's own fields is not the engine
 * speaking, and a verdict is about what the engine said.
 */
std::vector<engine_log_event> engine_events(std::string_view engine_log) {
    std::vector<engine_log_event> events;
    for (const auto& event : parse_engine_log_text(engine_log, engine_log_name))
        if (event.states_line_fields)
            events.push_back(event);
    return events;
}

/** The messages of the engine's ERROR lines, in the order it wrote them. */
std::vector<std::string> error_messages(std::string_view engine_log) {
    std::vector<std::string> errors;
    for (const auto& event : engine_events(engine_log))
        if (event.severity == "error")
            errors.push_back(event.message);
    return errors;
}

/** Whether a message says the same thing as one already collected. */
bool restates(const std::vector<std::string>& failures, const std::string& message) {
    return std::ranges::any_of(failures, [&](const auto& failure) {
        return failure.find(message) != std::string::npos ||
               message.find(failure) != std::string::npos;
    });
}

/**
 * @brief What the engine says it could not do, one line per reason.
 *
 * The engine writes the useful detail before the verdict that names the
 * analytic: the mandatory quote it could not find, the curve it failed to
 * build, and the trade it was building when it stopped. A structured message
 * carries those as fields, so its own sentence is reported with the trade or
 * the curve that explains it.
 */
std::vector<std::string> explanations(std::string_view engine_log) {
    std::vector<std::string> reasons;
    for (const auto& event : engine_events(engine_log)) {
        if (event.severity != "error")
            continue;

        // A structured message that states no message of its own says nothing a
        // person can act on, so it is not a reason.
        if (!event.message_stated)
            continue;

        auto text = event.message;
        if (event.subject == "trade") {
            text += " (trade " + event.subject_id;
            const auto type = field_of(event, "tradeType");
            if (!type.empty())
                text += ", " + type;
            text += ")";
        } else if (!event.cause.empty()) {
            text += " (curve " + event.cause + ")";
        }
        if (!restates(reasons, text))
            reasons.push_back(std::move(text));
    }
    return reasons;
}

/**
 * @brief The engine's own verdicts that the analytics failed, one per distinct
 * message.
 *
 * ORE writes the same sentence twice: on an ALERT line, and inside the
 * structured warning that follows it. The first form is the shorter one, so it
 * is the one reported.
 */
std::vector<std::string> fatal_messages(std::string_view engine_log) {
    std::vector<std::string> failures;
    for (const auto& event : engine_events(engine_log)) {
        if (event.message.contains(fatal_marker) && !restates(failures, event.message))
            failures.push_back(event.message);
    }
    return failures;
}

/** The trailing clause that names the errors, capped for readability. */
std::string reported_errors(const std::vector<std::string>& errors) {
    const auto omitted = errors.size() > max_reported_errors ? errors.size() - max_reported_errors :
                                                               static_cast<std::size_t>(0);
    std::string out = omitted == 0 ? " The engine reported:" :
                                     std::format(" The engine reported {} errors; the last {} are:",
                                                 errors.size(),
                                                 max_reported_errors);
    for (auto it = errors.begin() + static_cast<std::ptrdiff_t>(omitted); it != errors.end(); ++it)
        out += "\n  " + *it;
    return out;
}

}

engine_run_outcome judge_engine_run(const std::vector<std::string>& output_file_names,
                                    std::string_view engine_log) {
    engine_run_outcome outcome;

    // The engine's own verdict comes first. ORE writes its calibration and
    // timing reports whether or not the analytic it was asked for ran, so the
    // files alone cannot tell a failed run from a working one, and a run that
    // wrote nothing but its log still states why.
    const auto failures = fatal_messages(engine_log);
    if (!failures.empty()) {
        auto reasons = failures;
        for (const auto& reason : explanations(engine_log)) {
            if (!restates(reasons, reason))
                reasons.push_back(reason);
        }
        outcome.failure = "The engine's analytic failed." + reported_errors(reasons);
        return outcome;
    }

    if (std::ranges::any_of(output_file_names, is_analytics)) {
        outcome.succeeded = true;
        return outcome;
    }

    const auto errors = error_messages(engine_log);

    outcome.failure = "The engine produced no analytics.";
    if (errors.empty()) {
        if (!engine_log.empty())
            outcome.failure +=
                std::format(" It logged {} lines and no error.", line_count(engine_log));
        return outcome;
    }

    outcome.failure += reported_errors(errors);
    return outcome;
}

}
