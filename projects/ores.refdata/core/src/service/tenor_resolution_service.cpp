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
#include "ores.refdata.core/service/tenor_resolution_service.hpp"
#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include "ores.analytics.quant/service/tenor_resolution_engine.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.refdata.core/repository/calendar_event_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include "ores.refdata.core/repository/tenor_schedule_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <bitset>
#include <chrono>
#include <cstdint>
#include <limits>
#include <map>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace ores::refdata::service {

namespace aquant_domain = ores::analytics::quant::domain;

using ores::analytics::quant::domain::business_day_calendar_set;
using ores::analytics::quant::domain::calendar_date_row;
using ores::analytics::quant::domain::parse_business_day_convention_code;
using ores::analytics::quant::domain::parse_period_unit_code;
using ores::analytics::quant::domain::parse_resolution_algorithm_code;
using ores::analytics::quant::domain::parse_tenor_anchor_code;
using ores::analytics::quant::domain::parse_tenor_kind_code;
using ores::analytics::quant::domain::schedule_date;
using ores::analytics::quant::domain::schedule_kind;
using ores::analytics::quant::domain::tenor_resolution_request;
using ores::analytics::quant::domain::tenor_resolution_result;
using ores::analytics::quant::domain::tenor_rule;
using ores::analytics::quant::service::tenor_resolution_engine;
using ores::refdata::repository::calendar_date_repository;
using ores::refdata::repository::calendar_event_repository;
using ores::refdata::repository::tenor_convention_repository;
using ores::refdata::repository::tenor_convention_resolution_repository;
using ores::refdata::repository::tenor_repository;
using ores::refdata::repository::tenor_schedule_repository;
using ores::utility::domain::field_failure;
using ores::utility::domain::outcome;

using namespace std::chrono;

namespace {

/// A Saturday and Sunday weekend, which every calendar this operation serves
/// keeps. The calendar's own materialised days carry the rest: a day it holds
/// as a non-business day is passed to the engine as a holiday, which is what
/// it is for the one question the engine asks.
constexpr unsigned weekend_mask = 0b1000001;

/// How many of a calendar's diary entries to read when walking an
/// event-lookup schedule. Generous on purpose: a central bank meets a handful
/// of times a year, so this reaches decades ahead, and a schedule longer than
/// it would be read short rather than refused.
constexpr std::uint32_t diary_read_limit = 4096;

/// The engine's holiday set for one calendar, built from the days the
/// materialiser wrote. A materialised calendar holds a row for every day in
/// its window with the answer already decided, so the closed days are exactly
/// the ones to hand over.
business_day_calendar_set build_calendar_set(const std::vector<domain::calendar_date>& days) {
    std::vector<calendar_date_row> holidays;
    holidays.reserve(days.size());
    for (const auto& day : days) {
        if (!day.is_business_day)
            holidays.push_back(calendar_date_row{0, day.date});
    }

    const auto by_date = [](const calendar_date_row& a, const calendar_date_row& b) {
        return a.date < b.date;
    };
    std::sort(holidays.begin(), holidays.end(), by_date);
    holidays.erase(std::unique(holidays.begin(),
                               holidays.end(),
                               [](const calendar_date_row& a, const calendar_date_row& b) {
                                   return a.date == b.date;
                               }),
                   holidays.end());

    return business_day_calendar_set::from_rows(holidays, 1, {std::bitset<7>{weekend_mask}});
}

/// Names the row a refusal is about, in the words the database uses, so a
/// caller can go and look at it.
std::string field_name_for(aquant_domain::resolution_status status) {
    switch (status) {
        case aquant_domain::resolution_status::tenor_not_in_convention:
            return "tenor_convention_resolutions";
        case aquant_domain::resolution_status::missing_spot_date:
            return "spot";
        case aquant_domain::resolution_status::unsupported_anchor:
            return "tenor_anchor";
        case aquant_domain::resolution_status::period_tenor_without_period:
        case aquant_domain::resolution_status::special_tenor_without_offset:
        case aquant_domain::resolution_status::inconsistent_schedule_declaration:
        case aquant_domain::resolution_status::schedule_without_dates:
        case aquant_domain::resolution_status::schedule_dates_not_ordered:
        case aquant_domain::resolution_status::schedule_exhausted:
            return "tenor_convention_resolutions";
        case aquant_domain::resolution_status::calendar_index_out_of_range:
            return "calendar_code";
        case aquant_domain::resolution_status::ok:
            return {};
    }
    return {};
}

/// A refusal: what happened, which step refused, and the row a caller can go
/// and look at. Every refusal on every path is this shape.
ores::utility::domain::result
refusal_result(outcome what, std::string code, std::string message, std::string field) {
    ores::utility::domain::result refusal;
    refusal.outcome = what;
    refusal.message = std::move(message);
    refusal.code = std::move(code);
    refusal.fields.push_back(field_failure{
        .field = std::move(field), .code = refusal.code, .message = refusal.message});
    return refusal;
}

/// Fills a refusal into a response and hands it back.
messaging::resolve_tenor_response refuse_with(messaging::resolve_tenor_response response,
                                              outcome what,
                                              std::string code,
                                              std::string message,
                                              std::string field) {
    response.result =
        refusal_result(what, std::move(code), std::move(message), std::move(field));
    return response;
}

/// The refusal for a calendar nobody materialised. An unmaterialised calendar
/// has no business days at all, so every date would look like a business day.
/// That is a missing prerequisite rather than an answer, and saying so is the
/// only honest reply.
ores::utility::domain::result calendar_not_materialised(const std::string& calendar_code) {
    return refusal_result(outcome::missing,
                          "calendar_not_materialised",
                          "Calendar '" + calendar_code +
                              "' has no materialised dates for this tenant.",
                          "calendar_code");
}

/// The calendar's holidays as the engine takes them, or nothing when the
/// calendar has no materialised dates.
std::optional<business_day_calendar_set>
load_calendar(const tenor_resolution_service::context& ctx, const std::string& calendar_code) {
    calendar_date_repository dates{ctx};
    const auto days = dates.read_latest_by_calendar(calendar_code);
    if (days.empty())
        return std::nullopt;
    return build_calendar_set(days);
}

/// The dates of every schedule a batch walks, in the flat shape the engine
/// reads: ordered by schedule and then by date. A schedule is read the first
/// time a row names it, so a convention whose rows share a schedule reads it
/// once.
class schedule_axes final {
public:
    explicit schedule_axes(tenor_resolution_service::context ctx)
        : ctx_(std::move(ctx)) {}

    /// Points the rule at the named schedule. A schedule that cannot be walked
    /// is a refusal in the response, and every row that names it gets the same
    /// one.
    bool attach(const std::string& schedule_code,
                tenor_rule& rule,
                messaging::resolve_tenor_response& response) {
        auto found = known_.find(schedule_code);
        if (found == known_.end())
            found = known_.emplace(schedule_code, load(schedule_code)).first;

        if (const auto* refused = std::get_if<refusal>(&found->second)) {
            response = refuse_with(std::move(response),
                                   refused->what,
                                   refused->code,
                                   refused->message,
                                   refused->field);
            return false;
        }

        const auto& walked = std::get<axis>(found->second);
        rule.schedule = walked.kind;
        rule.schedule_index = walked.index;
        return true;
    }

    const std::vector<schedule_date>& dates() const { return dates_; }

private:
    struct axis {
        schedule_kind kind;
        std::size_t index;
    };

    struct refusal {
        outcome what;
        std::string code;
        std::string message;
        std::string field;
    };

    using verdict = std::variant<axis, refusal>;

    /// The schedule's source decides how its dates are produced, and a closed
    /// form this engine does not carry is refused rather than walked as an
    /// event lookup that will find nothing.
    verdict load(const std::string& schedule_code) {
        tenor_schedule_repository schedules;
        const auto schedule_rows = schedules.read_latest(ctx_, schedule_code);
        if (schedule_rows.empty()) {
            return refusal{outcome::missing,
                           "schedule_not_found",
                           "No tenor schedule '" + schedule_code + "' for this tenant.",
                           "schedule_code"};
        }
        const auto& schedule = schedule_rows.front();
        const std::size_t index = known_.size();

        if (schedule.schedule_source == "CLOSED_FORM") {
            if (schedule_code != "ROLL_QUARTER") {
                return refusal{outcome::invalid,
                               "schedule_not_walkable",
                               "Tenor schedule '" + schedule_code +
                                   "' is closed-form and this engine walks only ROLL_QUARTER.",
                               "schedule_code"};
            }
            return axis{schedule_kind::cds_roll_quarter, index};
        }

        if (!schedule.calendar_code || !schedule.diary_entry_type) {
            return refusal{outcome::invalid,
                           "schedule_incomplete",
                           "Tenor schedule '" + schedule_code +
                               "' is an event lookup with no calendar or diary entry type.",
                           "schedule_code"};
        }

        calendar_event_repository events;
        const auto entries =
            events.read_latest_by_calendar_code(ctx_, *schedule.calendar_code, 0, diary_read_limit);
        std::vector<schedule_date> mine;
        for (const auto& entry : entries) {
            if (entry.diary_entry_type == *schedule.diary_entry_type)
                mine.push_back(schedule_date{index, entry.event_date});
        }
        std::sort(mine.begin(), mine.end(), [](const schedule_date& a, const schedule_date& b) {
            return a.date < b.date;
        });
        dates_.insert(dates_.end(), mine.begin(), mine.end());
        return axis{schedule_kind::event_lookup, index};
    }

    tenor_resolution_service::context ctx_;
    std::map<std::string, verdict> known_;
    std::vector<schedule_date> dates_;
};

/// Turns one resolution row into the engine's question. The persisted codes
/// become the engine's vocabulary here and nowhere else. A row that cannot be
/// asked about is refused in @p response and the function returns false: an
/// unparseable code is data the engine cannot be asked about, and it refuses
/// rather than guessing at a nearest match.
bool build_request(const domain::tenor& tenor,
                   const domain::tenor_convention& convention,
                   const domain::tenor_convention_resolution& resolution_rule,
                   year_month_day horizon,
                   const std::optional<year_month_day>& spot,
                   schedule_axes& axes,
                   tenor_resolution_request& engine_request,
                   messaging::resolve_tenor_response& response) {
    engine_request = tenor_resolution_request{};
    engine_request.horizon = horizon;
    engine_request.spot_date = spot;
    engine_request.calendar_index = 0;
    try {
        engine_request.tenor.kind = parse_tenor_kind_code(tenor.kind);
        engine_request.tenor.period = {parse_period_unit_code(tenor.unit),
                                       tenor.multiplier.value_or(0)};

        auto& rule = engine_request.rule.emplace();
        rule.anchor = parse_tenor_anchor_code(
            resolution_rule.anchor_override.value_or(convention.measured_from));
        rule.algorithm = parse_resolution_algorithm_code(convention.resolution_algorithm);
        rule.roll = parse_business_day_convention_code(convention.business_day_convention_type);
        rule.schedule_step_count = resolution_rule.schedule_step_count.value_or(0);
        if (resolution_rule.offset_unit && resolution_rule.offset_multiplier) {
            rule.offset = {parse_period_unit_code(*resolution_rule.offset_unit),
                           *resolution_rule.offset_multiplier};
        }
    } catch (const std::invalid_argument& e) {
        response = refuse_with(
            std::move(response), outcome::invalid, "unparseable_code", e.what(), "tenor_convention");
        return false;
    }

    if (!resolution_rule.schedule_code)
        return true;

    // A row that names a schedule binds it to the schedule algorithm: under
    // ANCHOR_OFFSET the engine resolves on the calendar axis and never looks
    // at the schedule at all, so a row naming one there would be silently
    // ignored. Contradictory data is refused rather than read one way.
    if (engine_request.rule->algorithm != aquant_domain::resolution_algorithm::schedule_step) {
        response = refuse_with(std::move(response),
                               outcome::invalid,
                               "schedule_under_anchor_offset",
                               "The resolution row for tenor '" + tenor.code + "' names schedule '" +
                                   *resolution_rule.schedule_code + "', but convention '" +
                                   convention.code + "' resolves by " +
                                   convention.resolution_algorithm + ", which never walks a schedule.",
                               "resolution_algorithm");
        return false;
    }

    return axes.attach(*resolution_rule.schedule_code, *engine_request.rule, response);
}

/// Reads the engine's answer into a response. The dates the resolution reached
/// travel on the response whether the answer stands or not: a refusal that says
/// only "no" cannot be acted on.
messaging::resolve_tenor_response answered(messaging::resolve_tenor_response response,
                                           const tenor_resolution_result& result) {
    response.anchor_date = result.anchor_date;
    response.date_before_roll = result.unadjusted_date;
    response.adjusted_date = result.resolved_date;

    if (result.status != aquant_domain::resolution_status::ok) {
        return refuse_with(std::move(response),
                           outcome::invalid,
                           std::string(aquant_domain::to_string(result.status)),
                           "The rule did not resolve: " +
                               std::string(aquant_domain::to_string(result.status)) + ".",
                           field_name_for(result.status));
    }

    return response;
}

/// One tenor's answer, named by the tenor it is about.
messaging::convention_tenor_resolution entry_of(std::string tenor_code,
                                                const messaging::resolve_tenor_response& answer) {
    return messaging::convention_tenor_resolution{.tenor_code = std::move(tenor_code),
                                                  .result = answer.result,
                                                  .anchor_date = answer.anchor_date,
                                                  .date_before_roll = answer.date_before_roll,
                                                  .adjusted_date = answer.adjusted_date};
}

} // namespace

tenor_resolution_service::tenor_resolution_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::resolve_tenor_response
tenor_resolution_service::resolve(const messaging::resolve_tenor_request& request) const {
    messaging::resolve_tenor_response response;
    response.calendar = request.calendar_code;

    // A refusal is the answer to "why not", so every one of these fills the
    // response and returns rather than throwing. The dates already counted
    // travel with it, because a person looking at a refusal wants to know how
    // far the resolution got.
    tenor_repository tenors;
    const auto tenor_rows = tenors.read_latest(ctx_, request.tenor_code);
    if (tenor_rows.empty()) {
        return refuse_with(response,
                           outcome::missing,
                           "tenor_not_found",
                           "No tenor '" + request.tenor_code + "' for this tenant.",
                           "tenor_code");
    }

    tenor_convention_repository conventions;
    const auto convention_rows = conventions.read_latest(ctx_, request.convention_code);
    if (convention_rows.empty()) {
        return refuse_with(response,
                           outcome::missing,
                           "convention_not_found",
                           "No tenor convention '" + request.convention_code + "' for this tenant.",
                           "convention_code");
    }

    // A tenor resolves only because a row says it belongs to the convention,
    // and that row is also where the duration of a SPECIAL tenor lives. Its
    // absence is a refusal, not a default.
    tenor_convention_resolution_repository resolutions{ctx_};
    const auto resolution_rows = resolutions.read_latest_by_convention(request.convention_code);
    const auto resolution_rule =
        std::find_if(resolution_rows.begin(), resolution_rows.end(), [&request](const auto& row) {
            return row.tenor_code == request.tenor_code;
        });
    if (resolution_rule == resolution_rows.end()) {
        return refuse_with(response,
                           outcome::missing,
                           "tenor_not_in_convention",
                           "Tenor '" + request.tenor_code + "' does not belong to convention '" +
                               request.convention_code + "': no tenor_convention_resolutions row.",
                           "tenor_convention_resolutions");
    }

    return resolve_rows(tenor_rows.front(),
                        convention_rows.front(),
                        *resolution_rule,
                        request.calendar_code,
                        request.horizon,
                        request.spot);
}

messaging::resolve_tenor_response tenor_resolution_service::preview(
    const messaging::preview_tenor_resolution_request& request) const {
    // Nothing is read for the rule: it is the draft the caller is holding, and
    // reading the stored row instead would answer about the wrong rule. The
    // calendar is still the database's, because holidays are not a draft.
    return resolve_rows(request.tenor,
                        request.convention,
                        request.resolution,
                        request.calendar_code,
                        request.horizon,
                        request.spot);
}

messaging::resolve_convention_response tenor_resolution_service::resolve_convention(
    const messaging::resolve_convention_request& request) const {
    messaging::resolve_convention_response response;
    response.calendar = request.calendar_code;

    tenor_convention_repository conventions;
    const auto convention_rows = conventions.read_latest(ctx_, request.convention_code);
    if (convention_rows.empty()) {
        response.result =
            refusal_result(outcome::missing,
                           "convention_not_found",
                           "No tenor convention '" + request.convention_code + "' for this tenant.",
                           "convention_code");
        return response;
    }
    const auto& convention = convention_rows.front();
    response.roll_rule = convention.business_day_convention_type;

    const auto calendars = load_calendar(ctx_, request.calendar_code);
    if (!calendars) {
        response.result = calendar_not_materialised(request.calendar_code);
        return response;
    }

    // The rows are the convention's membership: a tenor is in the convention
    // because a row says so, and that row is what the tenor resolves by.
    tenor_convention_resolution_repository resolutions{ctx_};
    const auto resolution_rows = resolutions.read_latest_by_convention(request.convention_code);
    if (resolution_rows.empty())
        return response;

    std::vector<std::string> codes;
    codes.reserve(resolution_rows.size());
    for (const auto& row : resolution_rows)
        codes.push_back(row.tenor_code);
    tenor_repository tenors;
    const auto tenor_rows = tenors.read_latest(ctx_, codes);

    // The catalogue's own order, so the answer reads the way the term
    // structure does. A row whose tenor the catalogue does not hold sorts last
    // and is refused where it stands.
    using row_and_tenor =
        std::pair<const domain::tenor_convention_resolution*, const domain::tenor*>;
    std::vector<row_and_tenor> ordered;
    ordered.reserve(resolution_rows.size());
    for (const auto& row : resolution_rows) {
        const auto tenor = std::find_if(tenor_rows.begin(), tenor_rows.end(), [&row](const auto& t) {
            return t.code == row.tenor_code;
        });
        ordered.emplace_back(&row, tenor == tenor_rows.end() ? nullptr : &*tenor);
    }
    const auto order_of = [](const row_and_tenor& entry) {
        return entry.second ? entry.second->sort_order : std::numeric_limits<int>::max();
    };
    std::sort(ordered.begin(), ordered.end(), [&order_of](const auto& a, const auto& b) {
        if (order_of(a) != order_of(b))
            return order_of(a) < order_of(b);
        return a.first->tenor_code < b.first->tenor_code;
    });

    // A row that cannot be asked about is refused in its own entry and left
    // out of the batch, so it costs only itself.
    schedule_axes axes{ctx_};
    std::vector<tenor_resolution_request> asked;
    std::vector<std::size_t> asked_entry;
    response.tenors.reserve(ordered.size());
    for (const auto& [rule, tenor] : ordered) {
        messaging::resolve_tenor_response answer;
        tenor_resolution_request question;
        if (!tenor) {
            answer = refuse_with(std::move(answer),
                                 outcome::missing,
                                 "tenor_not_found",
                                 "No tenor '" + rule->tenor_code + "' for this tenant.",
                                 "tenor_code");
        } else if (build_request(
                       *tenor, convention, *rule, request.horizon, request.spot, axes, question, answer)) {
            asked.push_back(question);
            asked_entry.push_back(response.tenors.size());
        }
        response.tenors.push_back(entry_of(rule->tenor_code, answer));
    }

    if (asked.empty())
        return response;

    const auto results = tenor_resolution_engine::resolve_batch(asked, *calendars, axes.dates());
    for (std::size_t i = 0; i != results.size(); ++i) {
        auto& entry = response.tenors[asked_entry[i]];
        entry = entry_of(entry.tenor_code, answered(messaging::resolve_tenor_response{}, results[i]));
    }
    return response;
}

messaging::resolve_tenor_response
tenor_resolution_service::resolve_rows(const domain::tenor& tenor,
                                       const domain::tenor_convention& convention,
                                       const domain::tenor_convention_resolution& resolution_rule,
                                       const std::string& calendar_code,
                                       year_month_day horizon,
                                       const std::optional<year_month_day>& spot) const {
    messaging::resolve_tenor_response response;
    response.calendar = calendar_code;
    response.roll_rule = convention.business_day_convention_type;

    const auto calendars = load_calendar(ctx_, calendar_code);
    if (!calendars) {
        response.result = calendar_not_materialised(calendar_code);
        return response;
    }

    schedule_axes axes{ctx_};
    tenor_resolution_request engine_request;
    if (!build_request(
            tenor, convention, resolution_rule, horizon, spot, axes, engine_request, response)) {
        return response;
    }

    const auto results = tenor_resolution_engine::resolve_batch(
        std::span<const tenor_resolution_request>(&engine_request, 1), *calendars, axes.dates());
    return answered(std::move(response), results.front());
}

} // namespace ores::refdata::service
