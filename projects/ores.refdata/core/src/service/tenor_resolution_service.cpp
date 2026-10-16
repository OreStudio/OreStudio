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
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <utility>
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
    const auto refuse =
        [&response](outcome what, std::string code, std::string message, std::string field) {
            response.result.outcome = what;
            response.result.message = std::move(message);
            response.result.code = std::move(code);
            response.result.fields.push_back(field_failure{.field = std::move(field),
                                                           .code = response.result.code,
                                                           .message = response.result.message});
            return response;
        };

    tenor_repository tenors;
    const auto tenor_rows = tenors.read_latest(ctx_, request.tenor_code);
    if (tenor_rows.empty()) {
        return refuse(outcome::missing,
                      "tenor_not_found",
                      "No tenor '" + request.tenor_code + "' for this tenant.",
                      "tenor_code");
    }
    const auto& tenor = tenor_rows.front();

    tenor_convention_repository conventions;
    const auto convention_rows = conventions.read_latest(ctx_, request.convention_code);
    if (convention_rows.empty()) {
        return refuse(outcome::missing,
                      "convention_not_found",
                      "No tenor convention '" + request.convention_code + "' for this tenant.",
                      "convention_code");
    }
    const auto& convention = convention_rows.front();

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
        return refuse(outcome::missing,
                      "tenor_not_in_convention",
                      "Tenor '" + request.tenor_code + "' does not belong to convention '" +
                          request.convention_code + "': no tenor_convention_resolutions row.",
                      "tenor_convention_resolutions");
    }

    // An unmaterialised calendar has no business days at all, so every date
    // would look like a business day. That is a missing prerequisite rather
    // than an answer, and saying so is the only honest reply.
    calendar_date_repository dates{ctx_};
    const auto calendar_days = dates.read_latest_by_calendar(request.calendar_code);
    if (calendar_days.empty()) {
        return refuse(outcome::missing,
                      "calendar_not_materialised",
                      "Calendar '" + request.calendar_code +
                          "' has no materialised dates for this tenant.",
                      "calendar_code");
    }
    const auto calendars = build_calendar_set(calendar_days);

    // The persisted codes become the engine's vocabulary here and nowhere
    // else. An unparseable code is data the engine cannot be asked about, so
    // it refuses rather than guessing at a nearest match.
    tenor_resolution_request engine_request;
    engine_request.horizon = request.horizon;
    engine_request.spot_date = request.spot;
    engine_request.calendar_index = 0;
    try {
        engine_request.tenor.kind = parse_tenor_kind_code(tenor.kind);
        engine_request.tenor.period = {parse_period_unit_code(tenor.unit),
                                       tenor.multiplier.value_or(0)};

        auto& rule = engine_request.rule.emplace();
        rule.anchor = parse_tenor_anchor_code(
            resolution_rule->anchor_override.value_or(convention.measured_from));
        rule.algorithm = parse_resolution_algorithm_code(convention.resolution_algorithm);
        rule.roll = parse_business_day_convention_code(convention.business_day_convention_type);
        rule.schedule_step_count = resolution_rule->schedule_step_count.value_or(0);
        if (resolution_rule->offset_unit && resolution_rule->offset_multiplier) {
            rule.offset = {parse_period_unit_code(*resolution_rule->offset_unit),
                           *resolution_rule->offset_multiplier};
        }
    } catch (const std::invalid_argument& e) {
        return refuse(outcome::invalid, "unparseable_code", e.what(), "tenor_convention");
    }

    response.roll_rule = convention.business_day_convention_type;

    // A row that names a schedule binds it to the schedule algorithm: under
    // ANCHOR_OFFSET the engine resolves on the calendar axis and never looks
    // at the schedule at all, so a row naming one there would be silently
    // ignored. Contradictory data is refused rather than read one way.
    if (resolution_rule->schedule_code &&
        engine_request.rule->algorithm != aquant_domain::resolution_algorithm::schedule_step) {
        return refuse(outcome::invalid,
                      "schedule_under_anchor_offset",
                      "The resolution row for tenor '" + request.tenor_code + "' names schedule '" +
                          *resolution_rule->schedule_code + "', but convention '" +
                          request.convention_code + "' resolves by " +
                          convention.resolution_algorithm + ", which never walks a schedule.",
                      "resolution_algorithm");
    }

    // The schedule axis, if the row names one. Its source decides how its
    // dates are produced, and a closed form this engine does not carry is
    // refused rather than walked as an event lookup that will find nothing.
    std::vector<schedule_date> schedule_dates;
    if (resolution_rule->schedule_code) {
        const auto& schedule_code = *resolution_rule->schedule_code;
        tenor_schedule_repository schedules;
        const auto schedule_rows = schedules.read_latest(ctx_, schedule_code);
        if (schedule_rows.empty()) {
            return refuse(outcome::missing,
                          "schedule_not_found",
                          "No tenor schedule '" + schedule_code + "' for this tenant.",
                          "schedule_code");
        }
        const auto& schedule = schedule_rows.front();

        if (schedule.schedule_source == "CLOSED_FORM") {
            if (schedule_code != "ROLL_QUARTER") {
                return refuse(outcome::invalid,
                              "schedule_not_walkable",
                              "Tenor schedule '" + schedule_code +
                                  "' is closed-form and this engine walks only ROLL_QUARTER.",
                              "schedule_code");
            }
            engine_request.rule->schedule = schedule_kind::cds_roll_quarter;
        } else {
            engine_request.rule->schedule = schedule_kind::event_lookup;
            if (!schedule.calendar_code || !schedule.diary_entry_type) {
                return refuse(outcome::invalid,
                              "schedule_incomplete",
                              "Tenor schedule '" + schedule_code +
                                  "' is an event lookup with no calendar or diary entry type.",
                              "schedule_code");
            }
            calendar_event_repository events;
            const auto entries = events.read_latest_by_calendar_code(
                ctx_, *schedule.calendar_code, 0, diary_read_limit);
            for (const auto& entry : entries) {
                if (entry.diary_entry_type == *schedule.diary_entry_type)
                    schedule_dates.push_back(schedule_date{0, entry.event_date});
            }
            const auto by_date = [](const schedule_date& a, const schedule_date& b) {
                return a.date < b.date;
            };
            std::sort(schedule_dates.begin(), schedule_dates.end(), by_date);
        }
    }

    const auto results = tenor_resolution_engine::resolve_batch(
        std::span<const tenor_resolution_request>(&engine_request, 1), calendars, schedule_dates);
    const tenor_resolution_result& result = results.front();

    // The dates the resolution reached travel on the response whether the
    // answer stands or not: a refusal that says only "no" cannot be acted on.
    response.anchor_date = result.anchor_date;
    response.date_before_roll = result.unadjusted_date;
    response.adjusted_date = result.resolved_date;

    if (result.status != aquant_domain::resolution_status::ok) {
        return refuse(outcome::invalid,
                      std::string(aquant_domain::to_string(result.status)),
                      "The rule did not resolve: " +
                          std::string(aquant_domain::to_string(result.status)) + ".",
                      field_name_for(result.status));
    }

    return response;
}

} // namespace ores::refdata::service
