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
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/service/calendar_rule_engine.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include "ores.refdata.core/repository/calendar_rule_repository.hpp"
#include "ores.variability.core/repository/system_settings_repository.hpp"
#include <algorithm>
#include <array>
#include <bitset>
#include <functional>
#include <map>
#include <set>
#include <stdexcept>
#include <unordered_set>

namespace ores::refdata::service {

using namespace ores::logging;
namespace quant_domain = ores::analytics::quant::domain;
namespace quant_service = ores::analytics::quant::service;

namespace {

quant_domain::calendar_rule_kind parse_kind(const std::string& s) {
    if (s == "fixed_date")
        return quant_domain::calendar_rule_kind::fixed_date;
    if (s == "nth_weekday_of_month")
        return quant_domain::calendar_rule_kind::nth_weekday_of_month;
    if (s == "last_weekday_of_month")
        return quant_domain::calendar_rule_kind::last_weekday_of_month;
    if (s == "easter_offset")
        return quant_domain::calendar_rule_kind::easter_offset;
    throw std::runtime_error("Unknown calendar_rule kind: " + s);
}

quant_domain::observance_shift parse_shift(const std::string& s) {
    if (s == "none")
        return quant_domain::observance_shift::none;
    if (s == "nearest_weekday")
        return quant_domain::observance_shift::nearest_weekday;
    if (s == "roll_forward_to_monday")
        return quant_domain::observance_shift::roll_forward_to_monday;
    throw std::runtime_error("Unknown observance_shift: " + s);
}

quant_domain::calendar_rule to_quant_rule(const domain::calendar_rule& v) {
    quant_domain::calendar_rule r;
    r.kind = parse_kind(v.kind);
    if (v.month)
        r.month = std::chrono::month{static_cast<unsigned>(*v.month)};
    if (v.day)
        r.day = static_cast<unsigned>(*v.day);
    if (v.weekday)
        r.weekday = std::chrono::weekday{static_cast<unsigned>(*v.weekday)};
    if (v.occurrence)
        r.occurrence = static_cast<unsigned>(*v.occurrence);
    r.day_offset = v.day_offset;
    r.shift = parse_shift(v.shift);
    if (v.effective_from)
        r.effective_from = std::chrono::year{*v.effective_from};
    if (v.effective_to)
        r.effective_to = std::chrono::year{*v.effective_to};
    return r;
}

quant_domain::calendar_exception to_quant_exception(const domain::calendar_exception& v) {
    return quant_domain::calendar_exception{.date = v.exception_date,
                                            .is_business_day = v.is_business_day};
}

// Default materialisation horizon, used when the corresponding
// calendar.materialisation.* system_setting is not configured for this
// tenant -- keeps this service usable before that seed data exists.
constexpr int k_default_start_offset_years = -2;
constexpr int k_default_end_horizon_year = 2050;
// Ceiling on how far out a caller-supplied end_year (from the NATS
// regenerate_calendar_dates_request, or the system-setting fallback) may
// push the materialisation horizon -- without it, an unbounded end_year
// forces the day-by-day loop below to synthesise and write an unbounded
// number of calendar_date rows.
constexpr int k_max_horizon_years_ahead = 100;

int clamp_horizon_year(int horizon_year) {
    const auto today_y = std::chrono::year_month_day{
        std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())}
                              .year();
    const int max_year = static_cast<int>(today_y) + k_max_horizon_years_ahead;
    return std::min(horizon_year, max_year);
}

int read_int_setting(ores::variability::repository::system_settings_repository& repo,
                     ores::database::context ctx,
                     const std::string& name,
                     int fallback) {
    auto rows = repo.read_latest(ctx, name);
    if (rows.empty())
        return fallback;
    try {
        return std::stoi(rows.front().value);
    } catch (const std::exception&) {
        return fallback;
    }
}

std::chrono::year_month_day year_end(std::chrono::year y) {
    return std::chrono::year_month_day{y / std::chrono::December / std::chrono::last};
}

std::chrono::year_month_day year_start(std::chrono::year y) {
    return y / std::chrono::January / 1;
}

bool is_weekend(std::chrono::weekday wd, const std::bitset<7>& weekend_mask) {
    return weekend_mask.test(static_cast<std::size_t>(wd.c_encoding()));
}

} // namespace

calendar_materialisation_service::calendar_materialisation_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::size_t calendar_materialisation_service::regenerate(const std::string& calendar_code,
                                                          std::optional<std::chrono::year> end_year) {
    repository::calendar_repository calendar_repo;
    auto calendars = calendar_repo.read_latest(ctx_, calendar_code);
    if (calendars.empty())
        throw std::runtime_error("No such calendar: " + calendar_code);
    const auto& cal = calendars.front();

    ores::variability::repository::system_settings_repository settings_repo;
    const auto start_offset = read_int_setting(
        settings_repo, ctx_, "calendar.materialisation.start_offset_years",
        k_default_start_offset_years);
    const auto horizon_year = clamp_horizon_year(
        end_year
            ? static_cast<int>(*end_year)
            : read_int_setting(settings_repo, ctx_, "calendar.materialisation.end_horizon",
                               k_default_end_horizon_year));

    // Cycle guard: a chain of base_calendar_code references must terminate
    // in a base-less calendar. Each recursive call below visits one more
    // link; this set catches a cycle before it recurses forever.
    std::unordered_set<std::string> visited;

    // Local because it recurses via base_calendar_code -- a based
    // calendar's own regeneration is defined in terms of its base's.
    std::function<std::vector<domain::calendar_date>(const domain::calendar&)> instantiate =
        [&](const domain::calendar& c) -> std::vector<domain::calendar_date> {
        if (!visited.insert(c.code).second)
            throw std::runtime_error("base_calendar_code cycle detected at: " + c.code);

        const auto today_y = std::chrono::year_month_day{
            std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())}
                                  .year();
        const auto range_start = year_start(today_y + std::chrono::years{start_offset});
        const auto range_end = year_end(std::chrono::year{horizon_year});

        repository::calendar_exception_repository exc_repo;
        auto exceptions = exc_repo.read_latest_by_calendar_code(ctx_, c.code, 0, 100000);

        std::vector<domain::calendar_date> rows;

        if (!c.base_calendar_code) {
            repository::calendar_rule_repository rule_repo;
            auto rules = rule_repo.read_latest_by_calendar_code(ctx_, c.code, 0, 100000);

            quant_domain::calendar_ruleset ruleset;
            for (const auto& r : rules)
                ruleset.rules.push_back(to_quant_rule(r));
            for (const auto& e : exceptions)
                ruleset.exceptions.push_back(to_quant_exception(e));

            std::array<quant_domain::calendar_ruleset, 1> calendars_span{ruleset};
            auto holidays = quant_service::calendar_rule_engine::instantiate_holidays_batch(
                calendars_span, range_start, range_end);

            std::set<std::chrono::year_month_day> holiday_set;
            for (const auto& h : holidays)
                holiday_set.insert(h.date);

            const std::string source = c.source == "user" ? "user_defined" : "quantlib_computed";

            for (auto d = range_start; d <= range_end;
                 d = std::chrono::year_month_day{
                     std::chrono::sys_days{d} + std::chrono::days{1}}) {
                const auto wd = std::chrono::weekday{std::chrono::sys_days{d}};
                const bool weekend = is_weekend(wd, ruleset.weekend_mask);
                const bool holiday = holiday_set.contains(d);
                domain::calendar_date row;
                row.tenant_id = ctx_.tenant_id().to_string();
                row.calendar_code = c.code;
                row.date = d;
                row.is_business_day = !weekend && !holiday;
                row.source = source;
                row.modified_by = ctx_.actor().empty() ? "system" : ctx_.actor();
                row.performed_by = "system";
                row.change_reason_code = "system.calendar_materialisation";
                row.change_commentary = "Materialised from calendar_rule/calendar_exception";
                rows.push_back(std::move(row));
            }
        } else {
            repository::calendar_repository base_repo;
            auto bases = base_repo.read_latest(ctx_, *c.base_calendar_code);
            if (bases.empty())
                throw std::runtime_error("Invalid base_calendar_code: " + *c.base_calendar_code);
            auto base_rows = instantiate(bases.front());

            std::map<std::chrono::year_month_day, bool> overrides;
            for (const auto& e : exceptions)
                overrides[e.exception_date] = e.is_business_day;

            for (const auto& base_row : base_rows) {
                domain::calendar_date row = base_row;
                row.calendar_code = c.code;
                row.source = "user_adjustment";
                if (auto it = overrides.find(row.date); it != overrides.end())
                    row.is_business_day = it->second;
                row.modified_by = ctx_.actor().empty() ? "system" : ctx_.actor();
                row.change_reason_code = "system.calendar_materialisation";
                row.change_commentary = "Materialised from base " + *c.base_calendar_code +
                    " + calendar_exception overlay";
                rows.push_back(std::move(row));
            }
        }
        return rows;
    };

    auto rows = instantiate(cal);

    // Extend-only: never rewrite a date already materialised at or below
    // the current watermark for this specific calendar.
    auto existing = repository::calendar_date_repository(ctx_).read_latest_by_calendar(calendar_code);
    std::set<std::chrono::year_month_day> existing_dates;
    for (const auto& e : existing)
        existing_dates.insert(e.date);

    std::vector<domain::calendar_date> to_write;
    for (auto& row : rows) {
        if (row.calendar_code == calendar_code && existing_dates.contains(row.date))
            continue;
        to_write.push_back(std::move(row));
    }

    if (!to_write.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Materialising " << to_write.size()
                                  << " calendar_date rows for " << calendar_code;
        repository::calendar_date_repository(ctx_).write(to_write);
    }

    return to_write.size();
}

std::size_t
calendar_materialisation_service::regenerate_all(std::optional<std::chrono::year> end_year) {
    repository::calendar_repository calendar_repo;
    auto calendars = calendar_repo.read_latest(ctx_);

    std::size_t total = 0;
    for (const auto& c : calendars) {
        try {
            total += regenerate(c.code, end_year);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to materialise calendar " << c.code << ": " << e.what();
        }
    }
    return total;
}

}
