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
#include "ores.analytics.quant/service/tenor_resolution_engine.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include <algorithm>
#include <cstddef>
#include <iterator>
#include <optional>

namespace ores::analytics::quant::service {

using domain::business_day_calendar_set;
using domain::resolution_algorithm;
using domain::resolution_status;
using domain::schedule_date;
using domain::schedule_kind;
using domain::tenor_anchor;
using domain::tenor_kind;
using domain::tenor_resolution_request;
using domain::tenor_resolution_result;

using namespace std::chrono;

namespace {

/// The caller's flat schedule dates, laid out CSR-style by schedule_index --
/// the same shape @c business_day_calendar_set uses for holidays. Built here
/// rather than handed in because the engine is its only reader.
class schedule_dates_index final {
public:
    explicit schedule_dates_index(std::span<const schedule_date> dates)
        : dates_(dates) {
        std::size_t schedule_count = 0;
        for (std::size_t i = 0; i < dates.size(); ++i) {
            schedule_count = std::max(schedule_count, dates[i].schedule_index + 1);
            // The walk reads from the first date at or after its start, so
            // rows out of order answer with a different date than the one
            // asked for, silently. Recorded rather than repaired.
            if (i > 0 && is_before(dates[i], dates[i - 1]))
                ordered_ = false;
        }

        offsets_.assign(schedule_count + 1, 0);
        for (const auto& row : dates_) {
            if (row.schedule_index + 1 < offsets_.size())
                ++offsets_[row.schedule_index + 1];
        }
        for (std::size_t i = 1; i < offsets_.size(); ++i)
            offsets_[i] += offsets_[i - 1];
    }

    [[nodiscard]] bool ordered() const noexcept {
        return ordered_;
    }

    [[nodiscard]] std::size_t size_of(std::size_t schedule_index) const noexcept {
        if (schedule_index + 1 >= offsets_.size())
            return 0;
        return offsets_[schedule_index + 1] - offsets_[schedule_index];
    }

    /// The @p n th date of schedule @p schedule_index at or after @p from,
    /// counting a date equal to @p from as the first -- or nothing when fewer
    /// than @p n qualify.
    [[nodiscard]] std::optional<year_month_day>
    nth_on_or_after(std::size_t schedule_index, year_month_day from, int n) const {
        if (schedule_index + 1 >= offsets_.size())
            return std::nullopt;

        auto it = dates_.begin() + static_cast<std::ptrdiff_t>(offsets_[schedule_index]);
        const auto end = dates_.begin() + static_cast<std::ptrdiff_t>(offsets_[schedule_index + 1]);
        while (it != end && it->date < from)
            ++it;
        if (static_cast<std::ptrdiff_t>(n) > end - it)
            return std::nullopt;
        return std::next(it, n - 1)->date;
    }

private:
    static bool is_before(const schedule_date& a, const schedule_date& b) {
        if (a.schedule_index != b.schedule_index)
            return a.schedule_index < b.schedule_index;
        return a.date < b.date;
    }

    std::span<const schedule_date> dates_;
    std::vector<std::size_t> offsets_;
    bool ordered_ = true;
};

struct anchor_outcome final {
    resolution_status status = resolution_status::ok;
    year_month_day date{};
};

anchor_outcome resolve_anchor(tenor_anchor anchor,
                              year_month_day horizon,
                              const std::optional<year_month_day>& spot_date) {
    switch (anchor) {
        case tenor_anchor::today:
            return {resolution_status::ok, horizon};
        case tenor_anchor::tomorrow:
            return {resolution_status::ok, year_month_day{sys_days{horizon} + days{1}}};
        case tenor_anchor::spot:
            if (!spot_date)
                return {resolution_status::missing_spot_date, {}};
            return {resolution_status::ok, *spot_date};
        case tenor_anchor::none:
        case tenor_anchor::near_leg:
        case tenor_anchor::imm_roll:
            return {resolution_status::unsupported_anchor, {}};
    }
    return {resolution_status::unsupported_anchor, {}};
}

/// The CDS quarterly roll date at or after @p from: the 20th of March, June,
/// September or December, stated exactly and *not* moved off a weekend. Two
/// conventions trade under the name "IMM date" and this is the credit one; the
/// next IMM third Wednesday is what @c calendar_arithmetic::next_imm_date
/// gives, and they are different dates (doc/knowledge/domain/imm_dates.org).
/// Moving the 20th is the convention's job, and it is applied by the caller of
/// this function, not here.
year_month_day cds_roll_on_or_after(year_month_day from) {
    const auto m = static_cast<unsigned>(from.month());
    const unsigned quarter_month = ((m + 2u) / 3u) * 3u;

    auto candidate = year_month_day{from.year() / month{quarter_month} / day{20}};
    if (candidate < from) {
        if (quarter_month < 12u)
            candidate = year_month_day{from.year() / month{quarter_month + 3u} / day{20}};
        else
            candidate = year_month_day{(from.year() + years{1}) / March / day{20}};
    }
    return candidate;
}

year_month_day nth_cds_roll_on_or_after(year_month_day from, int n) {
    auto candidate = cds_roll_on_or_after(from);
    for (int step = 1; step < n; ++step)
        candidate = cds_roll_on_or_after(year_month_day{sys_days{candidate} + days{1}});
    return candidate;
}

tenor_resolution_result resolve_one(const tenor_resolution_request& request,
                                    const business_day_calendar_set& calendars,
                                    const schedule_dates_index& schedules) {
    tenor_resolution_result result;

    if (!request.rule) {
        result.status = resolution_status::tenor_not_in_convention;
        return result;
    }
    const auto& rule = *request.rule;

    if (request.calendar_index >= calendars.calendar_count()) {
        result.status = resolution_status::calendar_index_out_of_range;
        return result;
    }

    const auto anchor = resolve_anchor(rule.anchor, request.horizon, request.spot_date);
    if (anchor.status != resolution_status::ok) {
        result.status = anchor.status;
        return result;
    }
    result.anchor_date = anchor.date;

    // A PERIOD tenor states its own duration; a SPECIAL one has none and takes
    // the one its convention's resolution row supplies.
    const auto period =
        request.tenor.kind == tenor_kind::period ? request.tenor.period : rule.offset;
    if (period.is_empty()) {
        result.status = request.tenor.kind == tenor_kind::period ?
                            resolution_status::period_tenor_without_period :
                            resolution_status::special_tenor_without_offset;
        return result;
    }

    if (rule.algorithm == resolution_algorithm::anchor_offset) {
        result.resolved_date = calendar_arithmetic::advance(
            anchor.date, period, rule.roll, rule.end_of_month, request.calendar_index, calendars);
        return result;
    }

    // A row either names a schedule axis and a step count, or neither. Naming
    // exactly one of the two is an inconsistent configuration and refuses;
    // naming neither is the axis fallback below.
    const bool has_axis = rule.schedule != schedule_kind::none;
    const bool has_step_count = rule.schedule_step_count != 0;
    if (has_axis != has_step_count || (has_axis && rule.schedule_step_count < 1)) {
        result.status = resolution_status::inconsistent_schedule_declaration;
        return result;
    }

    if (!has_axis) {
        // A membership-only row resolves on the calendar axis, exactly as
        // ANCHOR_OFFSET resolves it. That is how a split pillar with no
        // schedule of its own lands in a schedule-driven convention.
        result.resolved_date = calendar_arithmetic::advance(
            anchor.date, period, rule.roll, rule.end_of_month, request.calendar_index, calendars);
        return result;
    }

    // The walk starts at the anchor plus the period *unadjusted*: the schedule
    // holds its own business-day meaning, and rolling before walking would
    // step to a different date than the one the row names.
    const auto walk_start = calendar_arithmetic::add_period(anchor.date, period);

    year_month_day walked{};
    if (rule.schedule == schedule_kind::cds_roll_quarter) {
        walked = nth_cds_roll_on_or_after(walk_start, rule.schedule_step_count);
    } else {
        if (!schedules.ordered()) {
            result.status = resolution_status::schedule_dates_not_ordered;
            return result;
        }
        if (schedules.size_of(rule.schedule_index) == 0) {
            result.status = resolution_status::schedule_without_dates;
            return result;
        }
        const auto found =
            schedules.nth_on_or_after(rule.schedule_index, walk_start, rule.schedule_step_count);
        if (!found) {
            result.status = resolution_status::schedule_exhausted;
            return result;
        }
        walked = *found;
    }

    result.resolved_date =
        calendar_arithmetic::adjust(walked, rule.roll, request.calendar_index, calendars);
    return result;
}

} // namespace

std::vector<tenor_resolution_result>
tenor_resolution_engine::resolve_batch(std::span<const tenor_resolution_request> requests,
                                       const business_day_calendar_set& calendars,
                                       std::span<const schedule_date> schedule_dates) {
    const schedule_dates_index schedules{schedule_dates};

    std::vector<tenor_resolution_result> results;
    results.reserve(requests.size());
    for (const auto& request : requests)
        results.push_back(resolve_one(request, calendars, schedules));
    return results;
}

} // namespace ores::analytics::quant::service
