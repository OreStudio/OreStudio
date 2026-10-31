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
#include "ores.reporting.core/service/cut_resolution_plan.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <string>
#include <vector>

namespace ores::reporting::service {

namespace {

constexpr std::string_view rule_same_day_close = "same_day_close";
constexpr std::string_view rule_month_close = "last_business_day_of_month_close";
constexpr std::string_view rule_year_close = "last_business_day_of_year_close";
constexpr std::string_view rule_same_day_open = "same_day_open";
constexpr std::string_view rule_cut_it_is_built_on = "cut_it_is_built_on";
constexpr std::string_view rule_run_instant = "run_instant";
constexpr std::string_view rule_the_cut_named = "the_cut_named";

std::chrono::year_month_day date_of(const std::chrono::system_clock::time_point& tp) {
    return std::chrono::year_month_day(std::chrono::floor<std::chrono::days>(tp));
}

std::string date_text(const std::chrono::system_clock::time_point& tp) {
    return ores::platform::time::datetime::to_iso8601_date(date_of(tp));
}

std::string instant_text(const std::chrono::system_clock::time_point& tp) {
    auto text = ores::platform::time::datetime::to_iso8601_utc(tp);
    // The wire shape carries microseconds; a refusal names the second.
    if (text.size() > 19)
        text.erase(19);
    return text + "Z";
}

bool is_weekday(const std::chrono::year_month_day& date) {
    const auto day = std::chrono::weekday(std::chrono::sys_days(date));
    return day != std::chrono::Saturday && day != std::chrono::Sunday;
}

/// One candidate and the kind it was taken under, which is what its rule is.
struct candidate {
    const domain::cut* cut;
    const domain::cut_kind* kind;
};

cut_resolution refuse(std::string sentence) {
    cut_resolution out;
    out.refusal = std::move(sentence);
    return out;
}

/**
 * @brief The usable candidate first, else a sentence saying what stood in its way.
 *
 * A cut that waits on an authorisation is a body that exists and cannot be
 * priced from, and the refusal names the request it waits on: the difference is
 * worth stating, because the remedy is to chase a person and not to take a cut.
 */
cut_resolution settle(const std::vector<candidate>& matches,
                      const std::string& looked_for,
                      const cut_resolution_context& ctx) {
    for (const auto& c : matches) {
        if (is_usable(derive_state(*c.cut, 0))) {
            cut_resolution out;
            out.cut = *c.cut;
            return out;
        }
    }

    for (const auto& c : matches) {
        const auto state = derive_state(*c.cut, 0);
        if (state == cut_state::taken) {
            const auto label = cut_label_of(*c.cut, *c.kind);
            std::string sentence = "The cut " + label + " for scope " + ctx.scope_code +
                                   " at rates centre " + ctx.business_centre_code +
                                   " is taken and waits on ";
            if (c.cut->approval_request_id)
                sentence += "the approval request " +
                            boost::uuids::to_string(*c.cut->approval_request_id) + ".";
            else
                sentence += "an authorisation that is not recorded.";
            return refuse(std::move(sentence));
        }
        if (state == cut_state::refused) {
            const auto label = cut_label_of(*c.cut, *c.kind);
            std::string sentence = "The cut " + label + " for scope " + ctx.scope_code +
                                   " was refused and no second decision authorises it.";
            return refuse(std::move(sentence));
        }
    }

    return refuse("No " + looked_for + " is held for scope " + ctx.scope_code +
                  " at rates centre " + ctx.business_centre_code +
                  " on " + date_text(ctx.covered_date) + ".");
}

/// The candidates whose kind names @p rule and whose instant falls on @p date.
std::vector<candidate> matching(const std::vector<candidate>& candidates,
                                std::string_view rule,
                                const std::chrono::year_month_day& date) {
    std::vector<candidate> matches;
    for (const auto& c : candidates) {
        if (c.kind->resolution_rule_code == rule && date_of(c.cut->timepoint) == date)
            matches.push_back(c);
    }
    return matches;
}

cut_resolution resolve_by_rule(std::string_view rule,
                               const std::vector<candidate>& candidates,
                               const cut_resolution_context& ctx) {
    const auto covered = date_of(ctx.covered_date);

    if (rule == rule_same_day_open)
        return settle(matching(candidates, rule, covered), "start of day cut", ctx);

    if (rule == rule_cut_it_is_built_on)
        return settle(matching(candidates, rule, covered), "verification cut", ctx);

    if (rule == rule_month_close) {
        const std::chrono::year_month month(covered.year(), covered.month());
        return settle(matching(candidates, rule, last_business_day_of_month(month)),
                      "month end cut",
                      ctx);
    }

    if (rule == rule_year_close)
        return settle(matching(candidates, rule, last_business_day_of_year(covered.year())),
                      "year end cut",
                      ctx);

    if (rule == rule_same_day_close) {
        // The body is the same one on every end of day occasion, so a report of
        // the close prices from the month end or the year end cut of its date
        // where that date is one. The most specific occasion wins and the plain
        // close is the fallback.
        const std::string looked_for = "end of day cut";

        if (const auto year_end = last_business_day_of_year(covered.year()); covered == year_end) {
            auto matches = matching(candidates, rule_year_close, covered);
            if (!matches.empty())
                return settle(matches, looked_for, ctx);
        }
        if (const std::chrono::year_month month(covered.year(), covered.month());
            covered == last_business_day_of_month(month)) {
            auto matches = matching(candidates, rule_month_close, covered);
            if (!matches.empty())
                return settle(matches, looked_for, ctx);
        }
        return settle(matching(candidates, rule_same_day_close, covered), looked_for, ctx);
    }

    return refuse("The cut kind names the resolution rule " + std::string(rule) +
                  ", which no rule the component knows executes. A kind whose occasion no "
                  "rule resolves names " +
                  std::string(rule_the_cut_named) + " and a report reaches its cuts by pinning one.");
}

cut_resolution mint_live(const domain::cut_kind& kind, const cut_resolution_context& ctx) {
    if (!ctx.preview) {
        std::string missing;
        const auto note = [&missing](const char* part) {
            if (!missing.empty())
                missing += " and ";
            missing += part;
        };
        if (!ctx.body)
            return refuse("A " + kind.code + " body cannot be fixed at " +
                          instant_text(ctx.run_instant) + ": nothing in the request states the "
                          "body the assembly built.");
        if (!ctx.body->market_data_snapshot_id)
            note("the market data snapshot");
        if (!ctx.body->deal_population_id)
            note("the deal population");
        if (!ctx.body->reference_data_version)
            note("the reference data version");
        if (!ctx.body->engine_version)
            note("the engine version");
        if (!ctx.body->code_version)
            note("the code version");
        if (!missing.empty())
            return refuse("A " + kind.code + " body cannot be fixed at " +
                          instant_text(ctx.run_instant) + ": " + missing +
                          " could not be assembled.");
    }

    cut_take_plan plan;
    plan.kind_code = kind.code;
    plan.scope_code = ctx.scope_code;
    plan.business_centre_code = ctx.business_centre_code;
    plan.party_id = ctx.party_id;
    plan.timepoint_timezone = ctx.timepoint_timezone;
    plan.timepoint = ctx.run_instant;
    plan.taken_by = ctx.taken_by;
    plan.taken_at = ctx.run_instant;
    if (ctx.body) {
        plan.market_data_snapshot_id = ctx.body->market_data_snapshot_id;
        plan.deal_population_id = ctx.body->deal_population_id;
        plan.reference_data_version = ctx.body->reference_data_version;
        plan.engine_version = ctx.body->engine_version;
        plan.code_version = ctx.body->code_version;
    }
    plan.change_reason_code = "system.new_record";
    plan.change_commentary = "Minted at the instant the run started";

    cut_resolution out;
    out.mint = std::move(plan);
    return out;
}

cut_resolution resolve_pinned(const domain::cut_kind& kind,
                              const std::optional<boost::uuids::uuid>& cut_id,
                              const std::vector<candidate>& candidates,
                              const cut_resolution_context& ctx) {
    const auto id = boost::uuids::to_string(*cut_id);
    std::vector<candidate> matches;
    for (const auto& c : candidates) {
        if (boost::uuids::to_string(c.cut->id) != id)
            continue;
        if (c.cut->kind_code != kind.code)
            return refuse("The cut " + id + " is of kind " + c.cut->kind_code +
                          ", and the selection names kind " + kind.code + ".");
        if (c.cut->scope_code != ctx.scope_code)
            return refuse("The cut " + id + " fixes scope " + c.cut->scope_code +
                          ", and the report covers scope " + ctx.scope_code + ".");
        matches.push_back(c);
    }

    if (matches.empty())
        return refuse("No cut " + id + " is held for scope " + ctx.scope_code + ".");

    const auto& c = matches.front();
    const auto state = derive_state(*c.cut, 0);
    if (!is_usable(state))
        return refuse("The cut " + cut_label_of(*c.cut, *c.kind) + " is " +
                      std::string(to_string(state)) + " and cannot be priced from.");

    cut_resolution out;
    out.cut = *c.cut;
    return out;
}

}

std::chrono::year_month_day last_business_day_of_month(std::chrono::year_month month) {
    auto date = std::chrono::year_month_day(month.year() / month.month() / std::chrono::last);
    while (!is_weekday(date))
        date = std::chrono::year_month_day(std::chrono::sys_days(date) - std::chrono::days(1));
    return date;
}

std::chrono::year_month_day last_business_day_of_year(std::chrono::year year) {
    return last_business_day_of_month(year / std::chrono::December);
}

cut_resolution resolve_cut_selection(const cut_selection& selection,
                                     const std::vector<domain::cut_kind>& kinds,
                                     const std::vector<domain::cut>& cuts,
                                     const cut_resolution_context& ctx) {
    const auto kind_it = std::find_if(
        kinds.begin(), kinds.end(), [&selection](const auto& k) { return k.code == selection.kind_code; });
    if (kind_it == kinds.end())
        return refuse("The cut kind " + selection.kind_code + " is not held.");

    std::map<std::string, const domain::cut_kind*> kinds_by_code;
    for (const auto& kind : kinds)
        kinds_by_code.insert_or_assign(kind.code, &kind);

    std::vector<candidate> candidates;
    candidates.reserve(cuts.size());
    for (const auto& cut : cuts) {
        const auto it = kinds_by_code.find(cut.kind_code);
        if (it != kinds_by_code.end())
            candidates.push_back(candidate{&cut, it->second});
    }

    if (selection.cut_id)
        return resolve_pinned(*kind_it, selection.cut_id, candidates, ctx);

    const auto rule = kind_it->resolution_rule_code;
    if (rule == rule_run_instant)
        return mint_live(*kind_it, ctx);

    if (rule == rule_the_cut_named)
        return refuse("The cut kind " + kind_it->code +
                      " names no rule that resolves a date, so a report reaches its cuts by "
                      "pinning one and this report pins none.");

    return resolve_by_rule(rule, candidates, ctx);
}

}
