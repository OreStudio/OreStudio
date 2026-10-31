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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_RESOLUTION_PLAN_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_RESOLUTION_PLAN_HPP

#include "ores.reporting.api/domain/cut.hpp"
#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/cut_take_plan.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief What a report selects: a kind of cut, or a kind and one specific body.
 *
 * The pair is one fact. A definition that names a kind says *value this scope
 * against the close of its day*; a definition that also names a cut says *price
 * from this body*. The second is how a report reaches an occasion no rule
 * resolves, such as a flash cut.
 */
struct cut_selection {
    std::string kind_code;
    std::optional<boost::uuids::uuid> cut_id;
};

/**
 * @brief The four inputs the assembly fixed, for a body the run mints itself.
 *
 * Empty members state parts the assembly could not fill. They are what a LIVE
 * run's refusal names, because a LIVE result that could not be reproduced would
 * be a number with a claim it cannot meet.
 */
struct cut_body {
    std::optional<boost::uuids::uuid> market_data_snapshot_id;
    std::optional<boost::uuids::uuid> deal_population_id;
    std::optional<std::string> reference_data_version;
    std::optional<std::string> engine_version;
    std::optional<std::string> code_version;
};

/**
 * @brief The facts the resolution reads besides the selection and the store.
 *
 * The covered date is the date the report covers and not the cut's: it comes
 * from the run settings, and it says which cut to look for. The scope and the
 * centre come from the report, and the run instant is the clock of the run that
 * asks, which is what a LIVE selection fixes its body at.
 */
struct cut_resolution_context {
    /** @brief The date the report covers, as midnight UTC of that date. */
    std::chrono::system_clock::time_point covered_date;

    std::string scope_code;
    std::string business_centre_code;

    /** @brief The party whose books and market data the resolved cut fixes. */
    boost::uuids::uuid party_id;

    /** @brief The IANA clock the centre's instant is stated in. */
    std::string timepoint_timezone = "UTC";

    /** @brief The instant the run starts, for a kind whose body the run mints. */
    std::chrono::system_clock::time_point run_instant;

    /**
     * @brief The account the run acts as, written on a body it mints.
     *
     * Nobody presses *Take a cut* for a LIVE selection, so the taking identity
     * is the run's account and the cut's page names a system account.
     */
    std::string taken_by;

    /**
     * @brief What the assembly fixed, for a body the run mints.
     *
     * Absent on a preview, which mints nothing, and present on a run, which
     * must state every part or be refused with the parts it could not fill.
     */
    std::optional<cut_body> body;

    /**
     * @brief Whether this resolution answers a preview rather than a run.
     *
     * A preview mints no body and refuses no part: it says what the selection
     * would resolve to right now, so a person sees a stale choice before a run
     * discovers it. A run requires the body a mint needs.
     */
    bool preview = false;
};

/**
 * @brief What a selection resolved to: a body, a body to mint, or a refusal.
 *
 * Exactly one of the three holds. A mint is not a cut yet: the caller writes the
 * row from @ref cut_take_plan, which is where the kind's retention is copied
 * onto the body.
 */
struct cut_resolution {
    /** @brief The body the selection resolved to, where the store holds it. */
    std::optional<domain::cut> cut;

    /** @brief The body the caller must mint, for a kind whose instant it supplies. */
    std::optional<cut_take_plan> mint;

    /**
     * @brief One sentence saying why nothing resolved, empty when something did.
     *
     * The same sentence serves the preview, the report's page and the run's
     * output message, so a scheduled run nobody watched still says why it did
     * not price.
     */
    std::string refusal;

    [[nodiscard]] bool ok() const { return cut.has_value() || mint.has_value(); }
};

/**
 * @brief The last Monday-to-Friday day of a month.
 *
 * Holiday calendars are not consulted, as the valuation date of a run does not
 * consult them: the design fixes the occasion's instant, and a centre's holiday
 * calendar is a later refinement of it.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::chrono::year_month_day
last_business_day_of_month(std::chrono::year_month month);

/**
 * @brief The last Monday-to-Friday day of a year.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::chrono::year_month_day
last_business_day_of_year(std::chrono::year year);

/**
 * @brief Resolves a report's cut selection to one body.
 *
 * The rule is the kind's, read live at the moment of the resolution, because a
 * report resolving today must follow today's rule and not the rule in force when
 * the report was defined. The rules are:
 *
 * | Rule | Resolves to |
 * |------+-------------|
 * | =same_day_close= | the end of day family's cut of the covered date, the year end or month end cut where that date is one and the plain close otherwise |
 * | =last_business_day_of_month_close= | the month end cut of the covered month |
 * | =last_business_day_of_year_close= | the year end cut of the covered year |
 * | =same_day_open= | the start of day cut of the covered date |
 * | =cut_it_is_built_on= | the verification cut of the covered date |
 * | =run_instant= | a body the caller mints at the run's instant |
 * | =the_cut_named= | the cut the selection pins, and nothing else |
 *
 * Candidate kinds are matched by the rule they name and not by their code, so a
 * firm that seeds its own month end kind needs no change here.
 *
 * A candidate offers a body only where its state is usable: a cut that waits on
 * an authorisation, or one that was refused, is not a body a run may price from,
 * and the refusal names the request it waits on.
 *
 * @param selection What the report selects.
 * @param kinds Every cut kind the store holds, for the rules the candidates name.
 * @param candidates The cuts of the report's scope and centre in the covered
 * year, as the store answered.
 * @param ctx The covered date, the scope, the centre and the run's instant.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT cut_resolution
resolve_cut_selection(const cut_selection& selection,
                      const std::vector<domain::cut_kind>& kinds,
                      const std::vector<domain::cut>& candidates,
                      const cut_resolution_context& ctx);

}

#endif
