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
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.reporting.core/service/cut_take_plan.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief The resolution rule codes a kind may name, as the design states them.
 *
 * The codes are also rows of the cut resolution rule lookup, which is where a
 * person reads what each one means. These names are what the resolution matches
 * on, so a rule added to the lookup without a branch here is refused by name
 * rather than resolved by accident.
 */
namespace resolution_rules {
inline constexpr std::string_view same_day_close = "same_day_close";
inline constexpr std::string_view last_business_day_of_month_close =
    "last_business_day_of_month_close";
inline constexpr std::string_view last_business_day_of_year_close =
    "last_business_day_of_year_close";
inline constexpr std::string_view same_day_open = "same_day_open";
inline constexpr std::string_view cut_it_is_built_on = "cut_it_is_built_on";
inline constexpr std::string_view run_instant = "run_instant";
inline constexpr std::string_view the_cut_named = "the_cut_named";
}

/**
 * @brief What a report selects: a kind of cut, or a kind and one specific body.
 *
 * A definition that names a kind says *value this scope against the close of its
 * day*; a definition that also names a cut says *price from this body*, which is
 * how a report reaches an occasion no rule resolves.
 */
struct cut_selection {
    std::string kind_code;
    std::optional<boost::uuids::uuid> cut_id;
};

/**
 * @brief The four inputs the assembly fixed, for a body the run mints itself.
 *
 * An empty member states a part the assembly could not fill, which is what a
 * LIVE run's refusal names.
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
 * The covered date is the report's and not the cut's: it says which cut to look
 * for, where the cut states its own instant.
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

    /** @brief The account the run acts as, written on a body it mints. */
    std::string taken_by;

    /**
     * @brief What the assembly fixed, for a body the run mints.
     *
     * Absent on a preview, which mints nothing.
     */
    std::optional<cut_body> body;

    /**
     * @brief Whether this resolution answers a preview rather than a run.
     *
     * A preview answers what the selection would resolve to right now, mints
     * nothing and refuses no part. A run requires the body a mint needs.
     */
    bool preview = false;
};

/**
 * @brief What a selection resolved to: a body, a body to mint, or a refusal.
 *
 * Exactly one of the three holds.
 */
struct cut_resolution {
    /** @brief The body the selection resolved to, where the store holds it. */
    std::optional<domain::cut> cut;

    /** @brief The body the caller must mint, for a kind whose instant it supplies. */
    std::optional<cut_take_plan> mint;

    /**
     * @brief The kind @ref mint was resolved against.
     *
     * Carried here so the caller mints from the kind the rules read, rather than
     * looking it up a second time and minting from a different row.
     */
    std::optional<domain::cut_kind> mint_kind;

    /** @brief The label a person quotes for the resolved cut, where one resolved. */
    std::optional<std::string> label;

    /** @brief Whether a result priced from the resolved body carries the indicative mark. */
    bool output_is_indicative = false;

    /**
     * @brief The state the resolved body is in, derived with supersession applied.
     *
     * The design asks one operation to answer the cut, its state and the sentence
     * that would refuse it, so the state travels with the answer rather than
     * being derived a second time by whatever draws it.
     */
    std::optional<cut_state> state;

    /**
     * @brief One sentence saying why nothing resolved, empty when something did.
     *
     * The same sentence serves the preview, the report's page and the run's
     * output message, so a scheduled run nobody watched still says why it did
     * not price.
     */
    std::string refusal;

    [[nodiscard]] bool ok() const {
        return cut.has_value() || mint.has_value();
    }
};

/**
 * @brief The last Monday-to-Friday day of a month.
 *
 * Holiday calendars are not consulted, as the valuation date of a run does not
 * consult them: the centre's calendar is a later refinement of the occasion.
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
 * The rule is the kind's, read live at the moment of the resolution: a report
 * resolving today must follow today's rule and not the rule in force when the
 * report was defined. Candidate kinds are matched by the rule they name and not
 * by their code, so a firm that seeds its own month end kind needs no change
 * here.
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
