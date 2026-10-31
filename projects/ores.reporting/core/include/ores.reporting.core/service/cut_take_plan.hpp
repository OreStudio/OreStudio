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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_TAKE_PLAN_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_TAKE_PLAN_HPP

#include "ores.reporting.api/domain/cut.hpp"
#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief What the caller states when it takes a cut.
 *
 * Every fact here is one only the caller knows: the body it assembled, the
 * identity it acts as, and the instant it acted. The four decisions the take
 * copies from the kind are deliberately absent, because a caller that stated
 * them could state a retention its kind does not promise.
 */
struct cut_take_plan {
    /** @brief The occasion the body was fixed for. */
    std::string kind_code;

    /** @brief The book population and centre the body fixes. */
    std::string scope_code;
    std::string business_centre_code;

    /** @brief The party whose books and market data the cut fixes. */
    boost::uuids::uuid party_id;

    /** @brief The IANA clock the timepoint is stated in. */
    std::string timepoint_timezone;

    /** @brief The instant the body was fixed at. */
    std::chrono::system_clock::time_point timepoint;

    /** @brief The named cut time the instant came from, where one supplied it. */
    std::optional<std::string> cut_time_code;

    /**
     * @brief One reference for each part of the body the assembly filled.
     *
     * A part the assembly could not fill is left empty and is recorded as a
     * gap instead, so an empty member here is a fact about the assembly and not
     * an omission of this plan.
     */
    std::optional<boost::uuids::uuid> market_data_snapshot_id;
    std::optional<boost::uuids::uuid> deal_population_id;
    std::optional<std::string> reference_data_version;
    std::optional<std::string> engine_version;
    std::optional<std::string> code_version;

    /**
     * @brief The account that fixes the body.
     *
     * For a cut the run mints, this is the run's account and the run states
     * itself, because nobody pressed a button to take it.
     */
    std::string taken_by;

    /** @brief When the body was fixed. The take and not the timepoint. */
    std::chrono::system_clock::time_point taken_at;

    std::string change_reason_code;
    std::string change_commentary;
};

/**
 * @brief The cut one take writes, with the kind's decisions copied onto it.
 *
 * The copy is the whole of this function, and it is the point of it: the
 * retention, the destination, the legal hold and whether an authorisation is
 * required are facts about this body, fixed at the take, so a later change to
 * the kind does not move the promise made about a body already in use. The
 * values are read from @p kind and never from the caller.
 *
 * A kind that names no approval kind took a body that requires no
 * authorisation, and the cut records that rather than looking unsigned.
 *
 * @param plan The body, the identity and the instant, as the caller stated them.
 * @param kind The kind the cut is taken under, as the store holds it.
 * @param id The surrogate key to write.
 * @param take_ordinal Which take of this occasion the row is: see
 * @ref next_take_ordinal.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT domain::cut plan_take(const cut_take_plan& plan,
                                                               const domain::cut_kind& kind,
                                                               boost::uuids::uuid id,
                                                               int take_ordinal);

/**
 * @brief Which take of one occasion @p plan is, given the takes already held.
 *
 * A retake of the same kind, scope and timepoint is the second cut of that
 * occasion, and the label states the ordinal so a person reads which body is
 * meant. The candidates are the cuts of the occasion, in any order and of any
 * take; the answer is one more than the highest ordinal among them.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT int
next_take_ordinal(const std::vector<domain::cut>& same_occasion);

}

#endif
