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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_ASSEMBLY_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_ASSEMBLY_HPP

#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief One part of a body that the assembly could not fill.
 *
 * A gap is recorded before it is a row: the take writes it with the cut it
 * belongs to, and the cut's id is the take's to mint, so the assembly answers
 * the part, the item and the reason and the take stamps the identity.
 */
struct cut_gap_candidate {
    /** @brief One of the four parts the gap check admits. */
    std::string part_code;

    /** @brief The item inside the part that could not be filled. */
    std::string item;

    /** @brief Why it could not be filled, in the words the person reads. */
    std::string reason;
};

/**
 * @brief What the store answered about one body, part by part.
 *
 * Every member is what a read answered, and an absent member is a read that
 * answered nothing rather than an omission of the caller: the assembly turns
 * each absence into a gap with the reason the read gave, so a person reads what
 * stands rather than a body that looks smaller than it is.
 *
 * The references are absent where the store can answer the part and materialise
 * no reference for it -- no copy of a population, no fixed snapshot, no code
 * revision. That is stated as the part's note and not as a gap, because the
 * body is answered and the design's own gap table does not name it: a body that
 * could not be taken for want of a reference the platform does not keep would
 * make every hand take impossible, and the design refuses a take on a gap.
 */
struct cut_body_facts {
    /** @brief How many deals the store held at the instant, or nothing. */
    std::optional<int> deal_count;

    /** @brief Why the deal population read answered nothing, where it did not. */
    std::string deals_refusal;

    /** @brief How many published sets were in use at the instant, or nothing. */
    std::optional<int> market_data_set_count;

    /** @brief The newest published instant among the sets in use, or empty. */
    std::string market_data_as_of;

    /** @brief How many candidate sets stand beside the sets in use. */
    int market_data_candidate_count = 0;

    /** @brief Why the published sets read answered nothing, where it did not. */
    std::string market_data_refusal;

    /** @brief The version of the reference data in force, or nothing. */
    std::optional<std::string> reference_data_version;

    /** @brief Why the reference data read answered nothing, where it did not. */
    std::string reference_data_refusal;

    /** @brief The engine version the body is assembled for, or nothing. */
    std::optional<std::string> engine_version;

    /** @brief The wrapper version the work will run, or empty. */
    std::string wrapper_version;

    /** @brief Why the engine read answered nothing, where it did not. */
    std::string engine_refusal;

    /** @brief The revision of the code that assembled the body, or nothing. */
    std::optional<std::string> code_version;
};

/**
 * @brief The body one assembly answered: every part with its state, and every
 * gap the take records.
 */
struct cut_assembly {
    /** @brief Every part of the body, in the order a person reads them. */
    std::vector<ores::reporting::messaging::cut_body_part> parts;

    /** @brief The parts the assembly could not fill. */
    std::vector<cut_gap_candidate> gaps;

    /** @brief The parts the assembly filled, as the counts the take records. */
    int deal_count = 0;
    int market_data_set_count = 0;

    /** @brief Whether a take may fix the body: no gap stands and a part answered. */
    [[nodiscard]] bool complete() const { return gaps.empty() && !parts.empty(); }
};

/**
 * @brief Answers every part of a body from what the store answered about it.
 *
 * The rule is one sentence: a part is closed when a read answered it, and a gap
 * stands with the item, the reason and the remedy the person chases when the
 * read answered nothing. The kind is always closed -- a take of an unknown kind
 * is refused before it reaches here -- and so is the taking identity, which the
 * take itself is.
 *
 * Nothing about the store is read here: the facts are the caller's, so the rule
 * that a gap stands is stated once and tested without a database.
 *
 * @param kind The kind the body would be taken under, as the store holds it.
 * @param facts What each read answered.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT cut_assembly
assemble_cut_body(const domain::cut_kind& kind, const cut_body_facts& facts);

}

#endif
