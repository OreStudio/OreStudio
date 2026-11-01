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
#include "ores.reporting.core/service/cut_assembly.hpp"
#include <format>
#include <string>
#include <utility>

namespace ores::reporting::service {

namespace {

using ores::reporting::messaging::cut_body_part;

/** The part codes the body holds, which are the codes a gap may name. */
constexpr std::string_view kind_part = "kind";
constexpr std::string_view deals_part = "deal_population";
constexpr std::string_view market_data_part = "market_data";
constexpr std::string_view reference_data_part = "reference_data";
constexpr std::string_view engine_part = "engine_code";

/** A part the assembly filled, with what it resolved and what it cannot state. */
cut_body_part
closed(std::string code, std::string summary, std::string reference, std::string note = {}) {
    cut_body_part part;
    part.part_code = std::move(code);
    part.state = "closed";
    part.summary = std::move(summary);
    part.reference = std::move(reference);
    part.snapshot_note = std::move(note);
    return part;
}

/** A part the assembly could not fill, with the item, the reason and the remedy. */
cut_body_part gapped(std::string code, std::string item, std::string reason, std::string remedy) {
    cut_body_part part;
    part.part_code = std::move(code);
    part.state = "gapped";
    part.item = std::move(item);
    part.reason = std::move(reason);
    part.remedy = std::move(remedy);
    return part;
}

} // namespace

cut_assembly assemble_cut_body(const domain::cut_kind& kind, const cut_body_facts& facts) {
    cut_assembly out;

    // The kind is the one fact the take cannot proceed without, because the
    // retention and the authorisation basis are copied from it. A take that
    // named a kind the store does not hold never reaches here.
    out.parts.push_back(
        closed(std::string(kind_part),
               std::format("{} · {}{}",
                           kind.code,
                           kind.name,
                           kind.approval_kind_code ? " · signed by " + *kind.approval_kind_code :
                                                     " · no authorisation"),
               {},
               std::format("kept {} months in the {} tier, copied onto the cut at the take",
                           kind.retention_months,
                           kind.retention_destination_code)));

    // The deals: the population the store held at the instant, by count. The
    // store holds no copy of a population and no read materialises one, so the
    // count is what the body records and the note says so.
    if (facts.deal_count && *facts.deal_count > 0) {
        out.deal_count = *facts.deal_count;
        out.parts.push_back(closed(
            std::string(deals_part),
            std::format("{} deals held at the instant", *facts.deal_count),
            {},
            "the store answers the population at the instant and holds no copy of it, so the "
            "body records the count and no population reference"));
    } else {
        const auto reason =
            facts.deals_refusal.empty() ?
                std::string("The store holds no deal at the instant, so the body would fix an "
                            "empty population.") :
                facts.deals_refusal;
        out.parts.push_back(gapped(std::string(deals_part),
                                   "the deal population",
                                   reason,
                                   "Correct the deal population, or state an instant at which "
                                   "the store holds one."));
        out.gaps.push_back({std::string(deals_part), "the deal population", reason});
    }

    // The market data: the published sets in use at the instant. A candidate
    // nobody approved is the remedy the journey names, so it is a gap of its
    // own rather than a note: a valuation cannot price from an unapproved set.
    if (facts.market_data_set_count && *facts.market_data_set_count > 0) {
        out.market_data_set_count = *facts.market_data_set_count;
        out.parts.push_back(closed(
            std::string(market_data_part),
            facts.market_data_as_of.empty() ?
                std::format("{} published sets in use", *facts.market_data_set_count) :
                std::format("{} published sets in use, newest as of {}",
                            *facts.market_data_set_count,
                            facts.market_data_as_of),
            {},
            "nothing in the store resolves every market role to one fixed snapshot, so the body "
            "names the sets in use and no snapshot reference"));
    } else if (facts.market_data_candidate_count > 0) {
        const auto reason =
            std::format("The store holds {} candidate set(s) and no published set in use, so a "
                        "valuation cannot price from the body.",
                        facts.market_data_candidate_count);
        out.parts.push_back(gapped(std::string(market_data_part),
                                   "a curve in the candidate state",
                                   reason,
                                   "Ask for a publish to be approved."));
        out.gaps.push_back(
            {std::string(market_data_part), "a curve in the candidate state", reason});
    } else {
        const auto reason =
            facts.market_data_refusal.empty() ?
                std::string("The store holds no published set in use at or before the instant, so "
                            "the snapshot would carry a hole.") :
                facts.market_data_refusal;
        out.parts.push_back(gapped(std::string(market_data_part),
                                   "a series with no value at or before the instant",
                                   reason,
                                   "Publish a set for the series, or state an instant at which "
                                   "the store holds one."));
        out.gaps.push_back({std::string(market_data_part),
                            "a series with no value at or before the instant",
                            reason});
    }

    // The reference data: the versions in force of the configuration the run
    // reads.
    if (facts.reference_data_version) {
        out.parts.push_back(closed(std::string(reference_data_part),
                                   *facts.reference_data_version,
                                   *facts.reference_data_version));
    } else {
        const auto reason =
            facts.reference_data_refusal.empty() ?
                std::string("Nothing names the configuration in force at the instant, so the body "
                            "would record no settings.") :
                facts.reference_data_refusal;
        out.parts.push_back(gapped(std::string(reference_data_part),
                                   "the configuration in force",
                                   reason,
                                   "Bind a configuration to a report, or name the version the run "
                                   "will use."));
        out.gaps.push_back(
            {std::string(reference_data_part), "the configuration in force", reason});
    }

    // The engine and the code. The engine version is required: a body that
    // fixed the data and omitted the engine holds a body its own result does
    // not follow from. The code revision is not: nothing in the store records
    // the build of the services that assembled the body, so the part states
    // that as its note rather than refusing a body over a fact the platform
    // does not keep.
    if (facts.engine_version) {
        out.parts.push_back(closed(
            std::string(engine_part),
            facts.wrapper_version.empty() ?
                std::format("engine {}", *facts.engine_version) :
                std::format("engine {}, wrapper {}", *facts.engine_version, facts.wrapper_version),
            facts.code_version.value_or(std::string{}),
            facts.code_version ?
                std::string{} :
                std::string("nothing records the revision of the services that assembled the "
                            "body, so the part names the engine and the wrapper and no code "
                            "revision")));
    } else {
        const auto reason =
            facts.engine_refusal.empty() ?
                std::string("No engine version recorded: nothing in the store names the version "
                            "that would compute the result.") :
                facts.engine_refusal;
        out.parts.push_back(gapped(std::string(engine_part),
                                   "the engine version",
                                   reason,
                                   "Register the app version, or name the version the run will "
                                   "use."));
        out.gaps.push_back({std::string(engine_part), "the engine version", reason});
    }

    return out;
}

}
