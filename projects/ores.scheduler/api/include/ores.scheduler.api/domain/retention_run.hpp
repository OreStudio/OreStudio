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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SCHEDULER_API_DOMAIN_RETENTION_RUN_HPP
#define ORES_SCHEDULER_API_DOMAIN_RETENTION_RUN_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::scheduler::domain {

/**
 * @brief One retention policy the sweep applied, with what it removed and when.
 *
 * The read model a person reads to see what housekeeping did: one row for each
 * policy the sweep considered, carrying the policy's period and whether it is on,
 * the store it bounds, what the run removed and its outcome.
 *
 * The sweep writes the rows and this view reads them. It is a view because the
 * ledger answers "what has housekeeping done" and a screen asks it beside the
 * policies themselves: the period a policy states now is the policy table's, not
 * the ledger's, and joining the two is the view's whole purpose. A policy
 * disabled after a run still shows the run, with the state it has now.
 */
struct retention_run final {
    /**
     * @brief The ledger row this view stands for.
     */
    boost::uuids::uuid id;

    /**
     * @brief The policy the sweep applied or skipped.
     */
    std::string policy_code;

    /**
     * @brief The store the policy bounds: a schema qualified table name, or a key prefix.
     */
    std::string target;

    /**
     * @brief What the sweep did to the store: drop its chunks, delete its rows, or expire its
     * objects.
     */
    std::string action;

    /**
     * @brief The period the policy states now, in days.
     */
    int retention_days = 0;

    /**
     * @brief Whether the policy is on now. A policy turned off after a run keeps its rows, so a
     * person sees that it ran and that it no longer will.
     */
    bool enabled = false;

    /**
     * @brief When the sweep considered this policy.
     */
    std::chrono::system_clock::time_point ran_at;

    /**
     * @brief The chunks or rows the run removed. A policy that removed nothing is a policy worth
     * looking at.
     */
    std::int64_t removed{};

    /**
     * @brief What happened: applied, skipped or failed.
     */
    std::string outcome;

    /**
     * @brief Why the run did not apply, when it did not.
     */
    std::string detail;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const retention_run&, const retention_run&) = default;
};

/**
 * @brief Dispatch-key identifier for retention_run, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const retention_run&) {
    return "ores.scheduler.retention_run";
}

}

#endif
