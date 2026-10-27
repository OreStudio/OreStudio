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
#ifndef ORES_ANALYTICS_API_DOMAIN_SENSITIVITY_ASSET_CLASS_HPP
#define ORES_ANALYTICS_API_DOMAIN_SENSITIVITY_ASSET_CLASS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief The asset classes a sensitivity template belongs to, the prefix ORE presents each with,
 * and the words a person reads.
 *
 * A sensitivity template key names an asset class, and the class is the reference
 * data's: interest_rates, fx, equity, commodity or credit. The key spells
 * that class as a short prefix a person recognises -- IR, FX, EQ, COMM,
 * CR -- and the two spellings are not the same, because ORE's template keys grew
 * their own vocabulary (it spells commodity COMM where the rest of ORE spells it
 * COM).
 *
 * So this table is the bridge. It holds one row per class ORE presents a template
 * prefix for: the reference data code the template points at, the prefix the key
 * spells, and the words a person reads. Bond and inflation are asset classes of the
 * reference data with no row here, because ORE's examples present no template prefix
 * for them; a tenant that needs one adds the row.
 *
 * The prefix is a presentation and never the identity: a key's spelling is the
 * tenant's, ORE resolves the key a configuration states, and the template's
 * asset_class_code is what the table checks. A screen that shows the class shows
 * the name from here, and the prefix stands beside it as what the key spells.
 *
 * The rows are seeded for the system tenant and read by every tenant.
 */
struct sensitivity_asset_class final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The reference data's asset class code: 'interest_rates', 'fx', 'equity', 'commodity'
     * or 'credit'.
     *
     * Soft FK to ores_refdata_asset_class_codes_tbl.
     */
    std::string asset_class_code;

    /**
     * @brief The prefix ORE spells in a template key for this class: 'IR', 'FX', 'EQ', 'COMM' or
     * 'CR'.
     */
    std::string template_prefix;

    /**
     * @brief The class in the words a person reads: 'Interest rate', 'Foreign exchange', 'Equity',
     * 'Commodity' or 'Credit'.
     */
    std::string name;

    /**
     * @brief What the class means, in one line, for a reader who does not know ORE.
     */
    std::string description;

    /**
     * @brief The order the classes are offered in, which is the order ORE's own documentation lists
     * them in.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this sensitivity asset class.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

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
    friend bool operator==(const sensitivity_asset_class&,
                           const sensitivity_asset_class&) = default;
};

/**
 * @brief Dispatch-key identifier for sensitivity_asset_class, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const sensitivity_asset_class&) {
    return "ores.analytics.sensitivity_asset_class";
}

}

#endif
