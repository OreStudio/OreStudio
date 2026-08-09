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
#ifndef ORES_REFDATA_API_DOMAIN_IR_CURVE_BOOTSTRAP_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_IR_CURVE_BOOTSTRAP_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The recipe that bootstraps one raw instrument grid into a discount curve -- source series,
 * family role, build order, interpolation, day-count, split tenor.
 *
 * Records *how* a curve is bootstrapped as a named, inspectable
 * artefact -- the "curve template" concept
 * [[id:A26CFA71-21C0-4E98-ABC4-25F0EAD517E3][Multicurve Management]] calls for. Owned by
 * ores.refdata, matching where every other recipe/config entity lives in this codebase
 * (crm_topology_config, curve_role, tenor), not ores.marketdata,
 * which owns only the generic value store the bootstrap output is
 * written into.
 *
 * source_series_id and output_series_id are soft, cross-component
 * references into ores.marketdata's market_series table (the raw
 * instrument grid this config bootstraps, and the official curve series
 * it publishes into) -- deliberately not hard FK constraints, matching
 * the same soft-reference principle already used for
 * market_series.derivation_config_id and ir_curve_tick's own
 * producer/config identity: the referenced table lives in a different
 * component/schema. output_series_id is minted (the market_series
 * catalog row created) at config-creation time by the owning service,
 * never left null/deferred -- there is no "not yet published" state to
 * guard against.
 *
 * curve_family_role (FUNDING/PROJECTION) and
 * discount_curve_config_id (self-referencing, nil-uuid sentinel for
 * FUNDING) encode
 * [[id:7CB0024B-84FB-4AE0-ADF2-763079E888D5][Multi-Curve Construction]]'s Funding-before-Projection
 * build-order dependency as data: a PROJECTION config's discount_curve_config_id must point at a
 * FUNDING config (strict two-tier, no chaining -- Multi-Curve Construction names
 * basis-linked/cyclic Projection dependencies as an explicit out-of-scope modelling gap, not
 * something this design should silently permit), and must not reference itself.
 *
 * source_series_id (the raw grid this config bootstraps from) and
 * output_series_id (the published curve it writes to) must differ.
 * Without this, curve_republish_service's first-publish step would
 * permanently reclassify the raw, externally-fed input series as this
 * config's own IR_CURVE_BOOTSTRAP-derived output, and every
 * subsequent bootstrapped observation would be written into the exact
 * series id the raw feed keeps writing ticks into -- silently
 * corrupting the raw data rather than failing loudly.
 *
 * interpolation_method and curve_family_role are small,
 * fixed-vocabulary fields intrinsic to this record (not references to
 * another entity), following the same plain-check-constraint pattern
 * ir_curve_generation_config.role's 'self_discounting'/discount/
 * projection vocabulary already establishes -- no lookup table for a
 * handful of values with no independent lifecycle of their own.
 * day_count_convention references the existing
 * day_count_fraction_type reference table. split_tenor_code
 * references tenor.code, following the same soft (undeclared,
 * documentation-only) tenor reference ir_curve_template_entry's own
 * start_tenor_code/end_tenor_code already use -- tenor validation
 * happens through the tenor_resolution machinery at the application
 * layer, not a per-consumer DB-level FK, consistent with that sibling
 * entity. For a single-segment interpolation_method, split_tenor_code
 * is a genuine value (the curve's own last pillar's end_tenor_code),
 * not a fabricated sentinel -- matching the spirit of
 * ir_curve_template_entries's own 'SPOT' tenor, which its own doc
 * comment is explicit isn't a sentinel hack either.
 */
struct ir_curve_bootstrap_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this bootstrap config.
     */
    boost::uuids::uuid id;

    /**
     * @brief Soft reference to the official, bootstrapped-curve market_series (in ores.marketdata)
     * this config publishes into -- minted at config-creation time, never deferred. The sole
     * natural key: exactly one active bootstrap config, tenant-wide, may produce any given output
     * series -- not merely one per party, since two different parties must not each be able to
     * claim the same output series.
     */
    boost::uuids::uuid output_series_id;

    /**
     * @brief Owning party (legal entity) this bootstrap config belongs to. Set server-side from the
     * authenticated session, enforced by RLS. Deliberately *not* part of the natural key below:
     * uniqueness on output_series_id is enforced tenant-wide, not per-party -- two different
     * parties must not each be able to hold an active config claiming the same output series.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Soft reference to the raw RATES/YIELD market_series (in ores.marketdata) this config
     * bootstraps.
     */
    boost::uuids::uuid source_series_id;

    /**
     * @brief Whether this config bootstraps the discounting anchor curve of its family (FUNDING) or
     * a dependent curve discounted off one (PROJECTION) -- see
     * [[id:6EA42D11-94F4-4D3A-9A51-024EC56EFC44][Funding and Projection Curves]]. Fixed 2-value
     * vocabulary intrinsic to this record, not a reference to another entity -- no soft FK,
     * validated by a plain SQL check.
     */
    std::string curve_family_role = "FUNDING";

    /**
     * @brief Self-referencing: the FUNDING config this config's own pillars discount off during
     * bootstrap, when this config's own curve_family_role is PROJECTION -- the
     * ores_utility_nil_uuid_fn() sentinel when it is FUNDING instead. Strict two-tier only
     * (validated in the insert trigger, not just shape-checked): the referenced config must itself
     * have a curve_family_role of FUNDING, and must not equal this config's own id.
     */
    boost::uuids::uuid discount_curve_config_id = boost::uuids::nil_uuid();

    /**
     * @brief Interpolation method for discount factors between pillars (LOG_LINEAR_DISCOUNT,
     * CUBIC_SPLINE, or the two-segment FLAT_FORWARD_THEN_LOG_LINEAR the FOMC-dated short-end task
     * uses). Fixed vocabulary intrinsic to this record, not a reference to another entity.
     */
    std::string interpolation_method = "LOG_LINEAR_DISCOUNT";

    /**
     * @brief References day_count_fraction_type.code (e.g. A360, A365) -- the day-count convention
     * applied per pillar during bootstrap.
     */
    std::string day_count_convention;

    /**
     * @brief References tenor.code -- the pillar tenor at which this curve transitions from a
     * short-end interpolation segment to a continuous long-end one. For a single-segment
     * interpolation_method, this equals the curve's own last pillar's end_tenor_code (a genuine
     * value, not a sentinel). Soft reference, undeclared at the DB level -- matching
     * ir_curve_template_entry's own start_tenor_code/ end_tenor_code tenor references, validated
     * through the tenor_resolution machinery at the application layer.
     */
    std::string split_tenor_code;

    /**
     * @brief Username of the person who last modified this IR curve bootstrap config.
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
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for ir_curve_bootstrap_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ir_curve_bootstrap_config&) {
    return "ores.refdata.ir_curve_bootstrap_config";
}

}

#endif
