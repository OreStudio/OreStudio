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
#ifndef ORES_REFDATA_API_DOMAIN_IR_CURVE_BOOTSTRAP_PILLAR_HPP
#define ORES_REFDATA_API_DOMAIN_IR_CURVE_BOOTSTRAP_PILLAR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One (tenor, curve-role) pillar of an IR curve bootstrap config's recipe.
 *
 * One row of a bootstrap config's pillar list: which tenor period,
 * priced/bootstrapped as which curve role (deposit-equivalent,
 * FRA-equivalent, swap-equivalent -- see
 * [[id:D9A9B2C5-2C22-456A-851F-B458E0568CD3][Curve Role]]), in what order. Same
 * one-parent-many-children shape ir_curve_template_entry uses for ores.synthetic's generation
 * recipe, but source-independent: this names the pillar list a
 * bootstrap reads from the raw instrument grid, not a generation
 * recipe. start_tenor_code/end_tenor_code are soft (undeclared)
 * tenor references, matching ir_curve_template_entry's own tenor
 * columns exactly -- tenor validation happens through the
 * tenor_resolution machinery at the application layer, not a
 * per-consumer DB-level FK.
 */
struct ir_curve_bootstrap_pillar final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this pillar row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this pillar's config belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent ir_curve_bootstrap_config this pillar contributes to.
     */
    boost::uuids::uuid bootstrap_config_id;

    /**
     * @brief Zero-based ordinal of this pillar within its parent's list (short-end to long-end).
     */
    int sequence_index = 0;

    /**
     * @brief Tenor label marking the start of this pillar's period (references tenor.code). 'SPOT'
     * for point instruments (deposits, swaps); the period's own front tenor (e.g. '3M') for
     * interval instruments (FRAs) -- same convention as ir_curve_template_entry.
     */
    std::string start_tenor_code;

    /**
     * @brief Tenor label marking the end (maturity) of this pillar's period (references tenor.code,
     * e.g. "1M", "2Y").
     */
    std::string end_tenor_code;

    /**
     * @brief Instrument-pricing role this tenor bootstraps as (references curve_role.code: DEPOSIT,
     * FRA, or SWAP -- see [[id:D9A9B2C5-2C22-456A-851F-B458E0568CD3][Curve Role]]) -- determines
     * which inverse-pricing derivation the bootstrapping engine applies at this pillar.
     */
    std::string curve_role_code;

    /**
     * @brief Username of the person who last modified this IR curve bootstrap pillar.
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
 * @brief Dispatch-key identifier for ir_curve_bootstrap_pillar, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ir_curve_bootstrap_pillar&) {
    return "ores.refdata.ir_curve_bootstrap_pillar";
}

}

#endif
