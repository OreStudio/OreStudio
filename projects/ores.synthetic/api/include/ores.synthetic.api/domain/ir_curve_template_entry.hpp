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
#ifndef ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_TEMPLATE_ENTRY_HPP
#define ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_TEMPLATE_ENTRY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief One (tenor, instrument) entry of an IR Curve Template.
 *
 * One row of the raw instrument grid (the "Curve Template") an
 * ir_curve_generation_config publishes: which tenor period, priced as
 * which instrument type (deposit-equivalent, FRA-equivalent,
 * swap-equivalent -- see ores.refdata.instrument_code), in what order.
 * Every entry is modelled as a genuine [start, end) period rather than
 * a single maturity label: start_tenor_code/end_tenor_code are both
 * ordinary tenor references resolved through the same
 * ores::refdata::domain::resolve_window/resolve_end_date machinery
 * (see ores.refdata.api/domain/tenor_resolution.hpp). Point instruments
 * (deposits, swaps) set start_tenor_code to 'SPOT' (a genuine
 * zero-duration PERIOD/DAY tenor already in the catalog, resolving
 * directly to the horizon's spot date -- not a null/sentinel hack);
 * interval instruments (FRAs) set it to the period's own front tenor
 * (e.g. '3M' for a 3x6 FRA whose end_tenor_code is '6M'). This
 * symmetric shape is what lets the tenor-collision validator
 * (validate_curve_template in ores.synthetic.api) detect genuine
 * period overlaps via plain windows_overlap(), without a special case
 * for point vs. interval instruments. Entries belong to a parent config
 * via ir_curve_config_id, the same one-config-many-children shape
 * fx_spot_generation_config's gmm_component rows use. Every entry's
 * published rate is derived from the parent config's short-rate
 * process's discount_factor() at the tenor's maturity -- never an
 * independently-noised value -- so the published tick batch is, by
 * construction, a slice of one internally consistent curve. Party- and
 * tenant-scoped.
 */
struct ir_curve_template_entry final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this template entry.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this entry's configuration belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent ir_curve_generation_config this entry contributes to.
     */
    boost::uuids::uuid ir_curve_config_id;

    /**
     * @brief Zero-based ordinal of this entry within its parent's template (short-end to long-end).
     */
    int sequence_index = 0;

    /**
     * @brief Tenor label marking the start of this entry's period (references tenor.code). 'SPOT'
     * for point instruments (deposits, swaps); the period's own front tenor (e.g. '3M') for
     * interval instruments (FRAs).
     */
    std::string start_tenor_code;

    /**
     * @brief Tenor label marking the end (maturity) of this entry's period (references tenor.code,
     * e.g. "1M", "2Y").
     */
    std::string end_tenor_code;

    /**
     * @brief Instrument type this tenor is priced as (references instrument_code.code, e.g.
     * "ForwardRateAgreement", "Swap") -- determines which pricing derivation applies (simple rate,
     * implied forward, or par-rate solve) when this entry's rate is computed from the parent
     * process's discount_factor().
     */
    std::string instrument_code;

    /**
     * @brief Username of the person who last modified this IR curve template entry.
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
 * @brief Dispatch-key identifier for ir_curve_template_entry, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ir_curve_template_entry&) {
    return "ores.synthetic.ir_curve_template_entry";
}

}

#endif
