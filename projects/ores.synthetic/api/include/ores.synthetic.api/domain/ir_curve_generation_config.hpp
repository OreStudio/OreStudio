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
#ifndef ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_GENERATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief Configuration for generating a synthetic IR curve from a short-rate process.
 *
 * A typed sub-configuration owned by a market_data_generation_config. Describes
 * how to synthesise a single currency+index interest-rate curve: which
 * short-rate process drives it (vasicek/cir/hull_white, per
 * ores.analytics.quant's IYieldCurveProcess engines) and that process's
 * parameters. The actual instrument grid (which tenors to publish at which
 * role) is held separately as ordered ir_curve_template_entry rows, the
 * same one-config-many-children shape fx_spot_generation_config uses for
 * its gmm_component rows. Scoped to a tenant and a party.
 */
struct ir_curve_generation_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this configuration belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent market_data_generation_config this sub-config belongs to.
     */
    boost::uuids::uuid config_id;

    /**
     * @brief Currency this curve is denominated in (soft FK to currencies).
     */
    std::string currency_code;

    /**
     * @brief Floating-rate index this curve represents, stored as the full ISDA/ORE code
     * (references ores.refdata.floating_index_type.code, e.g. "USD-SOFR", "EUR-ESTR", "GBP-SONIA")
     * rather than a bare suffix ("SOFR") -- floating_index_type.code already bakes in currency, and
     * storing the full code lets this FK reuse the entity's own generated single-argument validator
     * directly (ores_refdata_validate_floating_index_type_fn(tenant_id, value)) instead of a
     * bespoke composite validator the codegen Insert-trigger Validations mechanism can't express
     * (it always calls fn(tenant_id, NEW.column), one column in, one value out).
     * ir_curve_feed_source_name() and display code strip the redundant <currency_code>- prefix back
     * off for the "ir_curve.<ccy>.<idx>" subject and UI labels, so the wire subject/tree text are
     * unaffected by this storage change.
     */
    std::string index_name;

    /**
     * @brief Short-rate process engine driving this curve (references
     * yield_curve_process_type.code: VASICEK, CIR, or HULL_WHITE) -- selects among
     * ores.analytics.quant's IYieldCurveProcess engines via
     * process_factory::make_yield_curve_process().
     */
    std::string process_type = "VASICEK";

    /**
     * @brief Mean-reversion speed parameter.
     */
    double kappa = 0.0;

    /**
     * @brief Constant mean-reversion target level. Persisted as a single scalar, not a full
     * time-varying theta_path: fitting theta(t) to reproduce an observed market curve exactly is
     * out of scope (needs curve bootstrapping, a separate future story) -- a constant theta still
     * drives a real, internally-consistent latent curve via each engine's discount_factor(), just
     * not one fitted to today's market.
     */
    double theta = 0.0;

    /**
     * @brief Volatility parameter.
     */
    double sigma = 0.0;

    /**
     * @brief Starting short rate the process simulates from.
     */
    double initial_rate = 0.0;

    /**
     * @brief Number of synthetic ticks (curve publications) produced per hour.
     */
    int ticks_per_hour = 0;

    /**
     * @brief Whether the configuration is active and eligible for generation.
     */
    bool enabled = false;

    /**
     * @brief Payment frequency for a Swap curve-template entry's fixed leg (references
     * ores.refdata.payment_frequency.code), used to build the strip of intermediate payment dates
     * between spot and maturity that curve_instrument_pricer::swap_par_rate() needs. Not consulted
     * for Deposit/FRA entries, which have no intermediate schedule.
     */
    std::string fixed_leg_payment_frequency_code = "Annual";

    /**
     * @brief The folder this curve lives under (the instrument-type folder, e.g. "IR Curves",
     * nested under an asset-class folder under this curve's owning collection). Real, queryable
     * hierarchy position -- not parsed out of source_name, which stays a display/NATS-subject
     * string. Nullable for now: only the publish-from-dq path populates it; manual creation doesn't
     * yet resolve/create folders -- follow-up work. Mirrors fx_spot_generation_config.folder_id
     * exactly.
     */
    std::optional<boost::uuids::uuid> folder_id;

    /**
     * @brief Username of the person who last modified this IR curve generation config.
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
 * @brief Dispatch-key identifier for ir_curve_generation_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ir_curve_generation_config&) {
    return "ores.synthetic.ir_curve_generation_config";
}

}

#endif
