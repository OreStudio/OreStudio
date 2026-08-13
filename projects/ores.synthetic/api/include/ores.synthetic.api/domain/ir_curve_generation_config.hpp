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
 * short-rate process drives it (vasicek/cox_ingersoll_ross/hull_white/
 * two_factor_gaussian, per ores.analytics.quant's IYieldCurveProcess
 * engines) and that process's parameters, stored row-based as
 * ir_curve_generation_config_process_parameter_value children joined onto the
 * yield_curve_process_parameter_definition catalogue -- so a new process
 * type or parameter is seed data, not a schema change. The actual instrument
 * grid (which tenors to publish at which role) is held separately as ordered
 * ir_curve_template_entry rows, the same one-config-many-children shape
 * fx_spot_generation_config uses for its gmm_component rows. Scoped to a
 * tenant and a party.
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
     * @brief Floating-rate index family this curve represents (oresmd's index_family: libor/euribor
     * for term IBOR-style indices, or one of the overnight/RFR-style families --
     * sofr/estr/sonia/tona/ saron/aonia/corra/honia/sora/swestr/nowa/kofr/mibor/
     * zaronia/destr/polonia/nzonia/shibor/tiie/taibor), stored as a fixed lower-case token rather
     * than the full ISDA/ORE code the predecessor index_name column held ("USD-SOFR") --
     * currency_code already carries the currency half of that pair. The (currency_code,
     * index_family) pair is validated as a whole by a hand-written composite trigger (not the
     * codegen single-argument Validations mechanism, which cannot express a two-column lookup)
     * against overnight_index_convention/ibor_index_convention, whose own id is currency_code + "-"
     * + upper(index_family).
     */
    std::string index_family;

    /**
     * @brief Tenor of this curve's floating-rate index (references tenor.code, e.g. "3M", "6M"),
     * required when index_family is a term family (libor/euribor), empty for overnight families
     * (sofr/estr/sonia/tona, which have no tenor dimension) -- mirrors oresmd's own
     * index_family-conditional grammar. The literal discriminator "FOMC" is valid only for
     * index_family sofr (schema-enforced): it keys a meeting-dated curve variant for the same
     * (currency_code, index_family) (e.g. the USD-SOFR FOMC-dated short end, whose template entries
     * resolve to meeting-dated pillars 1F..8F via tenor_resolution), so the config publishes its
     * own distinct series identity instead of colliding with the standard curve's empty-tenor row.
     * Other overnight families must leave tenor empty: no tenor_resolution meeting-dated pillar
     * machinery exists for them. Empty string, not SQL NULL: the codegen used here has no
     * true-nullable text support, same pattern as vintage_source/vintage_date. Not DB-FK-validated
     * against tenor, matching the precedent set by ir_curve_template_entry's own
     * start_tenor_code/end_tenor_code (also soft FKs to tenor.code, resolved only at the
     * application layer via ores.refdata.api/domain/tenor_resolution.hpp, not a DB trigger --
     * tenor.org itself emits no single-argument validator function to reuse).
     */
    std::string tenor;

    /**
     * @brief Whether this curve discounts cashflows, projects a floating index's forward rate, or
     * both (oresmd's curve_role: discount/projection/self_discounting). Defaults to
     * self_discounting for backward-compatible seed data (the shape every existing synthetic IR
     * curve config already assumes: one curve serving both purposes). Fixed 3-value vocabulary
     * intrinsic to this record, not a reference to another entity -- no soft FK, validated by a
     * plain SQL check rather than a lookup-table trigger. curve_feed_controller's collision check
     * deliberately excludes role from its conflict key, so a discount config and a projection
     * config for the same (currency_code, index_family, tenor) can run side by side.
     */
    std::string role = "self_discounting";

    /**
     * @brief Short-rate process engine driving this curve (references
     * yield_curve_process_type.code: VASICEK, COX_INGERSOLL_ROSS, HULL_WHITE, or
     * TWO_FACTOR_GAUSSIAN) -- selects among ores.analytics.quant's IYieldCurveProcess engines. The
     * engine's parameters are not stored as columns on this record; each process type has a
     * yield_curve_process_parameter_definition catalogue (name, description, min/max, defaults) and
     * this config's values live as ir_curve_generation_config_process_parameter_value rows, one per
     * parameter, joined onto the definitions at feed-start time by
     * map_parameters_to_yield_curve_process() -- the same one-config- many-children shape
     * fx_spot_generation_config uses for its gmm_component rows. Adding a new process type (or
     * parameter) is therefore purely seed data: no ALTER TABLE, no codegen.
     */
    std::string process_type = "VASICEK";

    /**
     * @brief Number of synthetic ticks (curve publications) produced per hour.
     */
    int ticks_per_hour = 0;

    /**
     * @brief Whether the configuration is startable at all -- manually or automatically.
     * enabledfalse= means retired/disabled: neither auto-start nor a manual Start action may start
     * it. Orthogonal to auto_start (see below), which further restricts which of the enabled
     * configs actually start on their own.
     */
    bool enabled = false;

    /**
     * @brief Whether this config starts automatically when the service comes up, as opposed to
     * manual-start-only. Orthogonal to enabled: a config can be enabledtrue, auto_startfalse
     * (valid, manually startable, but never auto-started -- e.g. a legacy/alternate-index variant
     * living alongside a currency's primary config so the two never silently fight over the same
     * published qualifier at boot). Starting a feed -- whether auto-start-at-boot or a manual Start
     * -- is rejected outright if another feed is already running for the same (currency_code,
     * index_family, tenor) tuple *and the same* role; nothing auto-disables the running one, so
     * switching requires an explicit Stop then Start.
     */
    bool auto_start = false;

    /**
     * @brief Where this curve's starting short rate comes from: "fixed" (use the initial_rate
     * parameter value as entered in the parameter rows) or "vintage" (derive it from the
     * vintage_source/vintage_date market-data observation on this config's shortest-tenor DEPO
     * entry, guarded by availability). The vintage_source/vintage_date pair is populated only when
     * this is "vintage" -- see the SQL check. Unlike fx_spot_generation_config.price_source, this
     * check does not sentinel initial_rate to a fixed value when "vintage": real short rates can
     * legitimately be zero or negative (unlike an FX spot price), so initial_rate is simply left at
     * whatever was last stored/computed and overwritten by the resolved vintage value at feed-start
     * time. Defaults to "fixed" (unlike fx_spot_generation_config.price_source, which defaults to
     * "vintage"): existing IR curve configs migrated from the flat-column era all predate
     * vintage-awareness, so "fixed" is the non-breaking default for this column's introduction.
     */
    std::string price_source = "fixed";

    /**
     * @brief Source tag of the market-data vintage this curve's starting short rate and
     * availability guard are validated against (e.g. "ore.reference"), matching
     * market_observation.source. Only populated (and required) when price_source is "vintage";
     * empty when "fixed" -- see price_source. (Empty string, not SQL NULL: the codegen used here
     * has no true-nullable text support.)
     */
    std::string vintage_source;

    /**
     * @brief Observation date of the market-data vintage this curve's starting short rate and
     * availability guard are validated against, ISO format (e.g. "2016-02-05"), matching
     * market_observation.observation_datetime. Only populated (and required) when price_source is
     * "vintage"; empty when "fixed" -- see price_source.
     */
    std::string vintage_date;

    /**
     * @brief Free-text description of what this configuration represents -- what
     * regime/vintage/index it targets, and, for a legacy index (e.g. USD LIBOR-3M, EONIA), that it
     * is historical/discontinued and why it is still offered (testing pre-cessation scenarios).
     * Shown in the Detail dialog and list views rather than left to a bare currency/index code to
     * explain itself.
     */
    std::string description;

    /**
     * @brief Payment frequency for a Swap curve-template entry's fixed leg (references
     * ores.refdata.payment_frequency.code), used to build the strip of intermediate payment dates
     * between spot and maturity that curve_instrument_pricer::swap_par_rate() needs. Not consulted
     * for Deposit/FRA entries, which have no intermediate schedule.
     */
    std::string fixed_leg_payment_frequency_code = "Annual";

    /**
     * @brief Stable source name carried as provenance of published ticks/observations (e.g.
     * "synthetic.realistic.usdsofr") and as the NATS subject suffix
     * (synthetic.v1.tick.ir_curve.<source_name>) -- mirrors fx_spot_generation_config.source_name
     * exactly: namespaced by collection only (so two collections' same currency+index never
     * collide), editable, defaulting to a derived value at publish/save time rather than parsed
     * back out of currency_code/index_family/tenor at every consumer (the earlier approach,
     * replaced by this column).
     */
    std::string source_name;

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
