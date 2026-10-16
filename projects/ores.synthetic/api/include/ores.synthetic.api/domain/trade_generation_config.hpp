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
#ifndef ORES_SYNTHETIC_API_DOMAIN_TRADE_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_API_DOMAIN_TRADE_GENERATION_CONFIG_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A named, party-scoped recipe for generating a synthetic trade population.
 *
 * A named recipe for the trades one party's books receive. It is the
 * population as configuration: a reviewer reads the row and the two scope
 * lists rather than a command.
 *
 * The row names the booking party, the instrument family, the tenor and
 * notional bands, the as-of anchor and the seed. The book scope and the
 * counterparty scope are the two multi-valued field sets and live in child
 * tables, one row per book and one row per counterparty, the same
 * one-config-many-children shape fx_spot_generation_config uses for its
 * gmm_component rows and ir_curve_generation_config for its
 * ir_curve_template_entry rows.
 *
 * The booking party is the row's own party_id. The writer sets
 * app.current_party_id to it before each trade, because the trading
 * insert triggers overwrite the instrument's party from session state.
 *
 * The two scope lists are the child tables trade_generation_book and
 * trade_generation_counterparty, one row per book and one row per
 * counterparty. Both are edited from this configuration's detail dialog.
 *
 * The population is a function of the pair (seed, as-of anchor). The seed
 * pins the shape of the population and not the identity of its rows. The
 * same pair emits the same rows; a later anchor moves the population
 * forward rather than repeating it. See
 * [[id:6E648B05-2ADB-4E03-89CB-D4670CB72FC3][Investigation: Populating ACME
 * with a trade population]], "What makes a seed deterministic".
 */
struct trade_generation_config final {
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
     * @brief Owning party (legal entity) this configuration belongs to. It is also the booking
     * party: the party whose books the population is booked into, and the party the writer sets
     * app.current_party_id from. A reference to ores_refdata_parties_tbl, held as a plain uuid as
     * every neighbouring synthetic generation config holds its party.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Stable name for this configuration, unique per tenant and party.
     */
    std::string name;

    /**
     * @brief Free-text description of the configuration.
     */
    std::string description;

    /**
     * @brief Instrument family this configuration populates, as an asset class (references
     * asset_class_code.code: interest_rates, fx, equity, credit, commodity, inflation or bond). It
     * is a reference to the refdata catalogue rather than a parallel vocabulary, and it is
     * validated at insert time by the generated asset-class validator. One configuration carries
     * one family, so covering the book takes one configuration per family.
     */
    std::string asset_class_code = "interest_rates";

    /**
     * @brief Short end of the tenor band this configuration draws from (references tenor.code, e.g.
     * "1Y"). A soft FK resolved at the application layer through
     * ores.refdata.api/domain/tenor_resolution.hpp, not a DB trigger: tenor.org emits no
     * single-argument validator to reuse, matching the precedent ir_curve_template_entry's own
     * tenor columns set.
     */
    std::string min_tenor_code;

    /**
     * @brief Long end of the tenor band (references tenor.code, e.g. "10Y"). Its order against
     * min_tenor_code is not checkable in SQL, because a tenor is a label the refdata catalogue
     * resolves; the generator resolves both and refuses a band whose ends do not order.
     */
    std::string max_tenor_code;

    /**
     * @brief Smallest notional a generated trade may carry, in the trade's own currency. An exact
     * decimal, per the exact-numbers rule; the floor and the ceiling of the band bound a draw, they
     * do not describe one trade.
     */
    ores::utility::decimal::decimal min_notional;

    /**
     * @brief Largest notional a generated trade may carry. Must be at least min_notional, which the
     * SQL check on this table enforces.
     */
    ores::utility::decimal::decimal max_notional;

    /**
     * @brief The as-of anchor every generated trade date is an offset from. It is an input, never a
     * read of the clock inside a field generator, so a re-provision at a later anchor moves the
     * population instead of shifting stored rows. The population is a function of this column and
     * seed together.
     */
    std::chrono::year_month_day as_of_date;

    /**
     * @brief Seed for the generator's random engine. Two runs at the same (seed, as-of) pair emit
     * the same population, which makes a review a rebuild and a diff. It pins the shape of the
     * population, not the identity of rows: a moved anchor deliberately moves the population, and a
     * stable trade identifier is derived from stable inputs rather than from this number alone.
     */
    std::uint64_t seed = 1;

    /**
     * @brief Whether the configuration is active and eligible for generation.
     */
    bool enabled = false;

    /**
     * @brief The curve the population's bonds are discounted on, as the oresmd URI of the market
     * series that curve is. An ORE bond resolves its discount curve by the index the trade names
     * and the market's own forwarding decides which curve that index is, so the trade states a
     * curve and the market decides the rest.
     *
     * The population is kept in one currency, the first book's own, so one curve serves every trade
     * it emits. A population whose book keeps no currency a curve exists for states none, and its
     * bonds then name no curve and are refused by the engine's builder rather than priced on a
     * guess.
     */
    std::string reference_curve_id;

    /**
     * @brief Username of the person who last modified this trade generation config.
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
    friend bool operator==(const trade_generation_config&,
                           const trade_generation_config&) = default;
};

/**
 * @brief Dispatch-key identifier for trade_generation_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_generation_config&) {
    return "ores.synthetic.trade_generation_config";
}

}

#endif
