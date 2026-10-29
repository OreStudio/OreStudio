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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_CLASSIFICATION_RULE_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_CLASSIFICATION_RULE_HPP

#include "ores.marketdata.api/domain/asset_class_authorities.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief How one oresmd identity maps onto the refdata asset class and series subclass codes.
 *
 * The taxonomy rule for a market series, one row per identity the runtime
 * classifies. The runtime reads the whole table once, builds a classifier from it,
 * and asks that classifier for the asset classes and the series subclass of every
 * series it creates.
 *
 * The table replaces a compiled C++ map, so the taxonomy is managed like every
 * other catalogue: an instrument a corpus adds later, or one a user brings, is an
 * inserted row rather than a rebuild. This matters because an ingest that cannot
 * classify a series it must create aborts on it, so a compiled table makes every
 * new instrument a code change.
 *
 * The key is the oresmd identity and not an ORE key. Two kinds of identity reach
 * this table, and key_kind says which a row is keyed by:
 *
 * - datum, keyed by instrument -- the text the URI writes under
 *   instrument, such as ir_swap or discount -- and by quote, the text it
 *   writes under quote, or the empty string for a rule that applies to every
 *   quote of its instrument. Most instruments classify the same way whatever their
 *   quote carries, so their row holds the empty string; a lookup tries the exact
 *   quote first and falls back to the empty one.
 * - index, keyed by instrument too, which for this kind holds the index
 *   family's oresmd name or the empty string for every family. A fixing series is
 *   an index and not a datum, so it is keyed by the index's own vocabulary.
 *
 * asset_class_source says where a row's classes come from:
 *
 * - literal names its class in asset_class_code.
 * - correlation_operands reads the classes from the two operand fields of a
 *   correlation datum, because a pairwise correlation belongs to the two classes
 *   it relates and to no class of its own.
 * - index_authority reads the class from the index family's schema row, whose
 *   asset authority names the market-data namespace the index is written in, and
 *   ores.refdata.asset_class_catalogue maps that authority onto a refdata class.
 *   One row serves every index family, and a family the catalogue maps to no class
 *   leaves the class list empty rather than mis-classing the series.
 *
 * The class codes name rows in refdata.asset_class_code and the subclass codes
 * name rows in refdata.series_subclass_code, and the insert trigger checks both
 * against those catalogues.
 *
 * The reader rejects an empty table, because an empty table aborts the first
 * ingest that reads it and the message should name the table rather than the first
 * series it met.
 */
struct series_classification_rule final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The identity's instrument, as the URI writes it under instrument, in lower case:
     * ir_swap, discount, correlation for a datum; the index family's name such as ibor for an
     * index, or the empty string for a rule that speaks for every family. An empty instrument is
     * the one honest "any" this table states, and only an index row uses it: a datum's identity
     * always names its instrument.
     */
    std::string instrument;

    /**
     * @brief The identity's quote, as the URI writes it under quote, in lower case, or the empty
     * string for a rule that applies to every quote of its instrument. An index has no quote and
     * holds the empty string.
     *
     * A lookup tries the exact quote first and falls back to the empty one.
     */
    std::string quote;

    /**
     * @brief Which kind of oresmd identity this rule is keyed by: datum for a market datum and
     * index for a market index, which is what a fixing series is.
     *
     * Examples: 'datum', 'index'.
     */
    std::string key_kind;

    /**
     * @brief How this rule obtains its asset classes: literal, correlation_operands or
     * index_authority.
     *
     * A literal rule names its class in asset_class_code. A correlation rule reads the classes from
     * the two operand fields of the datum, because a pairwise correlation belongs to the two
     * classes it relates and to no class of its own. An index_authority rule reads the class from
     * the index family's own schema row: one index row serves every family, and the class cannot be
     * a literal because the corpus carries rates, equity, commodity, power, inflation, security and
     * generic fixings alike.
     */
    std::string asset_class_source;

    /**
     * @brief Code of the asset class this type measures, from refdata.asset_class_code.
     *
     * Null on a correlation rule, which names no class of its own, and on a rule whose classes come
     * from the key. The column is nullable rather than empty because null is the honest "no class
     * here", and the insert trigger validates a code only where one is set.
     */
    std::optional<std::string> asset_class_code;

    /**
     * @brief Code of the series subclass this type belongs to, from refdata.series_subclass_code.
     *
     * Example: 'spot' for FX, 'index_fixing' for FIXING, 'correlation' for CORRELATION.
     */
    std::string series_subclass_code;

    /**
     * @brief Why the identity carries this taxonomy, and the evidence behind it.
     *
     * Example: 'The two operands of a correlation datum name the classes it relates; the subclass
     * is the correlation itself.'
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this series classification rule.
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
    friend bool operator==(const series_classification_rule&,
                           const series_classification_rule&) = default;
};

/**
 * @brief Dispatch-key identifier for series_classification_rule, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_classification_rule&) {
    return "ores.marketdata.series_classification_rule";
}

}

#endif
