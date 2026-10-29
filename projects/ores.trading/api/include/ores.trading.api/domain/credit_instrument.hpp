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
#ifndef ORES_TRADING_API_DOMAIN_CREDIT_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_CREDIT_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief The credit family header: one row per credit trade, keyed by trade_id, owning the family's
 * facts, legs and basket.
 *
 * One row per credit trade, the family's own header. It holds what every
 * product in the family states the same way: the trade it belongs to, the trade
 * type code, the party, the activity that wrote the version, the reference
 * entity, the instrument's start and maturity dates and its description. It
 * holds no product economics.
 *
 * The header is the family's *coverage*: the trade_type_code check that the
 * routed-instrument template generates from ores.trading.trade_type_catalogue
 * lists exactly the six codes the importer writes, and the two codes with no
 * importer, CreditDefaultSwapOption and CBO, stay out until a mapper writes them.
 * It is also the family's *owner*: the delete cascade section below names every
 * table the family writes, so the delete rule closes the legs, the basket and
 * the facts with the header and the family does not outlive it.
 *
 * The products' own fields stay in their fact tables, keyed by the same trade
 * and joined to this row by the trade id. A fact table carries no party_id and
 * no trade_type_code: the header is the family's party boundary and its
 * routing boundary.
 *
 * The dates are the family's *common dates*, the one set of names for the start
 * and the maturity of every credit product. A risk participation agreement's
 * protection start and end are those dates.
 */
struct credit_instrument final {
    instrument_identity identity;

    /**
     * @brief What kind of identifier reference_entity_id states: the legal entity's LEI, or an
     * identifier the system's own data uses.
     *
     * A reference is typed rather than guessed, because a credit curve named ACME in an engine
     * document and an LEI are different things and a column that holds either without saying which
     * cannot be joined to anything.
     *
     * The closed set is LEI and Internal, and it follows the shape the trading family's other
     * identifier columns use.
     */
    std::string reference_entity_id_type;

    /**
     * @brief The identifier of the reference entity the risk is on, read with
     * reference_entity_id_type.
     *
     * Every credit product states one. When the type is LEI the value is the legal entity's LEI and
     * the soft reference below applies; when it is Internal the value is the identifier the source
     * states, such as an engine document's credit curve or issuer id, and nothing is claimed about
     * a legal entity.
     *
     * Soft FK to the DQ lei_entity dataset when the type is LEI: the entity belongs to that dataset
     * rather than to a trading table, so the reference is recorded rather than copied. A later
     * change tightens it into a real foreign key once a legal entity lives in refdata.
     */
    std::string reference_entity_id;

    /**
     * @brief The instrument's start date. A risk participation agreement's protection start is this
     * column.
     *
     * Null when the product states none.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief The instrument's maturity date. A risk participation agreement's protection end is
     * this column.
     *
     * Null with start_date, and for the same reason.
     */
    std::optional<std::chrono::year_month_day> maturity_date;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
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
    friend bool operator==(const credit_instrument&, const credit_instrument&) = default;
};

/**
 * @brief The members of credit_instrument the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view credit_instrument_economic_fields[] = {
    "reference_entity_id_type",
    "reference_entity_id",
    "start_date",
    "maturity_date",
};

/**
 * @brief Names the members of credit_instrument that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<credit_instrument>) {
    return credit_instrument_economic_fields;
}

/**
 * @brief Dispatch-key identifier for credit_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const credit_instrument&) {
    return "ores.trading.credit_instrument";
}

}

#endif
