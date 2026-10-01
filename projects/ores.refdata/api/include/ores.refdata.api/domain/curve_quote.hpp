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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_QUOTE_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_QUOTE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One market quote a curve entry or one of its segments is built from.
 *
 * The market quotes a curve is built from. The corpus holds four thousand one
 * hundred and fifty-six of them directly on a curve entry -- a default curve
 * names its credit spreads this way -- and twenty-nine thousand more inside the
 * segments of the yield curves, which is where most of them live.
 *
 * A quote is one string naming a market point, FRA/RATE/USD/1M/1M or
 * CDS/CREDIT_SPREAD/BANK/SR/USD/1Y, and the order they are listed in is the
 * order the curve is built in, so it is kept.
 *
 * Two places hold the same element and this one table holds both. A quote whose
 * curve_segment_id is set belongs to that segment's own list; a quote whose
 * curve_segment_id is nil belongs to the curve entry itself, and the export
 * writes it at the level it came from. The element is not always spelled Quote:
 * an average OIS segment names its quotes CompositeQuote, so the item's own
 * element name is a column.
 */
struct curve_quote final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the quote.
     */
    boost::uuids::uuid id;

    /**
     * @brief The curve recipe the quote belongs to, whether it sits on the entry or inside one of
     * the entry's segments. Carried on both so the quotes of a curve can be read without walking
     * the segments.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The segment whose own list holds the quote, or the nil uuid when the quote sits
     * directly on the curve entry.
     */
    boost::uuids::uuid curve_segment_id = boost::uuids::nil_uuid();

    /**
     * @brief The element the quote is written as: Quote in almost every list, CompositeQuote in an
     * average OIS segment's.
     */
    std::string item_kind;

    /**
     * @brief The market point the quote names, exactly as the document spells it.
     */
    std::string quote_text;

    /**
     * @brief The order the document listed the quote in, which is the order the curve is built in
     * and what the export restores.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this curve quote.
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
    friend bool operator==(const curve_quote&, const curve_quote&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_quote, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_quote&) {
    return "ores.refdata.curve_quote";
}

}

#endif
