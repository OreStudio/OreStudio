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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_IDENTITY_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_IDENTITY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One column per identity field of a market series, written from the same codec parse that
 * writes its oresmd URI.
 *
 * A projection of market_series.oresmd_uri, one row per series, so a query can
 * join on the series and filter on a real column. The URI stays the only spelling
 * the system writes and the codec stays the only thing that reads it; this table is
 * written from that parse, never read back to rebuild a URI.
 *
 * The column set is the identity fields the codec admits, one column per field
 * the instrument schema marks field_role::identity, and one column per field the
 * index grammar declares, because a fixing's whole ORE index name is its identity,
 * plus the columns that say which kind of identity the row carries. A field the
 * type's schema row, or the family's index row, does not declare is empty, because
 * the row holds only the columns its own grammar fills. A column is text, not a
 * typed relational form, because the codec keeps every value as the key spelled it
 * and a projection that reinterpreted it would be a second spelling of the same
 * identity.
 *
 * The table is a current state, not a history: the series row is already temporal
 * and the identity does not change under it, so one row per series is enough and
 * the primary key is the series. The column list is checked against the codec
 * schema by build/scripts/check_marketdata_identity_columns.py, so the two cannot
 * drift.
 *
 * The identity the decomposed columns spell is unique per party, as the series URI
 * is. A fixing row states that at the table: a unique index over its context and
 * its index-grammar columns, with nulls not distinct so an empty field counts as
 * equal to an empty field, refuses two fixings that spell one identity. A series
 * row cannot carry such an index, because its identity spans the whole instrument
 * schema, more columns than an index may hold; the series table's own unique URI
 * and the decomposition's injectivity are what keep it single.
 */
struct market_series_identity final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The market series this identity belongs to. A soft reference: the series table is
     * temporal and its primary key carries the validity window, so no foreign key can name a single
     * current row of it.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief The party that owns the series, copied so a query filters without joining the series
     * table.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Which grammar named the series: series for a series URI, index for an index (fixing)
     * URI, unknown for a URI neither admits. A series row carries the instrument schema's identity
     * fields and an index row the index grammar's, so a fixing is joinable on every field its ORE
     * index name carries: its currency, its index, its tenor, its FX source, its CMB family, its
     * expiry and its delivery window, as well as its asset class. An unknown row carries no field
     * value, so nothing is invented for a URI the codec could not read.
     */
    std::string identity_kind;

    /**
     * @brief The asset class, as the URI's authority writes it, or empty when the URI's grammar
     * names no asset class the codec knows.
     */
    std::string asset_class;

    /**
     * @brief The ORE instrument type, as the URI's instrument writes it, or empty when the identity
     * is not a series.
     */
    std::string instrument_type;

    /**
     * @brief The ORE quote type, as the URI's quote writes it, or empty when the identity is not a
     * series.
     */
    std::string quote_type;

    /**
     * @brief The atm field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string atm;

    /**
     * @brief The cap_floor field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string cap_floor;

    /**
     * @brief The ccy field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string ccy;

    /**
     * @brief The cds_index_name field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string cds_index_name;

    /**
     * @brief The commodity_name field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string commodity_name;

    /**
     * @brief The contract field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string contract;

    /**
     * @brief The contract_name field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string contract_name;

    /**
     * @brief The curve_id field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string curve_id;

    /**
     * @brief The day_counter field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string day_counter;

    /**
     * @brief The delivery date a power fixing names (POWER-NAME-YYYY-MM-DD), as the index grammar
     * spells it, or empty when the identity is not a power fixing. ORE reads the date as part of
     * the index name, so two power fixings that differ only in their delivery date are two series.
     */
    std::string delivery;

    /**
     * @brief The end of a power fixing's delivery window, in whole seconds from the start of the
     * delivery day, as the index grammar spells it, or empty when the fixing names no window. The
     * grammar name end is a PostgreSQL reserved word, so the column carries the window's name; the
     * projector maps end onto it.
     */
    std::string delivery_end;

    /**
     * @brief The start of a power fixing's delivery window, in whole seconds from the start of the
     * delivery day, as the index grammar spells it, or empty when the fixing names no window. The
     * column is named delivery_start to pair with delivery_end.
     */
    std::string delivery_start;

    /**
     * @brief The doc_clause field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string doc_clause;

    /**
     * @brief The dst field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string dst;

    /**
     * @brief The eq_name field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string eq_name;

    /**
     * @brief The expiry a commodity or bond fixing names (COMM-NAME-YYYY-MM), as the index grammar
     * spells it, or empty when the identity is not a dated fixing. ORE reads the date as part of
     * the index name, so two fixings that differ only in their expiry are two series, not one
     * series at two points.
     */
    std::string expiry;

    /**
     * @brief The family of a CMB fixing, the index grammar's subject (CMB-FAMILY-TENOR), as the URI
     * spells it, or empty when the identity is not a CMB fixing.
     */
    std::string family;

    /**
     * @brief The fixed_ccy field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string fixed_ccy;

    /**
     * @brief The fixed_tenor field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string fixed_tenor;

    /**
     * @brief The flat_ccy field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string flat_ccy;

    /**
     * @brief The flat_term field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string flat_term;

    /**
     * @brief The float_ccy field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string float_ccy;

    /**
     * @brief The float_tenor field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string float_tenor;

    /**
     * @brief The future_contract field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string future_contract;

    /**
     * @brief The fwd_start field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string fwd_start;

    /**
     * @brief The identifier field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string identifier;

    /**
     * @brief The index field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string index;

    /**
     * @brief The index1 field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string index1;

    /**
     * @brief The index2 field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string index2;

    /**
     * @brief The index_name field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string index_name;

    /**
     * @brief The index_tenor field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string index_tenor;

    /**
     * @brief The index_term field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string index_term;

    /**
     * @brief The spread_offset field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it: the spacing between the two legs of a commodity
     * calendar spread option.
     *
     * The field is named spread_offset rather than ORE's own offset because offset is a reserved
     * word in PostgreSQL, and the data layer names its columns without quoting them. A column of
     * that name is refused by the store on every write, so the field cannot carry it either. The
     * name says which offset it is, the spacing of the spread, which offset alone does not.
     */
    std::string spread_offset;

    /**
     * @brief The option_type field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string option_type;

    /**
     * @brief The payer_receiver field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string payer_receiver;

    /**
     * @brief The qualifier field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string qualifier;

    /**
     * @brief The quote_name field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string quote_name;

    /**
     * @brief The quote_tag field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string quote_tag;

    /**
     * @brief The rating_name field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string rating_name;

    /**
     * @brief The relative field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string relative;

    /**
     * @brief The running_spread field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string running_spread;

    /**
     * @brief The seasonality_type field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string seasonality_type;

    /**
     * @brief The security_id field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string security_id;

    /**
     * @brief The seniority field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string seniority;

    /**
     * @brief The side field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string side;

    /**
     * @brief The source of an FX fixing (FX-SOURCE-CCY1-CCY2), as the index grammar spells it, or
     * empty when the identity is not an FX fixing. Two FX fixings of one currency pair differ only
     * in their source, so the column is what keeps them apart.
     */
    std::string source;

    /**
     * @brief The tenor field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string tenor;

    /**
     * @brief The term field of the identity, as the URI spells it, or empty when the type's schema
     * row does not declare it.
     */
    std::string term;

    /**
     * @brief The time_unit field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string time_unit;

    /**
     * @brief The underlying_name field of the identity, as the URI spells it, or empty when the
     * type's schema row does not declare it.
     */
    std::string underlying_name;

    /**
     * @brief The unit_ccy field of the identity, as the URI spells it, or empty when the type's
     * schema row does not declare it.
     */
    std::string unit_ccy;

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
    friend bool operator==(const market_series_identity&, const market_series_identity&) = default;
};

/**
 * @brief Dispatch-key identifier for market_series_identity, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_series_identity&) {
    return "ores.marketdata.market_series_identity";
}

}

#endif
