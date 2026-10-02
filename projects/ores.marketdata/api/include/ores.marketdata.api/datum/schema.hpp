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
#ifndef ORES_MARKETDATA_API_DATUM_SCHEMA_HPP
#define ORES_MARKETDATA_API_DATUM_SCHEMA_HPP

#include "ores.marketdata.api/datum/ore_types.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.marketdata.api/export.hpp"
#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>

/**
 * @file schema.hpp
 * @brief What a market datum of each instrument type holds.
 *
 * One row per instrument type lists the type's fields in the order its key
 * writes them, whether each field is part of the series identity or a coordinate
 * within the series, and whether the key may leave it out. The table is data:
 * an instrument type is a row here, not a C++ type.
 *
 * The fields carry ORE's meanings, taken from the inspectors ORE's datum classes
 * declare (see external/ore/catalogue). Where ORE drops a token its key carries
 * -- the curve name of a ZERO or DISCOUNT key, the identifier of a basis swap --
 * the token is a field all the same, because a key must read back as written.
 */

namespace ores::marketdata::datum {

/// Every field a market data key carries.
enum class field : std::uint8_t {
    ccy,
    curve_id,
    day_counter,
    term,
    index_name,
    fwd_start,
    contract_month,
    contract,
    tenor,
    imm1,
    imm2,
    flat_term,
    maturity,
    identifier,
    flat_ccy,
    float_ccy,
    float_tenor,
    fixed_ccy,
    fixed_tenor,
    underlying_name,
    seniority,
    doc_clause,
    running_spread,
    cds_index_name,
    attachment_point,
    detachment_point,
    unit_ccy,
    quote_tag,
    expiry,
    dimension,
    strike_level,
    payer_receiver,
    index_tenor,
    atm,
    relative,
    cap_floor,
    strike_label,
    index,
    seasonality_type,
    month,
    eq_name,
    strike,
    option_type,
    security_id,
    future_contract,
    qualifier,
    contract_name,
    index_term,
    side,
    commodity_name,
    offset,
    index1,
    index2,
    quote_name,
    delivery_date,
    start_time_in_sec,
    time_unit,
    dst,
    rating_name,
    from_rating,
    to_rating
};

inline constexpr std::size_t field_count = 61;

/// Which of the value types a field holds.
enum class value_kind : std::uint8_t { text, term, decimal, strike, code };

/// Whether a field names the series or a point within it.
enum class field_role : std::uint8_t { identity, coordinate };

/**
 * @brief The value type each field holds.
 *
 * A switch with no default, so a field added to the enum and not here fails the
 * build.
 */
constexpr value_kind kind_of(field f) {
    switch (f) {
        case field::ccy:
        case field::curve_id:
        case field::day_counter:
        case field::index_name:
        case field::contract_month:
        case field::contract:
        case field::identifier:
        case field::flat_ccy:
        case field::float_ccy:
        case field::fixed_ccy:
        case field::underlying_name:
        case field::seniority:
        case field::doc_clause:
        case field::cds_index_name:
        case field::unit_ccy:
        case field::quote_tag:
        case field::strike_label:
        case field::index:
        case field::seasonality_type:
        case field::month:
        case field::eq_name:
        case field::security_id:
        case field::future_contract:
        case field::qualifier:
        case field::contract_name:
        case field::commodity_name:
        case field::index1:
        case field::index2:
        case field::quote_name:
        case field::rating_name:
        case field::from_rating:
        case field::to_rating:
            return value_kind::text;
        case field::term:
        case field::fwd_start:
        case field::tenor:
        case field::flat_term:
        case field::maturity:
        case field::float_tenor:
        case field::fixed_tenor:
        case field::expiry:
        case field::index_tenor:
        case field::index_term:
        case field::delivery_date:
            return value_kind::term;
        case field::imm1:
        case field::imm2:
        case field::running_spread:
        case field::attachment_point:
        case field::detachment_point:
        case field::strike_level:
        case field::offset:
        case field::start_time_in_sec:
            return value_kind::decimal;
        case field::strike:
            return value_kind::strike;
        case field::dimension:
        case field::payer_receiver:
        case field::atm:
        case field::relative:
        case field::cap_floor:
        case field::option_type:
        case field::side:
        case field::time_unit:
        case field::dst:
            return value_kind::code;
    }
    return value_kind::text;
}

template <value_kind K>
struct kind_type;
template <>
struct kind_type<value_kind::text> {
    using type = std::string;
};
template <>
struct kind_type<value_kind::term> {
    using type = term;
};
template <>
struct kind_type<value_kind::decimal> {
    using type = decimal;
};
template <>
struct kind_type<value_kind::strike> {
    using type = strike;
};
template <>
struct kind_type<value_kind::code> {
    using type = code;
};

/// The C++ type field @p F holds, so a typed read is checked at compile time.
template <field F>
using field_value_t = typename kind_type<kind_of(F)>::type;

/// The field's name, as a URI query key writes it: fwd_start, cap_floor.
ORES_MARKETDATA_API_EXPORT std::string_view name_of(field f);

/// The field @p name names, or nothing.
ORES_MARKETDATA_API_EXPORT std::optional<field> field_named(std::string_view name);

/**
 * @brief The tokens ORE accepts for a code field, as its parser checks them;
 * empty for a field that is not a code.
 */
ORES_MARKETDATA_API_EXPORT std::span<const std::string_view> codes_of(field f);

struct field_spec {
    field name;
    field_role role;
    /// Whether a key of this type may leave the field out, in which case the
    /// datum holds none.
    bool may_be_none;
};

/**
 * @brief The asset class an instrument type belongs to, which an oresmd URI
 * writes as its authority.
 *
 * The names are the oresmd authorities ores.refdata's asset class catalogue
 * maps onto its asset classes.
 */
enum class asset_class : std::uint8_t {
    ir,
    fx,
    credit,
    equity,
    commodity,
    inflation,
    security,
    correlation,
    rating,
    shape_profile
};

inline constexpr std::size_t asset_class_count = 10;

/// The asset class as an oresmd URI's authority writes it: ir, shape_profile.
ORES_MARKETDATA_API_EXPORT std::string_view name_of(asset_class a);

/// The asset class @p name names, or nothing.
ORES_MARKETDATA_API_EXPORT std::optional<asset_class> asset_class_named(std::string_view name);

struct schema_row {
    instrument_type type;
    std::span<const field_spec> fields;
    asset_class asset;
    /// The field that names what the datum is about: the currency of a rate,
    /// the reference entity of a CDS. An oresmd URI writes it as its path.
    field subject;
};

namespace detail {

constexpr field_spec id(field f) {
    return {f, field_role::identity, false};
}
constexpr field_spec id_or_none(field f) {
    return {f, field_role::identity, true};
}
constexpr field_spec at(field f) {
    return {f, field_role::coordinate, false};
}
constexpr field_spec at_or_none(field f) {
    return {f, field_role::coordinate, true};
}

using f = field;

inline constexpr std::array zero{id(f::ccy), id(f::curve_id), id(f::day_counter), at(f::term)};
inline constexpr std::array discount{id(f::ccy), id(f::curve_id), at(f::term)};
inline constexpr std::array mm{
    id(f::ccy), id_or_none(f::index_name), id(f::fwd_start), at(f::term)};
inline constexpr std::array mm_future{
    id(f::ccy), at(f::contract_month), id(f::contract), id(f::tenor)};
inline constexpr std::array oi_future{
    id(f::ccy), at(f::contract_month), id(f::contract), id(f::tenor)};
inline constexpr std::array fra{id(f::ccy), at(f::fwd_start), at(f::term)};
inline constexpr std::array imm_fra{id(f::ccy), at(f::imm1), at(f::imm2)};
inline constexpr std::array ir_swap{
    id(f::ccy), id_or_none(f::index_name), id(f::fwd_start), id(f::tenor), at(f::term)};
inline constexpr std::array basis_swap{
    id(f::flat_term), id(f::term), id(f::ccy), id_or_none(f::identifier), at(f::maturity)};
inline constexpr std::array bma_swap{id(f::ccy), id(f::term), at(f::maturity)};
inline constexpr std::array cc_basis_swap{
    id(f::flat_ccy), id(f::flat_term), id(f::ccy), id(f::term), at(f::maturity)};
inline constexpr std::array cc_fix_float_swap{
    id(f::float_ccy), id(f::float_tenor), id(f::fixed_ccy), id(f::fixed_tenor), at(f::maturity)};
inline constexpr std::array cds{id(f::underlying_name),
                                id_or_none(f::seniority),
                                id(f::ccy),
                                id_or_none(f::doc_clause),
                                at_or_none(f::term),
                                id_or_none(f::running_spread)};
inline constexpr std::array cds_index{id(f::cds_index_name), id(f::term), at(f::detachment_point)};
inline constexpr std::array fx_spot{id(f::unit_ccy), id(f::ccy)};
inline constexpr std::array fx_fwd{id(f::unit_ccy), id(f::ccy), at(f::term)};
inline constexpr std::array hazard_rate{
    id(f::underlying_name), id(f::seniority), id(f::ccy), id_or_none(f::doc_clause), at(f::term)};
inline constexpr std::array recovery_rate{id(f::underlying_name),
                                          id_or_none(f::seniority),
                                          id_or_none(f::ccy),
                                          id_or_none(f::doc_clause)};
inline constexpr std::array swaption{id(f::ccy),
                                     id_or_none(f::quote_tag),
                                     at_or_none(f::expiry),
                                     at(f::term),
                                     at_or_none(f::dimension),
                                     at_or_none(f::strike_level),
                                     id_or_none(f::payer_receiver)};
inline constexpr std::array capfloor{id(f::ccy),
                                     id_or_none(f::index_name),
                                     at_or_none(f::term),
                                     id(f::index_tenor),
                                     id_or_none(f::atm),
                                     id_or_none(f::relative),
                                     at_or_none(f::strike_level),
                                     id_or_none(f::cap_floor)};
inline constexpr std::array fx_option{
    id(f::unit_ccy), id(f::ccy), at(f::expiry), at(f::strike_label)};
inline constexpr std::array inflation_swap{id(f::index), at(f::term)};
inline constexpr std::array inflation_capfloor{
    id(f::index), at(f::term), id(f::cap_floor), at(f::strike_level)};
inline constexpr std::array seasonality{id(f::seasonality_type), id(f::index), at(f::month)};
inline constexpr std::array equity_spot{id(f::eq_name), id(f::ccy)};
inline constexpr std::array equity_dated{id(f::eq_name), id(f::ccy), at(f::expiry)};
inline constexpr std::array equity_option{
    id(f::eq_name), id(f::ccy), at(f::expiry), at(f::strike), id_or_none(f::option_type)};
inline constexpr std::array bond{id(f::security_id)};
inline constexpr std::array bond_future{id(f::security_id), id_or_none(f::future_contract)};
inline constexpr std::array bond_option{
    id(f::qualifier), at_or_none(f::expiry), at(f::term), at_or_none(f::dimension)};
inline constexpr std::array bond_future_option{
    id(f::contract_name), at(f::expiry), at(f::strike), id_or_none(f::option_type)};
inline constexpr std::array index_cds_option{id(f::index_name),
                                             id_or_none(f::index_term),
                                             at(f::expiry),
                                             at_or_none(f::strike),
                                             id_or_none(f::side)};
inline constexpr std::array index_cds_tranche{
    id(f::cds_index_name), id(f::term), at_or_none(f::attachment_point), at(f::detachment_point)};
inline constexpr std::array commodity_spot{id(f::commodity_name), id(f::ccy)};
inline constexpr std::array commodity_fwd{id(f::commodity_name), id(f::ccy), at(f::expiry)};
inline constexpr std::array correlation{
    id(f::index1), id(f::index2), at(f::expiry), at(f::strike_label)};
inline constexpr std::array commodity_option{id(f::commodity_name),
                                             id_or_none(f::ccy),
                                             at_or_none(f::expiry),
                                             at_or_none(f::strike),
                                             id_or_none(f::option_type)};
inline constexpr std::array commodity_calendar_spread_option{
    id(f::commodity_name), id(f::offset), id(f::ccy), at(f::expiry), at(f::strike)};
inline constexpr std::array shape_profile{id(f::quote_name),
                                          at(f::delivery_date),
                                          at(f::start_time_in_sec),
                                          id(f::time_unit),
                                          id_or_none(f::dst)};
inline constexpr std::array cpr{id(f::security_id)};
inline constexpr std::array rating{id(f::rating_name), at(f::from_rating), at(f::to_rating)};

using t = instrument_type;

}

/**
 * @brief The schema: one row per instrument type, in the enum's order.
 *
 * A row lists the fields in the order the type's key writes them. A key form
 * that leaves a field out -- a CDS with no doc clause, a swaption shift with no
 * expiry -- gives that field none.
 */
inline constexpr std::array<schema_row, instrument_type_count> schema{{
    {detail::t::zero, detail::zero, asset_class::ir, field::ccy},
    {detail::t::discount, detail::discount, asset_class::ir, field::ccy},
    {detail::t::mm, detail::mm, asset_class::ir, field::ccy},
    {detail::t::mm_future, detail::mm_future, asset_class::ir, field::ccy},
    {detail::t::oi_future, detail::oi_future, asset_class::ir, field::ccy},
    {detail::t::fra, detail::fra, asset_class::ir, field::ccy},
    {detail::t::imm_fra, detail::imm_fra, asset_class::ir, field::ccy},
    {detail::t::ir_swap, detail::ir_swap, asset_class::ir, field::ccy},
    {detail::t::basis_swap, detail::basis_swap, asset_class::ir, field::ccy},
    {detail::t::bma_swap, detail::bma_swap, asset_class::ir, field::ccy},
    {detail::t::cc_basis_swap, detail::cc_basis_swap, asset_class::ir, field::ccy},
    {detail::t::cc_fix_float_swap, detail::cc_fix_float_swap, asset_class::ir, field::fixed_ccy},
    {detail::t::cds, detail::cds, asset_class::credit, field::underlying_name},
    {detail::t::cds_index, detail::cds_index, asset_class::credit, field::cds_index_name},
    {detail::t::fx_spot, detail::fx_spot, asset_class::fx, field::unit_ccy},
    {detail::t::fx_fwd, detail::fx_fwd, asset_class::fx, field::unit_ccy},
    {detail::t::hazard_rate, detail::hazard_rate, asset_class::credit, field::underlying_name},
    {detail::t::recovery_rate, detail::recovery_rate, asset_class::credit, field::underlying_name},
    {detail::t::assumed_recovery_rate, detail::recovery_rate, asset_class::credit, field::underlying_name},
    {detail::t::swaption, detail::swaption, asset_class::ir, field::ccy},
    {detail::t::capfloor, detail::capfloor, asset_class::ir, field::ccy},
    {detail::t::fx_option, detail::fx_option, asset_class::fx, field::unit_ccy},
    {detail::t::zc_inflation_swap, detail::inflation_swap, asset_class::inflation, field::index},
    {detail::t::zc_inflation_capfloor, detail::inflation_capfloor, asset_class::inflation, field::index},
    {detail::t::yy_inflation_swap, detail::inflation_swap, asset_class::inflation, field::index},
    {detail::t::yy_inflation_capfloor, detail::inflation_capfloor, asset_class::inflation, field::index},
    {detail::t::seasonality, detail::seasonality, asset_class::inflation, field::index},
    {detail::t::equity_spot, detail::equity_spot, asset_class::equity, field::eq_name},
    {detail::t::equity_fwd, detail::equity_dated, asset_class::equity, field::eq_name},
    {detail::t::equity_dividend, detail::equity_dated, asset_class::equity, field::eq_name},
    {detail::t::equity_option, detail::equity_option, asset_class::equity, field::eq_name},
    {detail::t::bond, detail::bond, asset_class::security, field::security_id},
    {detail::t::bond_future, detail::bond_future, asset_class::security, field::security_id},
    {detail::t::bond_option, detail::bond_option, asset_class::security, field::qualifier},
    {detail::t::bond_future_option, detail::bond_future_option, asset_class::security, field::contract_name},
    {detail::t::index_cds_option, detail::index_cds_option, asset_class::credit, field::index_name},
    {detail::t::index_cds_tranche, detail::index_cds_tranche, asset_class::credit, field::cds_index_name},
    {detail::t::commodity_spot, detail::commodity_spot, asset_class::commodity, field::commodity_name},
    {detail::t::commodity_fwd, detail::commodity_fwd, asset_class::commodity, field::commodity_name},
    {detail::t::correlation, detail::correlation, asset_class::correlation, field::index1},
    {detail::t::commodity_option, detail::commodity_option, asset_class::commodity, field::commodity_name},
    {detail::t::commodity_calendar_spread_option, detail::commodity_calendar_spread_option, asset_class::commodity, field::commodity_name},
    {detail::t::shape_profile, detail::shape_profile, asset_class::shape_profile, field::quote_name},
    {detail::t::cpr, detail::cpr, asset_class::security, field::security_id},
    {detail::t::rating, detail::rating, asset_class::rating, field::rating_name},
}};

/// The row for @p t.
constexpr const schema_row& schema_of(instrument_type t) {
    return schema[static_cast<std::size_t>(t)];
}

namespace detail {

constexpr bool rows_follow_the_enum() {
    for (std::size_t i = 0; i < schema.size(); ++i) {
        if (static_cast<std::size_t>(schema[i].type) != i)
            return false;
    }
    return true;
}

constexpr bool no_row_repeats_a_field() {
    for (const auto& row : schema) {
        for (std::size_t i = 0; i < row.fields.size(); ++i) {
            for (std::size_t j = i + 1; j < row.fields.size(); ++j) {
                if (row.fields[i].name == row.fields[j].name)
                    return false;
            }
        }
    }
    return true;
}

constexpr bool every_row_has_an_identity() {
    for (const auto& row : schema) {
        bool found = false;
        for (const auto& spec : row.fields)
            found = found || (spec.role == field_role::identity && !spec.may_be_none);
        if (!found)
            return false;
    }
    return true;
}

constexpr bool every_subject_always_names_the_series() {
    for (const auto& row : schema) {
        bool found = false;
        for (const auto& spec : row.fields) {
            found = found || (spec.name == row.subject && spec.role == field_role::identity &&
                              !spec.may_be_none);
        }
        if (!found)
            return false;
    }
    return true;
}

}

static_assert(detail::rows_follow_the_enum(), "schema rows must follow instrument_type's order");
static_assert(detail::no_row_repeats_a_field(), "a schema row names a field twice");
static_assert(detail::every_row_has_an_identity(),
              "every schema row needs a field that always names the series");
static_assert(detail::every_subject_always_names_the_series(),
              "a row's subject must be one of its identity fields that is never none");

}

#endif
