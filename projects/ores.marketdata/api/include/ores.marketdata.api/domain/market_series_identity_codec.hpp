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
 * Template: cpp_domain_type_codec.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_IDENTITY_CODEC_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_IDENTITY_CODEC_HPP

#include "ores.marketdata.api/domain/market_series_identity.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::marketdata::domain {

/**
 * @brief The members of market_series_identity, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_identity>) {
    return std::tuple{
        utility::codec::field{"tenant_id", &market_series_identity::tenant_id},
        utility::codec::field{"series_id", &market_series_identity::series_id},
        utility::codec::field{"party_id", &market_series_identity::party_id},
        utility::codec::field{"identity_kind", &market_series_identity::identity_kind},
        utility::codec::field{"asset_class", &market_series_identity::asset_class},
        utility::codec::field{"instrument_type", &market_series_identity::instrument_type},
        utility::codec::field{"quote_type", &market_series_identity::quote_type},
        utility::codec::field{"atm", &market_series_identity::atm},
        utility::codec::field{"cap_floor", &market_series_identity::cap_floor},
        utility::codec::field{"ccy", &market_series_identity::ccy},
        utility::codec::field{"cds_index_name", &market_series_identity::cds_index_name},
        utility::codec::field{"commodity_name", &market_series_identity::commodity_name},
        utility::codec::field{"contract", &market_series_identity::contract},
        utility::codec::field{"contract_name", &market_series_identity::contract_name},
        utility::codec::field{"curve_id", &market_series_identity::curve_id},
        utility::codec::field{"day_counter", &market_series_identity::day_counter},
        utility::codec::field{"delivery", &market_series_identity::delivery},
        utility::codec::field{"delivery_end", &market_series_identity::delivery_end},
        utility::codec::field{"delivery_start", &market_series_identity::delivery_start},
        utility::codec::field{"doc_clause", &market_series_identity::doc_clause},
        utility::codec::field{"dst", &market_series_identity::dst},
        utility::codec::field{"eq_name", &market_series_identity::eq_name},
        utility::codec::field{"expiry", &market_series_identity::expiry},
        utility::codec::field{"family", &market_series_identity::family},
        utility::codec::field{"fixed_ccy", &market_series_identity::fixed_ccy},
        utility::codec::field{"fixed_tenor", &market_series_identity::fixed_tenor},
        utility::codec::field{"flat_ccy", &market_series_identity::flat_ccy},
        utility::codec::field{"flat_term", &market_series_identity::flat_term},
        utility::codec::field{"float_ccy", &market_series_identity::float_ccy},
        utility::codec::field{"float_tenor", &market_series_identity::float_tenor},
        utility::codec::field{"future_contract", &market_series_identity::future_contract},
        utility::codec::field{"fwd_start", &market_series_identity::fwd_start},
        utility::codec::field{"identifier", &market_series_identity::identifier},
        utility::codec::field{"index", &market_series_identity::index},
        utility::codec::field{"index1", &market_series_identity::index1},
        utility::codec::field{"index2", &market_series_identity::index2},
        utility::codec::field{"index_name", &market_series_identity::index_name},
        utility::codec::field{"index_tenor", &market_series_identity::index_tenor},
        utility::codec::field{"index_term", &market_series_identity::index_term},
        utility::codec::field{"spread_offset", &market_series_identity::spread_offset},
        utility::codec::field{"option_type", &market_series_identity::option_type},
        utility::codec::field{"payer_receiver", &market_series_identity::payer_receiver},
        utility::codec::field{"qualifier", &market_series_identity::qualifier},
        utility::codec::field{"quote_name", &market_series_identity::quote_name},
        utility::codec::field{"quote_tag", &market_series_identity::quote_tag},
        utility::codec::field{"rating_name", &market_series_identity::rating_name},
        utility::codec::field{"relative", &market_series_identity::relative},
        utility::codec::field{"running_spread", &market_series_identity::running_spread},
        utility::codec::field{"seasonality_type", &market_series_identity::seasonality_type},
        utility::codec::field{"security_id", &market_series_identity::security_id},
        utility::codec::field{"seniority", &market_series_identity::seniority},
        utility::codec::field{"side", &market_series_identity::side},
        utility::codec::field{"source", &market_series_identity::source},
        utility::codec::field{"surface_id", &market_series_identity::surface_id},
        utility::codec::field{"tenor", &market_series_identity::tenor},
        utility::codec::field{"term", &market_series_identity::term},
        utility::codec::field{"time_unit", &market_series_identity::time_unit},
        utility::codec::field{"underlying_name", &market_series_identity::underlying_name},
        utility::codec::field{"unit_ccy", &market_series_identity::unit_ccy},
    };
}

}

namespace ores::utility::codec {

/**
 * @brief The generated codec: what the wire codec resolves to for this type.
 *
 * The functions are declared here and defined in this type's own translation
 * unit, so a translation unit that only sends the type compiles a declaration
 * rather than a second copy of the encoder.
 */
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::domain::market_series_identity> {
    /**
     * @brief States that this is a generated codec.
     *
     * A test asserts this rather than the encoding, because the encoding is
     * what the type's own case already compares with the reflector: what it
     * cannot see is whether the seam resolved to this specialisation or fell
     * through to the primary template, and a marker that only this
     * specialisation declares is that answer at compile time.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::domain::market_series_identity& v);

    static ores::marketdata::domain::market_series_identity read_json(std::string_view text);

    static std::string write_msgpack(const ores::marketdata::domain::market_series_identity& v);

    static ores::marketdata::domain::market_series_identity
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
