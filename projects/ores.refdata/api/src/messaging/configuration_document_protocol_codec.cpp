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
 * Template: cpp_protocol_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.api/domain/base_correlation_config_codec.hpp"
#include "ores.refdata.api/domain/bond_future_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/cap_floor_proxy_config_codec.hpp"
#include "ores.refdata.api/domain/cap_floor_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/cds_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/cds_volatility_term_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_basis_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_config_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_cross_currency_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_direct_codec.hpp"
#include "ores.refdata.api/domain/commodity_curve_interpolation_codec.hpp"
#include "ores.refdata.api/domain/commodity_price_segment_codec.hpp"
#include "ores.refdata.api/domain/commodity_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/currency_pair_codec.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_codec.hpp"
#include "ores.refdata.api/domain/curve_bootstrap_config_codec.hpp"
#include "ores.refdata.api/domain/curve_configuration_codec.hpp"
#include "ores.refdata.api/domain/curve_configuration_section_codec.hpp"
#include "ores.refdata.api/domain/curve_correlation_config_codec.hpp"
#include "ores.refdata.api/domain/curve_definition_codec.hpp"
#include "ores.refdata.api/domain/curve_global_report_codec.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_codec.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_parameter_codec.hpp"
#include "ores.refdata.api/domain/curve_quote_codec.hpp"
#include "ores.refdata.api/domain/curve_report_configuration_codec.hpp"
#include "ores.refdata.api/domain/curve_security_config_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_convention_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_cross_currency_terms_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_curve_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_discount_ratio_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_extrapolation_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_ibor_fallback_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_pillar_rule_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_projection_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_reference_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_tenor_basis_curves_codec.hpp"
#include "ores.refdata.api/domain/curve_segment_weighted_average_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_constant_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_curve_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_delta_surface_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_kind_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_moneyness_surface_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_proxy_surface_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_smile_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_smile_parameter_codec.hpp"
#include "ores.refdata.api/domain/curve_volatility_strike_surface_codec.hpp"
#include "ores.refdata.api/domain/default_curve_benchmark_codec.hpp"
#include "ores.refdata.api/domain/default_curve_cds_terms_codec.hpp"
#include "ores.refdata.api/domain/default_curve_config_codec.hpp"
#include "ores.refdata.api/domain/default_curve_configuration_codec.hpp"
#include "ores.refdata.api/domain/default_curve_market_terms_codec.hpp"
#include "ores.refdata.api/domain/default_curve_transition_matrix_codec.hpp"
#include "ores.refdata.api/domain/default_curve_yield_reinterpretation_codec.hpp"
#include "ores.refdata.api/domain/equity_curve_config_codec.hpp"
#include "ores.refdata.api/domain/equity_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/fx_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/fx_volatility_smile_codec.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_strikes_codec.hpp"
#include "ores.refdata.api/domain/inflation_curve_config_codec.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_codec.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_factor_codec.hpp"
#include "ores.refdata.api/domain/intraday_power_curve_config_codec.hpp"
#include "ores.refdata.api/domain/swaption_proxy_config_codec.hpp"
#include "ores.refdata.api/domain/swaption_volatility_config_codec.hpp"
#include "ores.refdata.api/domain/swaption_volatility_smile_codec.hpp"
#include "ores.refdata.api/domain/yield_curve_config_codec.hpp"
#include "ores.refdata.api/domain/yield_volatility_config_codec.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::fx_convention>::write_json(
    const ores::refdata::messaging::fx_convention& v) {
    return object::write_json(v);
}

ores::refdata::messaging::fx_convention
wire_traits<ores::refdata::messaging::fx_convention>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::fx_convention>(text);
}

std::string wire_traits<ores::refdata::messaging::fx_convention>::write_msgpack(
    const ores::refdata::messaging::fx_convention& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::fx_convention
wire_traits<ores::refdata::messaging::fx_convention>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::fx_convention>(data);
}

std::string wire_traits<ores::refdata::messaging::curve_configuration_document>::write_json(
    const ores::refdata::messaging::curve_configuration_document& v) {
    return object::write_json(v);
}

ores::refdata::messaging::curve_configuration_document
wire_traits<ores::refdata::messaging::curve_configuration_document>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::curve_configuration_document>(text);
}

std::string wire_traits<ores::refdata::messaging::curve_configuration_document>::write_msgpack(
    const ores::refdata::messaging::curve_configuration_document& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::curve_configuration_document
wire_traits<ores::refdata::messaging::curve_configuration_document>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::curve_configuration_document>(data);
}

std::string
wire_traits<ores::refdata::messaging::save_curve_configuration_document_request>::write_json(
    const ores::refdata::messaging::save_curve_configuration_document_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::save_curve_configuration_document_request
wire_traits<ores::refdata::messaging::save_curve_configuration_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::save_curve_configuration_document_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::save_curve_configuration_document_request>::write_msgpack(
    const ores::refdata::messaging::save_curve_configuration_document_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::save_curve_configuration_document_request
wire_traits<ores::refdata::messaging::save_curve_configuration_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::save_curve_configuration_document_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::save_curve_configuration_document_response>::write_json(
    const ores::refdata::messaging::save_curve_configuration_document_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::save_curve_configuration_document_response
wire_traits<ores::refdata::messaging::save_curve_configuration_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::save_curve_configuration_document_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::save_curve_configuration_document_response>::write_msgpack(
    const ores::refdata::messaging::save_curve_configuration_document_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::save_curve_configuration_document_response
wire_traits<ores::refdata::messaging::save_curve_configuration_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::save_curve_configuration_document_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::get_curve_configuration_document_request>::write_json(
    const ores::refdata::messaging::get_curve_configuration_document_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_curve_configuration_document_request
wire_traits<ores::refdata::messaging::get_curve_configuration_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_curve_configuration_document_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_curve_configuration_document_request>::write_msgpack(
    const ores::refdata::messaging::get_curve_configuration_document_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_curve_configuration_document_request
wire_traits<ores::refdata::messaging::get_curve_configuration_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_curve_configuration_document_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_curve_configuration_document_response>::write_json(
    const ores::refdata::messaging::get_curve_configuration_document_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_curve_configuration_document_response
wire_traits<ores::refdata::messaging::get_curve_configuration_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_curve_configuration_document_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_curve_configuration_document_response>::write_msgpack(
    const ores::refdata::messaging::get_curve_configuration_document_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_curve_configuration_document_response
wire_traits<ores::refdata::messaging::get_curve_configuration_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::get_curve_configuration_document_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_request>::write_json(
    const ores::refdata::messaging::delete_curve_configuration_document_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_curve_configuration_document_request
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_curve_configuration_document_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_request>::write_msgpack(
    const ores::refdata::messaging::delete_curve_configuration_document_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_curve_configuration_document_request
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_curve_configuration_document_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_response>::write_json(
    const ores::refdata::messaging::delete_curve_configuration_document_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_curve_configuration_document_response
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::delete_curve_configuration_document_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_response>::write_msgpack(
    const ores::refdata::messaging::delete_curve_configuration_document_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_curve_configuration_document_response
wire_traits<ores::refdata::messaging::delete_curve_configuration_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_curve_configuration_document_response>(data);
}

}
