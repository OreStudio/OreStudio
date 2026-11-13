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
#include "ores.marketdata.api/domain/market_series_codec.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::series_identity_field>::write_json(
    const ores::marketdata::messaging::series_identity_field& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::series_identity_field
wire_traits<ores::marketdata::messaging::series_identity_field>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::series_identity_field>(text);
}

std::string wire_traits<ores::marketdata::messaging::series_identity_field>::write_msgpack(
    const ores::marketdata::messaging::series_identity_field& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::series_identity_field
wire_traits<ores::marketdata::messaging::series_identity_field>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::series_identity_field>(data);
}

std::string wire_traits<ores::marketdata::messaging::resolve_series_identity_request>::write_json(
    const ores::marketdata::messaging::resolve_series_identity_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::resolve_series_identity_request
wire_traits<ores::marketdata::messaging::resolve_series_identity_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::resolve_series_identity_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::resolve_series_identity_request>::write_msgpack(
    const ores::marketdata::messaging::resolve_series_identity_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::resolve_series_identity_request
wire_traits<ores::marketdata::messaging::resolve_series_identity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::resolve_series_identity_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::resolve_series_identity_response>::write_json(
    const ores::marketdata::messaging::resolve_series_identity_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::resolve_series_identity_response
wire_traits<ores::marketdata::messaging::resolve_series_identity_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::resolve_series_identity_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::resolve_series_identity_response>::write_msgpack(
    const ores::marketdata::messaging::resolve_series_identity_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::resolve_series_identity_response
wire_traits<ores::marketdata::messaging::resolve_series_identity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::resolve_series_identity_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_field_spec>::write_json(
    const ores::marketdata::messaging::oresmd_field_spec& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::oresmd_field_spec
wire_traits<ores::marketdata::messaging::oresmd_field_spec>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::oresmd_field_spec>(text);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_field_spec>::write_msgpack(
    const ores::marketdata::messaging::oresmd_field_spec& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::oresmd_field_spec
wire_traits<ores::marketdata::messaging::oresmd_field_spec>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::oresmd_field_spec>(data);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_instrument>::write_json(
    const ores::marketdata::messaging::oresmd_instrument& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::oresmd_instrument
wire_traits<ores::marketdata::messaging::oresmd_instrument>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::oresmd_instrument>(text);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_instrument>::write_msgpack(
    const ores::marketdata::messaging::oresmd_instrument& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::oresmd_instrument
wire_traits<ores::marketdata::messaging::oresmd_instrument>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::oresmd_instrument>(data);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_index_field>::write_json(
    const ores::marketdata::messaging::oresmd_index_field& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::oresmd_index_field
wire_traits<ores::marketdata::messaging::oresmd_index_field>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::oresmd_index_field>(text);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_index_field>::write_msgpack(
    const ores::marketdata::messaging::oresmd_index_field& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::oresmd_index_field
wire_traits<ores::marketdata::messaging::oresmd_index_field>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::oresmd_index_field>(data);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_index_family>::write_json(
    const ores::marketdata::messaging::oresmd_index_family& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::oresmd_index_family
wire_traits<ores::marketdata::messaging::oresmd_index_family>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::oresmd_index_family>(text);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_index_family>::write_msgpack(
    const ores::marketdata::messaging::oresmd_index_family& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::oresmd_index_family
wire_traits<ores::marketdata::messaging::oresmd_index_family>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::oresmd_index_family>(data);
}

std::string wire_traits<ores::marketdata::messaging::describe_oresmd_schema_request>::write_json(
    const ores::marketdata::messaging::describe_oresmd_schema_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::describe_oresmd_schema_request
wire_traits<ores::marketdata::messaging::describe_oresmd_schema_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::describe_oresmd_schema_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::describe_oresmd_schema_request>::write_msgpack(
    const ores::marketdata::messaging::describe_oresmd_schema_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::describe_oresmd_schema_request
wire_traits<ores::marketdata::messaging::describe_oresmd_schema_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::describe_oresmd_schema_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::describe_oresmd_schema_response>::write_json(
    const ores::marketdata::messaging::describe_oresmd_schema_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::describe_oresmd_schema_response
wire_traits<ores::marketdata::messaging::describe_oresmd_schema_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::describe_oresmd_schema_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::describe_oresmd_schema_response>::write_msgpack(
    const ores::marketdata::messaging::describe_oresmd_schema_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::describe_oresmd_schema_response
wire_traits<ores::marketdata::messaging::describe_oresmd_schema_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::describe_oresmd_schema_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_uri_field>::write_json(
    const ores::marketdata::messaging::oresmd_uri_field& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::oresmd_uri_field
wire_traits<ores::marketdata::messaging::oresmd_uri_field>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::oresmd_uri_field>(text);
}

std::string wire_traits<ores::marketdata::messaging::oresmd_uri_field>::write_msgpack(
    const ores::marketdata::messaging::oresmd_uri_field& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::oresmd_uri_field
wire_traits<ores::marketdata::messaging::oresmd_uri_field>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::oresmd_uri_field>(data);
}

std::string wire_traits<ores::marketdata::messaging::parse_oresmd_uri_request>::write_json(
    const ores::marketdata::messaging::parse_oresmd_uri_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::parse_oresmd_uri_request
wire_traits<ores::marketdata::messaging::parse_oresmd_uri_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::parse_oresmd_uri_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::parse_oresmd_uri_request>::write_msgpack(
    const ores::marketdata::messaging::parse_oresmd_uri_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::parse_oresmd_uri_request
wire_traits<ores::marketdata::messaging::parse_oresmd_uri_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::parse_oresmd_uri_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::parse_oresmd_uri_response>::write_json(
    const ores::marketdata::messaging::parse_oresmd_uri_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::parse_oresmd_uri_response
wire_traits<ores::marketdata::messaging::parse_oresmd_uri_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::parse_oresmd_uri_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::parse_oresmd_uri_response>::write_msgpack(
    const ores::marketdata::messaging::parse_oresmd_uri_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::parse_oresmd_uri_response
wire_traits<ores::marketdata::messaging::parse_oresmd_uri_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::parse_oresmd_uri_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::compose_oresmd_uri_request>::write_json(
    const ores::marketdata::messaging::compose_oresmd_uri_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::compose_oresmd_uri_request
wire_traits<ores::marketdata::messaging::compose_oresmd_uri_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::compose_oresmd_uri_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::compose_oresmd_uri_request>::write_msgpack(
    const ores::marketdata::messaging::compose_oresmd_uri_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::compose_oresmd_uri_request
wire_traits<ores::marketdata::messaging::compose_oresmd_uri_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::compose_oresmd_uri_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::compose_oresmd_uri_response>::write_json(
    const ores::marketdata::messaging::compose_oresmd_uri_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::compose_oresmd_uri_response
wire_traits<ores::marketdata::messaging::compose_oresmd_uri_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::compose_oresmd_uri_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::compose_oresmd_uri_response>::write_msgpack(
    const ores::marketdata::messaging::compose_oresmd_uri_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::compose_oresmd_uri_response
wire_traits<ores::marketdata::messaging::compose_oresmd_uri_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::compose_oresmd_uri_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_series_identities_request>::write_json(
    const ores::marketdata::messaging::list_series_identities_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_series_identities_request
wire_traits<ores::marketdata::messaging::list_series_identities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_series_identities_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_series_identities_request>::write_msgpack(
    const ores::marketdata::messaging::list_series_identities_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_series_identities_request
wire_traits<ores::marketdata::messaging::list_series_identities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_series_identities_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::listed_series>::write_json(
    const ores::marketdata::messaging::listed_series& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::listed_series
wire_traits<ores::marketdata::messaging::listed_series>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::listed_series>(text);
}

std::string wire_traits<ores::marketdata::messaging::listed_series>::write_msgpack(
    const ores::marketdata::messaging::listed_series& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::listed_series
wire_traits<ores::marketdata::messaging::listed_series>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::listed_series>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_series_identities_response>::write_json(
    const ores::marketdata::messaging::list_series_identities_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_series_identities_response
wire_traits<ores::marketdata::messaging::list_series_identities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_series_identities_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_series_identities_response>::write_msgpack(
    const ores::marketdata::messaging::list_series_identities_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_series_identities_response
wire_traits<ores::marketdata::messaging::list_series_identities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_series_identities_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::crm_rate_item>::write_json(
    const ores::marketdata::messaging::crm_rate_item& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::crm_rate_item
wire_traits<ores::marketdata::messaging::crm_rate_item>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::crm_rate_item>(text);
}

std::string wire_traits<ores::marketdata::messaging::crm_rate_item>::write_msgpack(
    const ores::marketdata::messaging::crm_rate_item& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::crm_rate_item
wire_traits<ores::marketdata::messaging::crm_rate_item>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::crm_rate_item>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rate_request>::write_json(
    const ores::marketdata::messaging::get_crm_rate_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_crm_rate_request
wire_traits<ores::marketdata::messaging::get_crm_rate_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_crm_rate_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rate_request>::write_msgpack(
    const ores::marketdata::messaging::get_crm_rate_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_crm_rate_request
wire_traits<ores::marketdata::messaging::get_crm_rate_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_crm_rate_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rate_response>::write_json(
    const ores::marketdata::messaging::get_crm_rate_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_crm_rate_response
wire_traits<ores::marketdata::messaging::get_crm_rate_response>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_crm_rate_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rate_response>::write_msgpack(
    const ores::marketdata::messaging::get_crm_rate_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_crm_rate_response
wire_traits<ores::marketdata::messaging::get_crm_rate_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_crm_rate_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rates_request>::write_json(
    const ores::marketdata::messaging::get_crm_rates_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_crm_rates_request
wire_traits<ores::marketdata::messaging::get_crm_rates_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_crm_rates_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rates_request>::write_msgpack(
    const ores::marketdata::messaging::get_crm_rates_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_crm_rates_request
wire_traits<ores::marketdata::messaging::get_crm_rates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_crm_rates_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rates_response>::write_json(
    const ores::marketdata::messaging::get_crm_rates_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_crm_rates_response
wire_traits<ores::marketdata::messaging::get_crm_rates_response>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_crm_rates_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_crm_rates_response>::write_msgpack(
    const ores::marketdata::messaging::get_crm_rates_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_crm_rates_response
wire_traits<ores::marketdata::messaging::get_crm_rates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_crm_rates_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::republish_curve_request>::write_json(
    const ores::marketdata::messaging::republish_curve_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::republish_curve_request
wire_traits<ores::marketdata::messaging::republish_curve_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::republish_curve_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::republish_curve_request>::write_msgpack(
    const ores::marketdata::messaging::republish_curve_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::republish_curve_request
wire_traits<ores::marketdata::messaging::republish_curve_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::republish_curve_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::republish_curve_response>::write_json(
    const ores::marketdata::messaging::republish_curve_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::republish_curve_response
wire_traits<ores::marketdata::messaging::republish_curve_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::republish_curve_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::republish_curve_response>::write_msgpack(
    const ores::marketdata::messaging::republish_curve_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::republish_curve_response
wire_traits<ores::marketdata::messaging::republish_curve_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::republish_curve_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::compute_curve_request>::write_json(
    const ores::marketdata::messaging::compute_curve_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::compute_curve_request
wire_traits<ores::marketdata::messaging::compute_curve_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::compute_curve_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::compute_curve_request>::write_msgpack(
    const ores::marketdata::messaging::compute_curve_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::compute_curve_request
wire_traits<ores::marketdata::messaging::compute_curve_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::compute_curve_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::computed_curve_point>::write_json(
    const ores::marketdata::messaging::computed_curve_point& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::computed_curve_point
wire_traits<ores::marketdata::messaging::computed_curve_point>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::computed_curve_point>(text);
}

std::string wire_traits<ores::marketdata::messaging::computed_curve_point>::write_msgpack(
    const ores::marketdata::messaging::computed_curve_point& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::computed_curve_point
wire_traits<ores::marketdata::messaging::computed_curve_point>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::computed_curve_point>(data);
}

std::string wire_traits<ores::marketdata::messaging::compute_curve_response>::write_json(
    const ores::marketdata::messaging::compute_curve_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::compute_curve_response
wire_traits<ores::marketdata::messaging::compute_curve_response>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::compute_curve_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::compute_curve_response>::write_msgpack(
    const ores::marketdata::messaging::compute_curve_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::compute_curve_response
wire_traits<ores::marketdata::messaging::compute_curve_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::compute_curve_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::publish_due_curves_request>::write_json(
    const ores::marketdata::messaging::publish_due_curves_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::publish_due_curves_request
wire_traits<ores::marketdata::messaging::publish_due_curves_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::publish_due_curves_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::publish_due_curves_request>::write_msgpack(
    const ores::marketdata::messaging::publish_due_curves_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::publish_due_curves_request
wire_traits<ores::marketdata::messaging::publish_due_curves_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::publish_due_curves_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::curve_republish_outcome>::write_json(
    const ores::marketdata::messaging::curve_republish_outcome& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::curve_republish_outcome
wire_traits<ores::marketdata::messaging::curve_republish_outcome>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::curve_republish_outcome>(text);
}

std::string wire_traits<ores::marketdata::messaging::curve_republish_outcome>::write_msgpack(
    const ores::marketdata::messaging::curve_republish_outcome& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::curve_republish_outcome
wire_traits<ores::marketdata::messaging::curve_republish_outcome>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::curve_republish_outcome>(data);
}

std::string wire_traits<ores::marketdata::messaging::publish_due_curves_response>::write_json(
    const ores::marketdata::messaging::publish_due_curves_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::publish_due_curves_response
wire_traits<ores::marketdata::messaging::publish_due_curves_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::publish_due_curves_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::publish_due_curves_response>::write_msgpack(
    const ores::marketdata::messaging::publish_due_curves_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::publish_due_curves_response
wire_traits<ores::marketdata::messaging::publish_due_curves_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::publish_due_curves_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::series_slice_instant>::write_json(
    const ores::marketdata::messaging::series_slice_instant& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::series_slice_instant
wire_traits<ores::marketdata::messaging::series_slice_instant>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::series_slice_instant>(text);
}

std::string wire_traits<ores::marketdata::messaging::series_slice_instant>::write_msgpack(
    const ores::marketdata::messaging::series_slice_instant& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::series_slice_instant
wire_traits<ores::marketdata::messaging::series_slice_instant>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::series_slice_instant>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_series_slice_request>::write_json(
    const ores::marketdata::messaging::get_series_slice_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_series_slice_request
wire_traits<ores::marketdata::messaging::get_series_slice_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_series_slice_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_series_slice_request>::write_msgpack(
    const ores::marketdata::messaging::get_series_slice_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_series_slice_request
wire_traits<ores::marketdata::messaging::get_series_slice_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_series_slice_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_series_slice_response>::write_json(
    const ores::marketdata::messaging::get_series_slice_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_series_slice_response
wire_traits<ores::marketdata::messaging::get_series_slice_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_series_slice_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_series_slice_response>::write_msgpack(
    const ores::marketdata::messaging::get_series_slice_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_series_slice_response
wire_traits<ores::marketdata::messaging::get_series_slice_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_series_slice_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::evolution_node>::write_json(
    const ores::marketdata::messaging::evolution_node& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::evolution_node
wire_traits<ores::marketdata::messaging::evolution_node>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::evolution_node>(text);
}

std::string wire_traits<ores::marketdata::messaging::evolution_node>::write_msgpack(
    const ores::marketdata::messaging::evolution_node& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::evolution_node
wire_traits<ores::marketdata::messaging::evolution_node>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::evolution_node>(data);
}

std::string wire_traits<ores::marketdata::messaging::evolution_instant>::write_json(
    const ores::marketdata::messaging::evolution_instant& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::evolution_instant
wire_traits<ores::marketdata::messaging::evolution_instant>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::evolution_instant>(text);
}

std::string wire_traits<ores::marketdata::messaging::evolution_instant>::write_msgpack(
    const ores::marketdata::messaging::evolution_instant& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::evolution_instant
wire_traits<ores::marketdata::messaging::evolution_instant>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::evolution_instant>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_series_evolution_request>::write_json(
    const ores::marketdata::messaging::get_series_evolution_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_series_evolution_request
wire_traits<ores::marketdata::messaging::get_series_evolution_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_series_evolution_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_series_evolution_request>::write_msgpack(
    const ores::marketdata::messaging::get_series_evolution_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_series_evolution_request
wire_traits<ores::marketdata::messaging::get_series_evolution_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_series_evolution_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_series_evolution_response>::write_json(
    const ores::marketdata::messaging::get_series_evolution_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_series_evolution_response
wire_traits<ores::marketdata::messaging::get_series_evolution_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_series_evolution_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_series_evolution_response>::write_msgpack(
    const ores::marketdata::messaging::get_series_evolution_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_series_evolution_response
wire_traits<ores::marketdata::messaging::get_series_evolution_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_series_evolution_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::point_provenance>::write_json(
    const ores::marketdata::messaging::point_provenance& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::point_provenance
wire_traits<ores::marketdata::messaging::point_provenance>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::point_provenance>(text);
}

std::string wire_traits<ores::marketdata::messaging::point_provenance>::write_msgpack(
    const ores::marketdata::messaging::point_provenance& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::point_provenance
wire_traits<ores::marketdata::messaging::point_provenance>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::point_provenance>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_curve_snapshot_request>::write_json(
    const ores::marketdata::messaging::get_curve_snapshot_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_curve_snapshot_request
wire_traits<ores::marketdata::messaging::get_curve_snapshot_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_curve_snapshot_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_curve_snapshot_request>::write_msgpack(
    const ores::marketdata::messaging::get_curve_snapshot_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_curve_snapshot_request
wire_traits<ores::marketdata::messaging::get_curve_snapshot_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_curve_snapshot_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_curve_snapshot_response>::write_json(
    const ores::marketdata::messaging::get_curve_snapshot_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_curve_snapshot_response
wire_traits<ores::marketdata::messaging::get_curve_snapshot_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_curve_snapshot_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_curve_snapshot_response>::write_msgpack(
    const ores::marketdata::messaging::get_curve_snapshot_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_curve_snapshot_response
wire_traits<ores::marketdata::messaging::get_curve_snapshot_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_curve_snapshot_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::import_market_data_request>::write_json(
    const ores::marketdata::messaging::import_market_data_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::import_market_data_request
wire_traits<ores::marketdata::messaging::import_market_data_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::import_market_data_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::import_market_data_request>::write_msgpack(
    const ores::marketdata::messaging::import_market_data_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::import_market_data_request
wire_traits<ores::marketdata::messaging::import_market_data_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::import_market_data_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::import_market_data_response>::write_json(
    const ores::marketdata::messaging::import_market_data_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::import_market_data_response
wire_traits<ores::marketdata::messaging::import_market_data_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::import_market_data_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::import_market_data_response>::write_msgpack(
    const ores::marketdata::messaging::import_market_data_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::import_market_data_response
wire_traits<ores::marketdata::messaging::import_market_data_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::import_market_data_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::feed_kind_counts>::write_json(
    const ores::marketdata::messaging::feed_kind_counts& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::feed_kind_counts
wire_traits<ores::marketdata::messaging::feed_kind_counts>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::feed_kind_counts>(text);
}

std::string wire_traits<ores::marketdata::messaging::feed_kind_counts>::write_msgpack(
    const ores::marketdata::messaging::feed_kind_counts& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::feed_kind_counts
wire_traits<ores::marketdata::messaging::feed_kind_counts>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::feed_kind_counts>(data);
}

std::string wire_traits<ores::marketdata::messaging::start_feeds_under_folder_request>::write_json(
    const ores::marketdata::messaging::start_feeds_under_folder_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::start_feeds_under_folder_request
wire_traits<ores::marketdata::messaging::start_feeds_under_folder_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::start_feeds_under_folder_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::start_feeds_under_folder_request>::write_msgpack(
    const ores::marketdata::messaging::start_feeds_under_folder_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::start_feeds_under_folder_request
wire_traits<ores::marketdata::messaging::start_feeds_under_folder_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::start_feeds_under_folder_request>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::start_feeds_under_folder_response>::write_json(
    const ores::marketdata::messaging::start_feeds_under_folder_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::start_feeds_under_folder_response
wire_traits<ores::marketdata::messaging::start_feeds_under_folder_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::start_feeds_under_folder_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::start_feeds_under_folder_response>::write_msgpack(
    const ores::marketdata::messaging::start_feeds_under_folder_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::start_feeds_under_folder_response
wire_traits<ores::marketdata::messaging::start_feeds_under_folder_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::start_feeds_under_folder_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_request>::write_json(
    const ores::marketdata::messaging::stop_feeds_under_folder_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::stop_feeds_under_folder_request
wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::stop_feeds_under_folder_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_request>::write_msgpack(
    const ores::marketdata::messaging::stop_feeds_under_folder_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::stop_feeds_under_folder_request
wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::stop_feeds_under_folder_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_response>::write_json(
    const ores::marketdata::messaging::stop_feeds_under_folder_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::stop_feeds_under_folder_response
wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::stop_feeds_under_folder_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_response>::write_msgpack(
    const ores::marketdata::messaging::stop_feeds_under_folder_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::stop_feeds_under_folder_response
wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::stop_feeds_under_folder_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_request>::write_json(
    const ores::marketdata::messaging::export_market_data_to_storage_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::export_market_data_to_storage_request
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::export_market_data_to_storage_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_request>::write_msgpack(
    const ores::marketdata::messaging::export_market_data_to_storage_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::export_market_data_to_storage_request
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::export_market_data_to_storage_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_response>::write_json(
    const ores::marketdata::messaging::export_market_data_to_storage_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::export_market_data_to_storage_response
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::export_market_data_to_storage_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_response>::write_msgpack(
    const ores::marketdata::messaging::export_market_data_to_storage_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::export_market_data_to_storage_response
wire_traits<ores::marketdata::messaging::export_market_data_to_storage_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::export_market_data_to_storage_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::backfill_series_identity_request>::write_json(
    const ores::marketdata::messaging::backfill_series_identity_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::backfill_series_identity_request
wire_traits<ores::marketdata::messaging::backfill_series_identity_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::backfill_series_identity_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::backfill_series_identity_request>::write_msgpack(
    const ores::marketdata::messaging::backfill_series_identity_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::backfill_series_identity_request
wire_traits<ores::marketdata::messaging::backfill_series_identity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::backfill_series_identity_request>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::backfill_series_identity_response>::write_json(
    const ores::marketdata::messaging::backfill_series_identity_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::backfill_series_identity_response
wire_traits<ores::marketdata::messaging::backfill_series_identity_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::backfill_series_identity_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::backfill_series_identity_response>::write_msgpack(
    const ores::marketdata::messaging::backfill_series_identity_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::backfill_series_identity_response
wire_traits<ores::marketdata::messaging::backfill_series_identity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::backfill_series_identity_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::key_manual_point_request>::write_json(
    const ores::marketdata::messaging::key_manual_point_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::key_manual_point_request
wire_traits<ores::marketdata::messaging::key_manual_point_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::key_manual_point_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::key_manual_point_request>::write_msgpack(
    const ores::marketdata::messaging::key_manual_point_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::key_manual_point_request
wire_traits<ores::marketdata::messaging::key_manual_point_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::key_manual_point_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::key_manual_point_response>::write_json(
    const ores::marketdata::messaging::key_manual_point_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::key_manual_point_response
wire_traits<ores::marketdata::messaging::key_manual_point_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::key_manual_point_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::key_manual_point_response>::write_msgpack(
    const ores::marketdata::messaging::key_manual_point_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::key_manual_point_response
wire_traits<ores::marketdata::messaging::key_manual_point_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::key_manual_point_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::clear_manual_point_request>::write_json(
    const ores::marketdata::messaging::clear_manual_point_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::clear_manual_point_request
wire_traits<ores::marketdata::messaging::clear_manual_point_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::clear_manual_point_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::clear_manual_point_request>::write_msgpack(
    const ores::marketdata::messaging::clear_manual_point_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::clear_manual_point_request
wire_traits<ores::marketdata::messaging::clear_manual_point_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::clear_manual_point_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::clear_manual_point_response>::write_json(
    const ores::marketdata::messaging::clear_manual_point_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::clear_manual_point_response
wire_traits<ores::marketdata::messaging::clear_manual_point_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::clear_manual_point_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::clear_manual_point_response>::write_msgpack(
    const ores::marketdata::messaging::clear_manual_point_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::clear_manual_point_response
wire_traits<ores::marketdata::messaging::clear_manual_point_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::clear_manual_point_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::export_market_data_request>::write_json(
    const ores::marketdata::messaging::export_market_data_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::export_market_data_request
wire_traits<ores::marketdata::messaging::export_market_data_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::export_market_data_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::export_market_data_request>::write_msgpack(
    const ores::marketdata::messaging::export_market_data_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::export_market_data_request
wire_traits<ores::marketdata::messaging::export_market_data_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::export_market_data_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::export_market_data_response>::write_json(
    const ores::marketdata::messaging::export_market_data_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::export_market_data_response
wire_traits<ores::marketdata::messaging::export_market_data_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::export_market_data_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::export_market_data_response>::write_msgpack(
    const ores::marketdata::messaging::export_market_data_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::export_market_data_response
wire_traits<ores::marketdata::messaging::export_market_data_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::export_market_data_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_tick>::write_json(
    const ores::marketdata::messaging::market_tick& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_tick
wire_traits<ores::marketdata::messaging::market_tick>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_tick>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_tick>::write_msgpack(
    const ores::marketdata::messaging::market_tick& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_tick
wire_traits<ores::marketdata::messaging::market_tick>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_tick>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_series_ticks_request>::write_json(
    const ores::marketdata::messaging::get_series_ticks_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_series_ticks_request
wire_traits<ores::marketdata::messaging::get_series_ticks_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_series_ticks_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_series_ticks_request>::write_msgpack(
    const ores::marketdata::messaging::get_series_ticks_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_series_ticks_request
wire_traits<ores::marketdata::messaging::get_series_ticks_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_series_ticks_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_series_ticks_response>::write_json(
    const ores::marketdata::messaging::get_series_ticks_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_series_ticks_response
wire_traits<ores::marketdata::messaging::get_series_ticks_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_series_ticks_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_series_ticks_response>::write_msgpack(
    const ores::marketdata::messaging::get_series_ticks_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_series_ticks_response
wire_traits<ores::marketdata::messaging::get_series_ticks_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_series_ticks_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::store_market_state_request>::write_json(
    const ores::marketdata::messaging::store_market_state_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::store_market_state_request
wire_traits<ores::marketdata::messaging::store_market_state_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::store_market_state_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::store_market_state_request>::write_msgpack(
    const ores::marketdata::messaging::store_market_state_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::store_market_state_request
wire_traits<ores::marketdata::messaging::store_market_state_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::store_market_state_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::store_market_state_response>::write_json(
    const ores::marketdata::messaging::store_market_state_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::store_market_state_response
wire_traits<ores::marketdata::messaging::store_market_state_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::store_market_state_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::store_market_state_response>::write_msgpack(
    const ores::marketdata::messaging::store_market_state_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::store_market_state_response
wire_traits<ores::marketdata::messaging::store_market_state_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::store_market_state_response>(data);
}

}
