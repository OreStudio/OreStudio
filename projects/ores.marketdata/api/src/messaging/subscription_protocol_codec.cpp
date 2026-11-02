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
#include "ores.marketdata.api/domain/subscription_codec.hpp"
#include "ores.marketdata.api/messaging/subscription_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::subscription_key>::write_json(
    const ores::marketdata::messaging::subscription_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_key
wire_traits<ores::marketdata::messaging::subscription_key>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_key>::write_msgpack(
    const ores::marketdata::messaging::subscription_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_key
wire_traits<ores::marketdata::messaging::subscription_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_write>::write_json(
    const ores::marketdata::messaging::subscription_write& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_write
wire_traits<ores::marketdata::messaging::subscription_write>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_write>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_write>::write_msgpack(
    const ores::marketdata::messaging::subscription_write& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_write
wire_traits<ores::marketdata::messaging::subscription_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_write>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_change>::write_json(
    const ores::marketdata::messaging::subscription_change& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_change
wire_traits<ores::marketdata::messaging::subscription_change>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_change>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_change>::write_msgpack(
    const ores::marketdata::messaging::subscription_change& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_change
wire_traits<ores::marketdata::messaging::subscription_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_change>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_removal>::write_json(
    const ores::marketdata::messaging::subscription_removal& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_removal
wire_traits<ores::marketdata::messaging::subscription_removal>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_removal>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_removal>::write_msgpack(
    const ores::marketdata::messaging::subscription_removal& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_removal
wire_traits<ores::marketdata::messaging::subscription_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_removal>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_lookup>::write_json(
    const ores::marketdata::messaging::subscription_lookup& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_lookup
wire_traits<ores::marketdata::messaging::subscription_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_lookup>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_lookup>::write_msgpack(
    const ores::marketdata::messaging::subscription_lookup& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_lookup
wire_traits<ores::marketdata::messaging::subscription_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_lookup>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscriptions_filter>::write_json(
    const ores::marketdata::messaging::subscriptions_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscriptions_filter
wire_traits<ores::marketdata::messaging::subscriptions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscriptions_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscriptions_filter>::write_msgpack(
    const ores::marketdata::messaging::subscriptions_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscriptions_filter
wire_traits<ores::marketdata::messaging::subscriptions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscriptions_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_event>::write_json(
    const ores::marketdata::messaging::subscription_event& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_event
wire_traits<ores::marketdata::messaging::subscription_event>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_event>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_event>::write_msgpack(
    const ores::marketdata::messaging::subscription_event& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_event
wire_traits<ores::marketdata::messaging::subscription_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_event>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_version_key>::write_json(
    const ores::marketdata::messaging::subscription_version_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_version_key
wire_traits<ores::marketdata::messaging::subscription_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_version_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_version_key>::write_msgpack(
    const ores::marketdata::messaging::subscription_version_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_version_key
wire_traits<ores::marketdata::messaging::subscription_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_version_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::subscription_versions_filter>::write_json(
    const ores::marketdata::messaging::subscription_versions_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::subscription_versions_filter
wire_traits<ores::marketdata::messaging::subscription_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::subscription_versions_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::subscription_versions_filter>::write_msgpack(
    const ores::marketdata::messaging::subscription_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::subscription_versions_filter
wire_traits<ores::marketdata::messaging::subscription_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::subscription_versions_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_subscriptions_request>::write_json(
    const ores::marketdata::messaging::list_subscriptions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_subscriptions_request
wire_traits<ores::marketdata::messaging::list_subscriptions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_subscriptions_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_subscriptions_request>::write_msgpack(
    const ores::marketdata::messaging::list_subscriptions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_subscriptions_request
wire_traits<ores::marketdata::messaging::list_subscriptions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_subscriptions_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_subscriptions_response>::write_json(
    const ores::marketdata::messaging::list_subscriptions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_subscriptions_response
wire_traits<ores::marketdata::messaging::list_subscriptions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_subscriptions_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_subscriptions_response>::write_msgpack(
    const ores::marketdata::messaging::list_subscriptions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_subscriptions_response
wire_traits<ores::marketdata::messaging::list_subscriptions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_subscriptions_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_subscription_request>::write_json(
    const ores::marketdata::messaging::get_subscription_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_subscription_request
wire_traits<ores::marketdata::messaging::get_subscription_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_subscription_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_subscription_request>::write_msgpack(
    const ores::marketdata::messaging::get_subscription_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_subscription_request
wire_traits<ores::marketdata::messaging::get_subscription_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_subscription_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_subscription_response>::write_json(
    const ores::marketdata::messaging::get_subscription_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_subscription_response
wire_traits<ores::marketdata::messaging::get_subscription_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_subscription_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_subscription_response>::write_msgpack(
    const ores::marketdata::messaging::get_subscription_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_subscription_response
wire_traits<ores::marketdata::messaging::get_subscription_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_subscription_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_many_subscriptions_request>::write_json(
    const ores::marketdata::messaging::get_many_subscriptions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_subscriptions_request
wire_traits<ores::marketdata::messaging::get_many_subscriptions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_subscriptions_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_many_subscriptions_request>::write_msgpack(
    const ores::marketdata::messaging::get_many_subscriptions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_subscriptions_request
wire_traits<ores::marketdata::messaging::get_many_subscriptions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_subscriptions_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_many_subscriptions_response>::write_json(
    const ores::marketdata::messaging::get_many_subscriptions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_subscriptions_response
wire_traits<ores::marketdata::messaging::get_many_subscriptions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_subscriptions_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_subscriptions_response>::write_msgpack(
    const ores::marketdata::messaging::get_many_subscriptions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_subscriptions_response
wire_traits<ores::marketdata::messaging::get_many_subscriptions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_subscriptions_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_subscription_request>::write_json(
    const ores::marketdata::messaging::put_subscription_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_subscription_request
wire_traits<ores::marketdata::messaging::put_subscription_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_subscription_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_subscription_request>::write_msgpack(
    const ores::marketdata::messaging::put_subscription_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_subscription_request
wire_traits<ores::marketdata::messaging::put_subscription_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_subscription_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_subscription_response>::write_json(
    const ores::marketdata::messaging::put_subscription_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_subscription_response
wire_traits<ores::marketdata::messaging::put_subscription_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_subscription_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_subscription_response>::write_msgpack(
    const ores::marketdata::messaging::put_subscription_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_subscription_response
wire_traits<ores::marketdata::messaging::put_subscription_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_subscription_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_many_subscriptions_request>::write_json(
    const ores::marketdata::messaging::put_many_subscriptions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_subscriptions_request
wire_traits<ores::marketdata::messaging::put_many_subscriptions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_subscriptions_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_many_subscriptions_request>::write_msgpack(
    const ores::marketdata::messaging::put_many_subscriptions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_subscriptions_request
wire_traits<ores::marketdata::messaging::put_many_subscriptions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_subscriptions_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_many_subscriptions_response>::write_json(
    const ores::marketdata::messaging::put_many_subscriptions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_subscriptions_response
wire_traits<ores::marketdata::messaging::put_many_subscriptions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_subscriptions_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_subscriptions_response>::write_msgpack(
    const ores::marketdata::messaging::put_many_subscriptions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_subscriptions_response
wire_traits<ores::marketdata::messaging::put_many_subscriptions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_subscriptions_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::delete_subscription_request>::write_json(
    const ores::marketdata::messaging::delete_subscription_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_subscription_request
wire_traits<ores::marketdata::messaging::delete_subscription_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_subscription_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::delete_subscription_request>::write_msgpack(
    const ores::marketdata::messaging::delete_subscription_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_subscription_request
wire_traits<ores::marketdata::messaging::delete_subscription_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_subscription_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::delete_subscription_response>::write_json(
    const ores::marketdata::messaging::delete_subscription_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_subscription_response
wire_traits<ores::marketdata::messaging::delete_subscription_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_subscription_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::delete_subscription_response>::write_msgpack(
    const ores::marketdata::messaging::delete_subscription_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_subscription_response
wire_traits<ores::marketdata::messaging::delete_subscription_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_subscription_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::delete_many_subscriptions_request>::write_json(
    const ores::marketdata::messaging::delete_many_subscriptions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_subscriptions_request
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_subscriptions_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_request>::write_msgpack(
    const ores::marketdata::messaging::delete_many_subscriptions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_subscriptions_request
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_many_subscriptions_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_response>::write_json(
    const ores::marketdata::messaging::delete_many_subscriptions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_subscriptions_response
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_subscriptions_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_response>::write_msgpack(
    const ores::marketdata::messaging::delete_many_subscriptions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_subscriptions_response
wire_traits<ores::marketdata::messaging::delete_many_subscriptions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_many_subscriptions_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::list_subscription_versions_request>::write_json(
    const ores::marketdata::messaging::list_subscription_versions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_subscription_versions_request
wire_traits<ores::marketdata::messaging::list_subscription_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_subscription_versions_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_subscription_versions_request>::write_msgpack(
    const ores::marketdata::messaging::list_subscription_versions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_subscription_versions_request
wire_traits<ores::marketdata::messaging::list_subscription_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_subscription_versions_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::list_subscription_versions_response>::write_json(
    const ores::marketdata::messaging::list_subscription_versions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_subscription_versions_response
wire_traits<ores::marketdata::messaging::list_subscription_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_subscription_versions_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::list_subscription_versions_response>::write_msgpack(
    const ores::marketdata::messaging::list_subscription_versions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_subscription_versions_response
wire_traits<ores::marketdata::messaging::list_subscription_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_subscription_versions_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::get_subscription_version_request>::write_json(
    const ores::marketdata::messaging::get_subscription_version_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_subscription_version_request
wire_traits<ores::marketdata::messaging::get_subscription_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_subscription_version_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_subscription_version_request>::write_msgpack(
    const ores::marketdata::messaging::get_subscription_version_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_subscription_version_request
wire_traits<ores::marketdata::messaging::get_subscription_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_subscription_version_request>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::get_subscription_version_response>::write_json(
    const ores::marketdata::messaging::get_subscription_version_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_subscription_version_response
wire_traits<ores::marketdata::messaging::get_subscription_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_subscription_version_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_subscription_version_response>::write_msgpack(
    const ores::marketdata::messaging::get_subscription_version_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_subscription_version_response
wire_traits<ores::marketdata::messaging::get_subscription_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_subscription_version_response>(
        data);
}

}
