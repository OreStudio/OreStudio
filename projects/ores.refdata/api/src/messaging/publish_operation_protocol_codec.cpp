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
#include "ores.refdata.api/messaging/publish_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string
wire_traits<ores::refdata::messaging::publish_trading_account_types_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_trading_account_types_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_trading_account_types_from_dq_request
wire_traits<ores::refdata::messaging::publish_trading_account_types_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_trading_account_types_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_trading_account_types_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_trading_account_types_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_trading_account_types_from_dq_request
wire_traits<ores::refdata::messaging::publish_trading_account_types_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_trading_account_types_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_asset_classes_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_asset_classes_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_asset_classes_from_dq_request
wire_traits<ores::refdata::messaging::publish_asset_classes_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_asset_classes_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_asset_classes_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_asset_classes_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_asset_classes_from_dq_request
wire_traits<ores::refdata::messaging::publish_asset_classes_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_asset_classes_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_asset_measures_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_asset_measures_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_asset_measures_from_dq_request
wire_traits<ores::refdata::messaging::publish_asset_measures_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_asset_measures_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_asset_measures_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_asset_measures_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_asset_measures_from_dq_request
wire_traits<ores::refdata::messaging::publish_asset_measures_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_asset_measures_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_benchmark_rates_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_benchmark_rates_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_benchmark_rates_from_dq_request
wire_traits<ores::refdata::messaging::publish_benchmark_rates_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_benchmark_rates_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_benchmark_rates_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_benchmark_rates_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_benchmark_rates_from_dq_request
wire_traits<ores::refdata::messaging::publish_benchmark_rates_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_benchmark_rates_from_dq_request>(
        data);
}

std::string wire_traits<ores::refdata::messaging::publish_books_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_books_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_books_from_dq_request
wire_traits<ores::refdata::messaging::publish_books_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_books_from_dq_request>(text);
}

std::string wire_traits<ores::refdata::messaging::publish_books_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_books_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_books_from_dq_request
wire_traits<ores::refdata::messaging::publish_books_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_books_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_business_centres_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_business_centres_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_business_centres_from_dq_request
wire_traits<ores::refdata::messaging::publish_business_centres_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_business_centres_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_business_centres_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_business_centres_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_business_centres_from_dq_request
wire_traits<ores::refdata::messaging::publish_business_centres_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_business_centres_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_business_processes_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_business_processes_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_business_processes_from_dq_request
wire_traits<ores::refdata::messaging::publish_business_processes_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_business_processes_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_business_processes_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_business_processes_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_business_processes_from_dq_request
wire_traits<ores::refdata::messaging::publish_business_processes_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_business_processes_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_business_units_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_business_units_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_business_units_from_dq_request
wire_traits<ores::refdata::messaging::publish_business_units_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_business_units_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_business_units_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_business_units_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_business_units_from_dq_request
wire_traits<ores::refdata::messaging::publish_business_units_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_business_units_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_calendar_events_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_calendar_events_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_calendar_events_from_dq_request
wire_traits<ores::refdata::messaging::publish_calendar_events_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_calendar_events_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_calendar_events_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_calendar_events_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_calendar_events_from_dq_request
wire_traits<ores::refdata::messaging::publish_calendar_events_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_calendar_events_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_calendar_types_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_calendar_types_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_calendar_types_from_dq_request
wire_traits<ores::refdata::messaging::publish_calendar_types_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_calendar_types_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_calendar_types_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_calendar_types_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_calendar_types_from_dq_request
wire_traits<ores::refdata::messaging::publish_calendar_types_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_calendar_types_from_dq_request>(
        data);
}

std::string wire_traits<ores::refdata::messaging::publish_calendars_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_calendars_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_calendars_from_dq_request
wire_traits<ores::refdata::messaging::publish_calendars_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_calendars_from_dq_request>(text);
}

std::string wire_traits<ores::refdata::messaging::publish_calendars_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_calendars_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_calendars_from_dq_request
wire_traits<ores::refdata::messaging::publish_calendars_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_calendars_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_cashflow_types_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_cashflow_types_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_cashflow_types_from_dq_request
wire_traits<ores::refdata::messaging::publish_cashflow_types_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_cashflow_types_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_cashflow_types_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_cashflow_types_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_cashflow_types_from_dq_request
wire_traits<ores::refdata::messaging::publish_cashflow_types_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_cashflow_types_from_dq_request>(
        data);
}

std::string wire_traits<ores::refdata::messaging::publish_commodities_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_commodities_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_commodities_from_dq_request
wire_traits<ores::refdata::messaging::publish_commodities_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_commodities_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_commodities_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_commodities_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_commodities_from_dq_request
wire_traits<ores::refdata::messaging::publish_commodities_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_commodities_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_legal_entities_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_legal_entities_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_legal_entities_from_dq_request
wire_traits<ores::refdata::messaging::publish_legal_entities_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_legal_entities_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_legal_entities_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_legal_entities_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_legal_entities_from_dq_request
wire_traits<ores::refdata::messaging::publish_legal_entities_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_legal_entities_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_equity_underlyings_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_equity_underlyings_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_equity_underlyings_from_dq_request
wire_traits<ores::refdata::messaging::publish_equity_underlyings_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_equity_underlyings_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_equity_underlyings_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_equity_underlyings_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_equity_underlyings_from_dq_request
wire_traits<ores::refdata::messaging::publish_equity_underlyings_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_equity_underlyings_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_conventions_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_conventions_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_conventions_from_dq_request
wire_traits<ores::refdata::messaging::publish_conventions_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_conventions_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_conventions_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_conventions_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_conventions_from_dq_request
wire_traits<ores::refdata::messaging::publish_conventions_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_conventions_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_counterparty_aliases_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_counterparty_aliases_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_counterparty_aliases_from_dq_request
wire_traits<ores::refdata::messaging::publish_counterparty_aliases_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_counterparty_aliases_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_counterparty_aliases_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_counterparty_aliases_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_counterparty_aliases_from_dq_request
wire_traits<ores::refdata::messaging::publish_counterparty_aliases_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_counterparty_aliases_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_countries_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_countries_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_countries_from_dq_request
wire_traits<ores::refdata::messaging::publish_countries_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_countries_from_dq_request>(text);
}

std::string wire_traits<ores::refdata::messaging::publish_countries_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_countries_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_countries_from_dq_request
wire_traits<ores::refdata::messaging::publish_countries_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_countries_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request
wire_traits<ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request
wire_traits<ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_csas_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_csas_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_csas_from_dq_request
wire_traits<ores::refdata::messaging::publish_csas_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_csas_from_dq_request>(text);
}

std::string wire_traits<ores::refdata::messaging::publish_csas_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_csas_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_csas_from_dq_request
wire_traits<ores::refdata::messaging::publish_csas_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_csas_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_currencies_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_currencies_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currencies_from_dq_request
wire_traits<ores::refdata::messaging::publish_currencies_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_currencies_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currencies_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_currencies_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currencies_from_dq_request
wire_traits<ores::refdata::messaging::publish_currencies_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_currencies_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_curve_configuration_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_curve_configuration_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_curve_configuration_from_dq_request
wire_traits<ores::refdata::messaging::publish_curve_configuration_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_curve_configuration_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_curve_configuration_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_curve_configuration_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_curve_configuration_from_dq_request
wire_traits<ores::refdata::messaging::publish_curve_configuration_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_curve_configuration_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_groups_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_currency_groups_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_groups_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_groups_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_currency_groups_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_groups_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_currency_groups_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_groups_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_groups_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_currency_groups_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_currency_groups_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_currency_currency_groups_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_currency_groups_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_currency_groups_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_currency_currency_groups_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_currency_groups_from_dq_request>::
    write_msgpack(
        const ores::refdata::messaging::publish_currency_currency_groups_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_currency_groups_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_currency_groups_from_dq_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_currency_currency_groups_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_pairs_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_currency_pairs_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_pairs_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_pairs_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_currency_pairs_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_pairs_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_currency_pairs_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_pairs_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_pairs_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_currency_pairs_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request>::
    write_json(
        const ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request>::
    write_msgpack(
        const ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_calendars_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_currency_calendars_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_calendars_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_calendars_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_currency_calendars_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_calendars_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_currency_calendars_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_calendars_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_calendars_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_currency_calendars_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_countries_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_currency_countries_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_countries_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_countries_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_currency_countries_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_countries_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_currency_countries_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_countries_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_countries_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_currency_countries_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request>::
    write_json(
        const ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request&
            v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request>::
    write_msgpack(
        const ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request&
            v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request
wire_traits<ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_entity_classifications_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_entity_classifications_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_entity_classifications_from_dq_request
wire_traits<ores::refdata::messaging::publish_entity_classifications_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_entity_classifications_from_dq_request>(text);
}

std::string wire_traits<ores::refdata::messaging::publish_entity_classifications_from_dq_request>::
    write_msgpack(
        const ores::refdata::messaging::publish_entity_classifications_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_entity_classifications_from_dq_request
wire_traits<ores::refdata::messaging::publish_entity_classifications_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_entity_classifications_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_lei_counterparties_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_lei_counterparties_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_lei_counterparties_from_dq_request
wire_traits<ores::refdata::messaging::publish_lei_counterparties_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_lei_counterparties_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_lei_counterparties_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_lei_counterparties_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_lei_counterparties_from_dq_request
wire_traits<ores::refdata::messaging::publish_lei_counterparties_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_lei_counterparties_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_lei_parties_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_lei_parties_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_lei_parties_from_dq_request
wire_traits<ores::refdata::messaging::publish_lei_parties_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_lei_parties_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_lei_parties_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_lei_parties_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_lei_parties_from_dq_request
wire_traits<ores::refdata::messaging::publish_lei_parties_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_lei_parties_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_local_jurisdictions_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_local_jurisdictions_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_local_jurisdictions_from_dq_request
wire_traits<ores::refdata::messaging::publish_local_jurisdictions_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_local_jurisdictions_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_local_jurisdictions_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_local_jurisdictions_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_local_jurisdictions_from_dq_request
wire_traits<ores::refdata::messaging::publish_local_jurisdictions_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_local_jurisdictions_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request
wire_traits<ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request
wire_traits<ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_netting_agreements_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_netting_agreements_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_netting_agreements_from_dq_request
wire_traits<ores::refdata::messaging::publish_netting_agreements_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_netting_agreements_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_netting_agreements_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_netting_agreements_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_netting_agreements_from_dq_request
wire_traits<ores::refdata::messaging::publish_netting_agreements_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_netting_agreements_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_netting_set_aliases_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_netting_set_aliases_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_netting_set_aliases_from_dq_request
wire_traits<ores::refdata::messaging::publish_netting_set_aliases_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_netting_set_aliases_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_netting_set_aliases_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_netting_set_aliases_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_netting_set_aliases_from_dq_request
wire_traits<ores::refdata::messaging::publish_netting_set_aliases_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_netting_set_aliases_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_netting_sets_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_netting_sets_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_netting_sets_from_dq_request
wire_traits<ores::refdata::messaging::publish_netting_sets_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_netting_sets_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_netting_sets_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_netting_sets_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_netting_sets_from_dq_request
wire_traits<ores::refdata::messaging::publish_netting_sets_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_netting_sets_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_contact_informations_from_dq_request>::
    write_json(
        const ores::refdata::messaging::publish_party_contact_informations_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_party_contact_informations_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_contact_informations_from_dq_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_party_contact_informations_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_contact_informations_from_dq_request>::
    write_msgpack(
        const ores::refdata::messaging::publish_party_contact_informations_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_party_contact_informations_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_contact_informations_from_dq_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_party_contact_informations_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_counterparties_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_party_counterparties_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_party_counterparties_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_counterparties_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_party_counterparties_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_counterparties_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_party_counterparties_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_party_counterparties_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_counterparties_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_party_counterparties_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_countries_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_party_countries_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_party_countries_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_countries_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_party_countries_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_countries_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_party_countries_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_party_countries_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_countries_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_party_countries_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_currencies_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_party_currencies_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_party_currencies_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_currencies_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_party_currencies_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_currencies_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_party_currencies_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_party_currencies_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_currencies_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_party_currencies_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_relationships_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_party_relationships_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_party_relationships_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_relationships_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_party_relationships_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_relationships_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_party_relationships_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_party_relationships_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_relationships_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_party_relationships_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_party_roles_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_party_roles_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_party_roles_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_roles_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_party_roles_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_party_roles_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_party_roles_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_party_roles_from_dq_request
wire_traits<ores::refdata::messaging::publish_party_roles_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_party_roles_from_dq_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::publish_payment_frequencies_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_payment_frequencies_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_payment_frequencies_from_dq_request
wire_traits<ores::refdata::messaging::publish_payment_frequencies_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_payment_frequencies_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_payment_frequencies_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_payment_frequencies_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_payment_frequencies_from_dq_request
wire_traits<ores::refdata::messaging::publish_payment_frequencies_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_payment_frequencies_from_dq_request>(data);
}

std::string wire_traits<ores::refdata::messaging::publish_person_roles_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_person_roles_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_person_roles_from_dq_request
wire_traits<ores::refdata::messaging::publish_person_roles_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_person_roles_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_person_roles_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_person_roles_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_person_roles_from_dq_request
wire_traits<ores::refdata::messaging::publish_person_roles_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_person_roles_from_dq_request>(
        data);
}

std::string wire_traits<ores::refdata::messaging::publish_portfolios_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_portfolios_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_portfolios_from_dq_request
wire_traits<ores::refdata::messaging::publish_portfolios_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_portfolios_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_portfolios_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_portfolios_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_portfolios_from_dq_request
wire_traits<ores::refdata::messaging::publish_portfolios_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_portfolios_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request>::
    write_json(
        const ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request
wire_traits<ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request>::
    write_msgpack(
        const ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request
wire_traits<ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_reporting_regimes_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_reporting_regimes_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_reporting_regimes_from_dq_request
wire_traits<ores::refdata::messaging::publish_reporting_regimes_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_reporting_regimes_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_reporting_regimes_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_reporting_regimes_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_reporting_regimes_from_dq_request
wire_traits<ores::refdata::messaging::publish_reporting_regimes_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_reporting_regimes_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_supervisory_bodies_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_supervisory_bodies_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_supervisory_bodies_from_dq_request
wire_traits<ores::refdata::messaging::publish_supervisory_bodies_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_supervisory_bodies_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_supervisory_bodies_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_supervisory_bodies_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_supervisory_bodies_from_dq_request
wire_traits<ores::refdata::messaging::publish_supervisory_bodies_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::publish_supervisory_bodies_from_dq_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::publish_tenor_schedules_from_dq_request>::write_json(
    const ores::refdata::messaging::publish_tenor_schedules_from_dq_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::publish_tenor_schedules_from_dq_request
wire_traits<ores::refdata::messaging::publish_tenor_schedules_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::publish_tenor_schedules_from_dq_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::publish_tenor_schedules_from_dq_request>::write_msgpack(
    const ores::refdata::messaging::publish_tenor_schedules_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::publish_tenor_schedules_from_dq_request
wire_traits<ores::refdata::messaging::publish_tenor_schedules_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::publish_tenor_schedules_from_dq_request>(
        data);
}

}
