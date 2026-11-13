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
 * Template: cpp_protocol_codec_roundtrip_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.api/domain/market_series_codec.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::marketdata::messaging::series_identity_field", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::series_identity_field>();
}

TEST_CASE("ores::marketdata::messaging::resolve_series_identity_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::resolve_series_identity_request>();
}

TEST_CASE("ores::marketdata::messaging::resolve_series_identity_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::resolve_series_identity_response>();
}

TEST_CASE("ores::marketdata::messaging::oresmd_field_spec", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::oresmd_field_spec>();
}

TEST_CASE("ores::marketdata::messaging::oresmd_instrument", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::oresmd_instrument>();
}

TEST_CASE("ores::marketdata::messaging::oresmd_index_field", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::oresmd_index_field>();
}

TEST_CASE("ores::marketdata::messaging::oresmd_index_family", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::oresmd_index_family>();
}

TEST_CASE("ores::marketdata::messaging::describe_oresmd_schema_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::describe_oresmd_schema_request>();
}

TEST_CASE("ores::marketdata::messaging::describe_oresmd_schema_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::describe_oresmd_schema_response>();
}

TEST_CASE("ores::marketdata::messaging::oresmd_uri_field", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::oresmd_uri_field>();
}

TEST_CASE("ores::marketdata::messaging::parse_oresmd_uri_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::parse_oresmd_uri_request>();
}

TEST_CASE("ores::marketdata::messaging::parse_oresmd_uri_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::parse_oresmd_uri_response>();
}

TEST_CASE("ores::marketdata::messaging::compose_oresmd_uri_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::compose_oresmd_uri_request>();
}

TEST_CASE("ores::marketdata::messaging::compose_oresmd_uri_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::compose_oresmd_uri_response>();
}

TEST_CASE("ores::marketdata::messaging::list_series_identities_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_series_identities_request>();
}

TEST_CASE("ores::marketdata::messaging::listed_series", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::listed_series>();
}

TEST_CASE("ores::marketdata::messaging::list_series_identities_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_series_identities_response>();
}

TEST_CASE("ores::marketdata::messaging::instrument_axis", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::instrument_axis>();
}

TEST_CASE("ores::marketdata::messaging::instrument_source", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::instrument_source>();
}

TEST_CASE("ores::marketdata::messaging::market_instrument", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::market_instrument>();
}

TEST_CASE("ores::marketdata::messaging::list_instruments_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::list_instruments_request>();
}

TEST_CASE("ores::marketdata::messaging::list_instruments_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::list_instruments_response>();
}

TEST_CASE("ores::marketdata::messaging::rung_quote", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::rung_quote>();
}

TEST_CASE("ores::marketdata::messaging::ladder_consensus", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::ladder_consensus>();
}

TEST_CASE("ores::marketdata::messaging::rate_ladder_rung", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::rate_ladder_rung>();
}

TEST_CASE("ores::marketdata::messaging::get_rate_ladder_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_rate_ladder_request>();
}

TEST_CASE("ores::marketdata::messaging::get_rate_ladder_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_rate_ladder_response>();
}

TEST_CASE("ores::marketdata::messaging::crm_rate_item", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::crm_rate_item>();
}

TEST_CASE("ores::marketdata::messaging::get_crm_rate_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_crm_rate_request>();
}

TEST_CASE("ores::marketdata::messaging::get_crm_rate_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_crm_rate_response>();
}

TEST_CASE("ores::marketdata::messaging::get_crm_rates_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_crm_rates_request>();
}

TEST_CASE("ores::marketdata::messaging::get_crm_rates_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_crm_rates_response>();
}

TEST_CASE("ores::marketdata::messaging::republish_curve_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::republish_curve_request>();
}

TEST_CASE("ores::marketdata::messaging::republish_curve_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::republish_curve_response>();
}

TEST_CASE("ores::marketdata::messaging::compute_curve_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::compute_curve_request>();
}

TEST_CASE("ores::marketdata::messaging::computed_curve_point", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::computed_curve_point>();
}

TEST_CASE("ores::marketdata::messaging::compute_curve_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::compute_curve_response>();
}

TEST_CASE("ores::marketdata::messaging::publish_due_curves_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::publish_due_curves_request>();
}

TEST_CASE("ores::marketdata::messaging::curve_republish_outcome", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::curve_republish_outcome>();
}

TEST_CASE("ores::marketdata::messaging::publish_due_curves_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::publish_due_curves_response>();
}

TEST_CASE("ores::marketdata::messaging::series_slice_instant", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::series_slice_instant>();
}

TEST_CASE("ores::marketdata::messaging::get_series_slice_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_series_slice_request>();
}

TEST_CASE("ores::marketdata::messaging::get_series_slice_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_series_slice_response>();
}

TEST_CASE("ores::marketdata::messaging::evolution_node", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::evolution_node>();
}

TEST_CASE("ores::marketdata::messaging::evolution_instant", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::evolution_instant>();
}

TEST_CASE("ores::marketdata::messaging::get_series_evolution_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_series_evolution_request>();
}

TEST_CASE("ores::marketdata::messaging::get_series_evolution_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_series_evolution_response>();
}

TEST_CASE("ores::marketdata::messaging::point_provenance", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::point_provenance>();
}

TEST_CASE("ores::marketdata::messaging::get_curve_snapshot_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_curve_snapshot_request>();
}

TEST_CASE("ores::marketdata::messaging::get_curve_snapshot_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_curve_snapshot_response>();
}

TEST_CASE("ores::marketdata::messaging::import_market_data_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::import_market_data_request>();
}

TEST_CASE("ores::marketdata::messaging::import_market_data_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::import_market_data_response>();
}

TEST_CASE("ores::marketdata::messaging::feed_kind_counts", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::feed_kind_counts>();
}

TEST_CASE("ores::marketdata::messaging::start_feeds_under_folder_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::start_feeds_under_folder_request>();
}

TEST_CASE("ores::marketdata::messaging::start_feeds_under_folder_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::start_feeds_under_folder_response>();
}

TEST_CASE("ores::marketdata::messaging::stop_feeds_under_folder_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::stop_feeds_under_folder_request>();
}

TEST_CASE("ores::marketdata::messaging::stop_feeds_under_folder_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::stop_feeds_under_folder_response>();
}

TEST_CASE("ores::marketdata::messaging::export_market_data_to_storage_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::export_market_data_to_storage_request>();
}

TEST_CASE("ores::marketdata::messaging::export_market_data_to_storage_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::export_market_data_to_storage_response>();
}

TEST_CASE("ores::marketdata::messaging::backfill_series_identity_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::backfill_series_identity_request>();
}

TEST_CASE("ores::marketdata::messaging::backfill_series_identity_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::backfill_series_identity_response>();
}

TEST_CASE("ores::marketdata::messaging::key_manual_point_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::key_manual_point_request>();
}

TEST_CASE("ores::marketdata::messaging::key_manual_point_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::key_manual_point_response>();
}

TEST_CASE("ores::marketdata::messaging::clear_manual_point_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::clear_manual_point_request>();
}

TEST_CASE("ores::marketdata::messaging::clear_manual_point_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::clear_manual_point_response>();
}

TEST_CASE("ores::marketdata::messaging::export_market_data_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::export_market_data_request>();
}

TEST_CASE("ores::marketdata::messaging::export_market_data_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::export_market_data_response>();
}

TEST_CASE("ores::marketdata::messaging::market_tick", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::market_tick>();
}

TEST_CASE("ores::marketdata::messaging::get_series_ticks_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_series_ticks_request>();
}

TEST_CASE("ores::marketdata::messaging::get_series_ticks_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_series_ticks_response>();
}

TEST_CASE("ores::marketdata::messaging::store_market_state_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::store_market_state_request>();
}

TEST_CASE("ores::marketdata::messaging::store_market_state_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::store_market_state_response>();
}
