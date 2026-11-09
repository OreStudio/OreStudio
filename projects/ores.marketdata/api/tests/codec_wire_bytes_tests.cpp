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
#include "ores.marketdata.api/domain/adapter_codec.hpp"
#include "ores.marketdata.api/domain/composite_codec.hpp"
#include "ores.marketdata.api/domain/composite_input_codec.hpp"
#include "ores.marketdata.api/domain/curve_publish_codec.hpp"
#include "ores.marketdata.api/domain/delivery_count_codec.hpp"
#include "ores.marketdata.api/domain/demand_interval_codec.hpp"
#include "ores.marketdata.api/domain/feed_codec.hpp"
#include "ores.marketdata.api/domain/feed_scope_codec.hpp"
#include "ores.marketdata.api/domain/market_calibration_value_codec.hpp"
#include "ores.marketdata.api/domain/market_data_mapping_codec.hpp"
#include "ores.marketdata.api/domain/market_data_vendor_instrument_codec.hpp"
#include "ores.marketdata.api/domain/market_fixing_codec.hpp"
#include "ores.marketdata.api/domain/market_input_value_codec.hpp"
#include "ores.marketdata.api/domain/market_observation_codec.hpp"
#include "ores.marketdata.api/domain/market_scenario_value_codec.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class_codec.hpp"
#include "ores.marketdata.api/domain/market_series_codec.hpp"
#include "ores.marketdata.api/domain/market_series_identity_codec.hpp"
#include "ores.marketdata.api/domain/observation_lineage_codec.hpp"
#include "ores.marketdata.api/domain/provider_codec.hpp"
#include "ores.marketdata.api/domain/rate_card_component_codec.hpp"
#include "ores.marketdata.api/domain/route_codec.hpp"
#include "ores.marketdata.api/domain/series_axis_codec.hpp"
#include "ores.marketdata.api/domain/series_axis_value_codec.hpp"
#include "ores.marketdata.api/domain/series_classification_rule_codec.hpp"
#include "ores.marketdata.api/domain/session_codec.hpp"
#include "ores.marketdata.api/domain/subscription_codec.hpp"
#include "ores.marketdata.api/domain/subscription_interval_codec.hpp"
#include "ores.marketdata.api/domain/supplier_codec.hpp"
#include "ores.marketdata.api/generators/adapter_generator.hpp"
#include "ores.marketdata.api/generators/composite_generator.hpp"
#include "ores.marketdata.api/generators/composite_input_generator.hpp"
#include "ores.marketdata.api/generators/curve_publish_generator.hpp"
#include "ores.marketdata.api/generators/delivery_count_generator.hpp"
#include "ores.marketdata.api/generators/demand_interval_generator.hpp"
#include "ores.marketdata.api/generators/feed_generator.hpp"
#include "ores.marketdata.api/generators/feed_scope_generator.hpp"
#include "ores.marketdata.api/generators/market_calibration_value_generator.hpp"
#include "ores.marketdata.api/generators/market_data_mapping_generator.hpp"
#include "ores.marketdata.api/generators/market_data_vendor_instrument_generator.hpp"
#include "ores.marketdata.api/generators/market_fixing_generator.hpp"
#include "ores.marketdata.api/generators/market_input_value_generator.hpp"
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_scenario_value_generator.hpp"
#include "ores.marketdata.api/generators/market_series_asset_class_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/generators/market_series_identity_generator.hpp"
#include "ores.marketdata.api/generators/observation_lineage_generator.hpp"
#include "ores.marketdata.api/generators/provider_generator.hpp"
#include "ores.marketdata.api/generators/rate_card_component_generator.hpp"
#include "ores.marketdata.api/generators/route_generator.hpp"
#include "ores.marketdata.api/generators/series_axis_generator.hpp"
#include "ores.marketdata.api/generators/series_axis_value_generator.hpp"
#include "ores.marketdata.api/generators/series_classification_rule_generator.hpp"
#include "ores.marketdata.api/generators/session_generator.hpp"
#include "ores.marketdata.api/generators/subscription_generator.hpp"
#include "ores.marketdata.api/generators/subscription_interval_generator.hpp"
#include "ores.marketdata.api/generators/supplier_generator.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/billing_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/curve_publish_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_terms_list_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_terms_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/publish_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <catch2/catch_test_macros.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The codec reads back what it wrote, in both formats.
 */
template <typename T>
void survives_the_wire(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (survives_the_wire(T{}), ...);
}

}

TEST_CASE("every marketdata domain type has its wire form", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    survives_the_wire(ores::marketdata::generators::generate_synthetic_adapter(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_composite(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_composite_input(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_curve_publish(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_delivery_count(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_demand_interval(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_feed(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_feed_scope(ctx));
    survives_the_wire(
        ores::marketdata::generators::generate_synthetic_market_calibration_value(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_data_mapping(ctx));
    survives_the_wire(
        ores::marketdata::generators::generate_synthetic_market_data_vendor_instrument(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_fixing(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_input_value(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_observation(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_scenario_value(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_series(ctx));
    survives_the_wire(
        ores::marketdata::generators::generate_synthetic_market_series_asset_class(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_market_series_identity(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_observation_lineage(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_provider(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_rate_card_component(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_route(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_series_axis(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_series_axis_value(ctx));
    survives_the_wire(
        ores::marketdata::generators::generate_synthetic_series_classification_rule(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_session(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_subscription(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_subscription_interval(ctx));
    survives_the_wire(ores::marketdata::generators::generate_synthetic_supplier(ctx));
}

TEST_CASE("every marketdata operation message's default has its wire form", codec_tags) {
    every_default_has_the_same_bytes<
        ores::marketdata::messaging::vendor_id,
        ores::marketdata::messaging::rejected_id,
        ores::marketdata::messaging::adapter_announce_request,
        ores::marketdata::messaging::adapter_announce_response,
        ores::marketdata::messaging::adapter_demand_request,
        ores::marketdata::messaging::adapter_demand_response,
        ores::marketdata::messaging::adapter_tick,
        ores::marketdata::messaging::adapter_heartbeat,
        ores::marketdata::messaging::adapter_status,
        ores::marketdata::messaging::list_adapter_status_request,
        ores::marketdata::messaging::list_adapter_status_response,
        ores::marketdata::messaging::bill_line_view,
        ores::marketdata::messaging::source_bill_view,
        ores::marketdata::messaging::party_charge_view,
        ores::marketdata::messaging::compute_bill_request,
        ores::marketdata::messaging::compute_bill_response,
        ores::marketdata::messaging::publish_curve_request,
        ores::marketdata::messaging::publish_curve_response,
        ores::marketdata::messaging::get_fixing_request,
        ores::marketdata::messaging::get_fixing_response,
        ores::marketdata::messaging::get_fixing_window_request,
        ores::marketdata::messaging::get_fixing_window_response,
        ores::marketdata::messaging::series_identity_field,
        ores::marketdata::messaging::resolve_series_identity_request,
        ores::marketdata::messaging::resolve_series_identity_response,
        ores::marketdata::messaging::oresmd_field_spec,
        ores::marketdata::messaging::oresmd_instrument,
        ores::marketdata::messaging::oresmd_index_field,
        ores::marketdata::messaging::oresmd_index_family,
        ores::marketdata::messaging::describe_oresmd_schema_request,
        ores::marketdata::messaging::describe_oresmd_schema_response,
        ores::marketdata::messaging::oresmd_uri_field,
        ores::marketdata::messaging::parse_oresmd_uri_request,
        ores::marketdata::messaging::parse_oresmd_uri_response,
        ores::marketdata::messaging::compose_oresmd_uri_request,
        ores::marketdata::messaging::compose_oresmd_uri_response,
        ores::marketdata::messaging::list_series_identities_request,
        ores::marketdata::messaging::listed_series,
        ores::marketdata::messaging::list_series_identities_response,
        ores::marketdata::messaging::crm_rate_item,
        ores::marketdata::messaging::get_crm_rate_request,
        ores::marketdata::messaging::get_crm_rate_response,
        ores::marketdata::messaging::get_crm_rates_request,
        ores::marketdata::messaging::get_crm_rates_response,
        ores::marketdata::messaging::republish_curve_request,
        ores::marketdata::messaging::republish_curve_response,
        ores::marketdata::messaging::compute_curve_request,
        ores::marketdata::messaging::computed_curve_point,
        ores::marketdata::messaging::compute_curve_response,
        ores::marketdata::messaging::series_slice_instant,
        ores::marketdata::messaging::get_series_slice_request,
        ores::marketdata::messaging::get_series_slice_response,
        ores::marketdata::messaging::evolution_node,
        ores::marketdata::messaging::evolution_instant,
        ores::marketdata::messaging::get_series_evolution_request,
        ores::marketdata::messaging::get_series_evolution_response,
        ores::marketdata::messaging::point_provenance,
        ores::marketdata::messaging::get_curve_snapshot_request,
        ores::marketdata::messaging::get_curve_snapshot_response,
        ores::marketdata::messaging::import_market_data_request,
        ores::marketdata::messaging::import_market_data_response,
        ores::marketdata::messaging::feed_kind_counts,
        ores::marketdata::messaging::start_feeds_under_folder_request,
        ores::marketdata::messaging::start_feeds_under_folder_response,
        ores::marketdata::messaging::stop_feeds_under_folder_request,
        ores::marketdata::messaging::stop_feeds_under_folder_response,
        ores::marketdata::messaging::export_market_data_to_storage_request,
        ores::marketdata::messaging::export_market_data_to_storage_response,
        ores::marketdata::messaging::backfill_series_identity_request,
        ores::marketdata::messaging::backfill_series_identity_response,
        ores::marketdata::messaging::key_manual_point_request,
        ores::marketdata::messaging::key_manual_point_response,
        ores::marketdata::messaging::clear_manual_point_request,
        ores::marketdata::messaging::clear_manual_point_response,
        ores::marketdata::messaging::export_market_data_request,
        ores::marketdata::messaging::export_market_data_response,
        ores::marketdata::messaging::market_tick,
        ores::marketdata::messaging::get_series_ticks_request,
        ores::marketdata::messaging::get_series_ticks_response,
        ores::marketdata::messaging::store_market_state_request,
        ores::marketdata::messaging::store_market_state_response,
        ores::marketdata::messaging::provider_terms_choice,
        ores::marketdata::messaging::provider_terms_outcome,
        ores::marketdata::messaging::accept_many_provider_terms_request,
        ores::marketdata::messaging::accept_many_provider_terms_response,
        ores::marketdata::messaging::accept_provider_terms_request,
        ores::marketdata::messaging::accept_provider_terms_response,
        ores::marketdata::messaging::withdraw_provider_terms_request,
        ores::marketdata::messaging::withdraw_provider_terms_response,
        ores::marketdata::messaging::published_curve_point,
        ores::marketdata::messaging::published_curve_set,
        ores::marketdata::messaging::list_published_curves_request,
        ores::marketdata::messaging::list_published_curves_response,
        ores::marketdata::messaging::get_published_curve_sets_request,
        ores::marketdata::messaging::get_published_curve_sets_response,
        ores::marketdata::messaging::withdraw_curve_request,
        ores::marketdata::messaging::withdraw_curve_response,
        ores::marketdata::messaging::publish_market_data_observations_from_dq_request,
        ores::marketdata::messaging::publish_adapters_from_dq_request,
        ores::marketdata::messaging::publish_suppliers_from_dq_request>();
}
