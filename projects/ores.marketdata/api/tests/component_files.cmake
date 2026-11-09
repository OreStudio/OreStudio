# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
# Template: cmake_component_files_tests.mustache
# To modify, update the template and regenerate.
set(files
    "adapter_codec_roundtrip_tests.cpp"
    "adapter_operation_protocol_codec_roundtrip_tests.cpp"
    "adapter_protocol_codec_roundtrip_tests.cpp"
    "billing_operation_protocol_codec_roundtrip_tests.cpp"
    "codec_wire_bytes_tests.cpp"
    "composite_codec_roundtrip_tests.cpp"
    "composite_input_codec_roundtrip_tests.cpp"
    "composite_input_protocol_codec_roundtrip_tests.cpp"
    "composite_protocol_codec_roundtrip_tests.cpp"
    "conventions_convention_lookup_tests.cpp"
    "curve_publish_codec_roundtrip_tests.cpp"
    "curve_publish_operations_protocol_codec_roundtrip_tests.cpp"
    "curve_publish_protocol_codec_roundtrip_tests.cpp"
    "datum_market_datum_tests.cpp"
    "datum_market_reference_tests.cpp"
    "datum_value_tests.cpp"
    "delivery_count_codec_roundtrip_tests.cpp"
    "delivery_count_protocol_codec_roundtrip_tests.cpp"
    "demand_interval_codec_roundtrip_tests.cpp"
    "demand_interval_protocol_codec_roundtrip_tests.cpp"
    "domain_adapter_subjects_tests.cpp"
    "domain_market_clock_tests.cpp"
    "domain_market_fixing_tests.cpp"
    "domain_market_observation_tests.cpp"
    "domain_market_series_tests.cpp"
    "domain_tick_subjects_tests.cpp"
    "feed_codec_roundtrip_tests.cpp"
    "feed_protocol_codec_roundtrip_tests.cpp"
    "feed_scope_codec_roundtrip_tests.cpp"
    "feed_scope_protocol_codec_roundtrip_tests.cpp"
    "fixing_read_operations_protocol_codec_roundtrip_tests.cpp"
    "fx_forward_operation_protocol_codec_roundtrip_tests.cpp"
    "main.cpp"
    "market_calibration_value_codec_roundtrip_tests.cpp"
    "market_calibration_value_protocol_codec_roundtrip_tests.cpp"
    "market_data_mapping_codec_roundtrip_tests.cpp"
    "market_data_mapping_protocol_codec_roundtrip_tests.cpp"
    "market_data_vendor_instrument_codec_roundtrip_tests.cpp"
    "market_data_vendor_instrument_protocol_codec_roundtrip_tests.cpp"
    "market_fixing_codec_roundtrip_tests.cpp"
    "market_fixing_protocol_codec_roundtrip_tests.cpp"
    "market_input_value_codec_roundtrip_tests.cpp"
    "market_input_value_protocol_codec_roundtrip_tests.cpp"
    "market_observation_codec_roundtrip_tests.cpp"
    "market_observation_protocol_codec_roundtrip_tests.cpp"
    "market_scenario_value_codec_roundtrip_tests.cpp"
    "market_scenario_value_protocol_codec_roundtrip_tests.cpp"
    "market_series_asset_class_codec_roundtrip_tests.cpp"
    "market_series_asset_class_protocol_codec_roundtrip_tests.cpp"
    "market_series_codec_roundtrip_tests.cpp"
    "market_series_identity_codec_roundtrip_tests.cpp"
    "market_series_protocol_codec_roundtrip_tests.cpp"
    "observation_lineage_codec_roundtrip_tests.cpp"
    "observation_lineage_protocol_codec_roundtrip_tests.cpp"
    "observation_read_operations_protocol_codec_roundtrip_tests.cpp"
    "operations_protocol_codec_roundtrip_tests.cpp"
    "projection_ore_projection_tests.cpp"
    "provider_codec_roundtrip_tests.cpp"
    "provider_protocol_codec_roundtrip_tests.cpp"
    "provider_terms_list_operations_protocol_codec_roundtrip_tests.cpp"
    "provider_terms_operations_protocol_codec_roundtrip_tests.cpp"
    "publish_operation_protocol_codec_roundtrip_tests.cpp"
    "published_curves_operations_protocol_codec_roundtrip_tests.cpp"
    "rate_card_component_codec_roundtrip_tests.cpp"
    "rate_card_component_protocol_codec_roundtrip_tests.cpp"
    "route_codec_roundtrip_tests.cpp"
    "route_protocol_codec_roundtrip_tests.cpp"
    "series_axis_codec_roundtrip_tests.cpp"
    "series_axis_value_codec_roundtrip_tests.cpp"
    "series_classification_rule_codec_roundtrip_tests.cpp"
    "series_classification_rule_protocol_codec_roundtrip_tests.cpp"
    "session_codec_roundtrip_tests.cpp"
    "session_protocol_codec_roundtrip_tests.cpp"
    "subscription_codec_roundtrip_tests.cpp"
    "subscription_interval_codec_roundtrip_tests.cpp"
    "subscription_interval_protocol_codec_roundtrip_tests.cpp"
    "subscription_protocol_codec_roundtrip_tests.cpp"
    "supplier_codec_roundtrip_tests.cpp"
    "supplier_protocol_codec_roundtrip_tests.cpp"
)
