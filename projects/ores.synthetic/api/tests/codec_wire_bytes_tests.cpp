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
#include "ores.synthetic.api/domain/calibration_fallback_codec.hpp"
#include "ores.synthetic.api/domain/calibration_policy_codec.hpp"
#include "ores.synthetic.api/domain/calibration_provider_order_codec.hpp"
#include "ores.synthetic.api/domain/folder_codec.hpp"
#include "ores.synthetic.api/domain/fx_spot_fixing_item_codec.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config_codec.hpp"
#include "ores.synthetic.api/domain/gmm_component_codec.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_codec.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value_codec.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry_codec.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config_codec.hpp"
#include "ores.synthetic.api/domain/market_data_provider_codec.hpp"
#include "ores.synthetic.api/domain/trade_generation_book_codec.hpp"
#include "ores.synthetic.api/domain/trade_generation_config_codec.hpp"
#include "ores.synthetic.api/domain/trade_generation_counterparty_codec.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition_codec.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_type_codec.hpp"
#include "ores.synthetic.api/generators/calibration_fallback_generator.hpp"
#include "ores.synthetic.api/generators/calibration_policy_generator.hpp"
#include "ores.synthetic.api/generators/calibration_provider_order_generator.hpp"
#include "ores.synthetic.api/generators/folder_generator.hpp"
#include "ores.synthetic.api/generators/fx_spot_fixing_item_generator.hpp"
#include "ores.synthetic.api/generators/fx_spot_generation_config_generator.hpp"
#include "ores.synthetic.api/generators/gmm_component_generator.hpp"
#include "ores.synthetic.api/generators/ir_curve_generation_config_generator.hpp"
#include "ores.synthetic.api/generators/ir_curve_generation_config_process_parameter_value_generator.hpp"
#include "ores.synthetic.api/generators/ir_curve_template_entry_generator.hpp"
#include "ores.synthetic.api/generators/market_data_generation_config_generator.hpp"
#include "ores.synthetic.api/generators/market_data_provider_generator.hpp"
#include "ores.synthetic.api/generators/trade_generation_book_generator.hpp"
#include "ores.synthetic.api/generators/trade_generation_config_generator.hpp"
#include "ores.synthetic.api/generators/trade_generation_counterparty_generator.hpp"
#include "ores.synthetic.api/generators/yield_curve_process_parameter_definition_generator.hpp"
#include "ores.synthetic.api/generators/yield_curve_process_type_generator.hpp"
#include "ores.synthetic.api/messaging/convert_fx_mixture_protocol.hpp"
#include "ores.synthetic.api/messaging/convert_fx_simple_protocol.hpp"
#include "ores.synthetic.api/messaging/convert_ir_parameters_protocol.hpp"
#include "ores.synthetic.api/messaging/fault_operation_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_selection_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The generated codec writes the bytes reflect-cpp wrote, in both
 * formats, and reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (same_bytes_as_reflect_cpp(T{}), ...);
}

}

TEST_CASE("every synthetic domain type has reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_calibration_fallback(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_calibration_policy(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_calibration_provider_order(ctx));
    same_bytes_as_reflect_cpp(ores::synthetic::generators::generate_synthetic_folder(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_fx_spot_fixing_item(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_fx_spot_generation_config(ctx));
    same_bytes_as_reflect_cpp(ores::synthetic::generators::generate_synthetic_gmm_component(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_ir_curve_generation_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::
            generate_synthetic_ir_curve_generation_config_process_parameter_value(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_ir_curve_template_entry(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_market_data_generation_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_market_data_provider(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_trade_generation_book(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_trade_generation_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_trade_generation_counterparty(ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_yield_curve_process_parameter_definition(
            ctx));
    same_bytes_as_reflect_cpp(
        ores::synthetic::generators::generate_synthetic_yield_curve_process_type(ctx));
}

TEST_CASE("every synthetic operation message's default has reflect-cpp's bytes", codec_tags) {
    every_default_has_the_same_bytes<ores::synthetic::messaging::convert_fx_mixture_request,
                                     ores::synthetic::messaging::convert_fx_mixture_response,
                                     ores::synthetic::messaging::convert_fx_simple_request,
                                     ores::synthetic::messaging::convert_fx_simple_response,
                                     ores::synthetic::messaging::convert_ir_parameters_request,
                                     ores::synthetic::messaging::convert_ir_parameters_response,
                                     ores::synthetic::messaging::set_faults_request,
                                     ores::synthetic::messaging::set_faults_response,
                                     ores::synthetic::messaging::start_feed_request,
                                     ores::synthetic::messaging::start_feed_response,
                                     ores::synthetic::messaging::stop_feed_request,
                                     ores::synthetic::messaging::stop_feed_response,
                                     ores::synthetic::messaging::provider_dispersion,
                                     ores::synthetic::messaging::feed_start,
                                     ores::synthetic::messaging::running_feed,
                                     ores::synthetic::messaging::list_feeds_request,
                                     ores::synthetic::messaging::list_feeds_response,
                                     ores::synthetic::messaging::calibration_policy_summary,
                                     ores::synthetic::messaging::candidate_outcome,
                                     ores::synthetic::messaging::calibrated_feed,
                                     ores::synthetic::messaging::calibrate_request,
                                     ores::synthetic::messaging::calibrate_response,
                                     ores::synthetic::messaging::feed_folder,
                                     ores::synthetic::messaging::list_feed_folders_request,
                                     ores::synthetic::messaging::list_feed_folders_response,
                                     ores::synthetic::messaging::folder_feed,
                                     ores::synthetic::messaging::list_folder_feeds_request,
                                     ores::synthetic::messaging::list_folder_feeds_response,
                                     ores::synthetic::messaging::start_selected_feeds_request,
                                     ores::synthetic::messaging::start_selected_feeds_response,
                                     ores::synthetic::messaging::stop_selected_feeds_request,
                                     ores::synthetic::messaging::stop_selected_feeds_response,
                                     ores::synthetic::messaging::parameter_spec,
                                     ores::synthetic::messaging::simulate_ir_curve_paths_request,
                                     ores::synthetic::messaging::simulate_ir_curve_paths_response,
                                     ores::synthetic::messaging::preview_ir_curve_template_row,
                                     ores::synthetic::messaging::preview_ir_curve_shape_request,
                                     ores::synthetic::messaging::preview_ir_curve_shape_point,
                                     ores::synthetic::messaging::preview_ir_curve_shape_response,
                                     ores::synthetic::messaging::simulate_fx_spot_paths_request,
                                     ores::synthetic::messaging::simulate_fx_spot_paths_response>();
}
