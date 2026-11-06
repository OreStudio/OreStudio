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
#include "ores.utility/codec/json_codec.hpp"
#include "ores.analytics.api/domain/credit_simulation_config_codec.hpp"
#include "ores.analytics.api/domain/credit_simulation_entity_config_codec.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_config_codec.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_row_config_codec.hpp"
#include "ores.analytics.api/domain/credit_simulation_netting_set_config_codec.hpp"
#include "ores.analytics.api/domain/pricing_engine_type_codec.hpp"
#include "ores.analytics.api/domain/pricing_method_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_config_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_engine_pair_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_product_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter_codec.hpp"
#include "ores.analytics.api/domain/pricing_pair_setting_codec.hpp"
#include "ores.analytics.api/domain/sensitivity_asset_class_codec.hpp"
#include "ores.analytics.api/domain/sensitivity_method_codec.hpp"
#include "ores.analytics.api/domain/sensitivity_template_codec.hpp"
#include "ores.analytics.api/domain/shift_type_codec.hpp"
#include "ores.analytics.api/domain/stress_shift_family_codec.hpp"
#include "ores.analytics.api/domain/stress_test_library_codec.hpp"
#include "ores.analytics.api/domain/stress_test_scenario_codec.hpp"
#include "ores.analytics.api/domain/stress_test_shift_codec.hpp"
#include "ores.analytics.api/domain/todays_market_collection_codec.hpp"
#include "ores.analytics.api/domain/todays_market_collection_kind_codec.hpp"
#include "ores.analytics.api/domain/todays_market_config_codec.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding_codec.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_codec.hpp"
#include "ores.analytics.api/domain/todays_market_entry_codec.hpp"
#include "ores.analytics.api/generators/credit_simulation_config_generator.hpp"
#include "ores.analytics.api/generators/credit_simulation_entity_config_generator.hpp"
#include "ores.analytics.api/generators/credit_simulation_matrix_config_generator.hpp"
#include "ores.analytics.api/generators/credit_simulation_matrix_row_config_generator.hpp"
#include "ores.analytics.api/generators/credit_simulation_netting_set_config_generator.hpp"
#include "ores.analytics.api/generators/pricing_engine_type_generator.hpp"
#include "ores.analytics.api/generators/pricing_method_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_config_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_engine_pair_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_parameter_generator.hpp"
#include "ores.analytics.api/generators/pricing_pair_setting_generator.hpp"
#include "ores.analytics.api/generators/sensitivity_asset_class_generator.hpp"
#include "ores.analytics.api/generators/sensitivity_method_generator.hpp"
#include "ores.analytics.api/generators/sensitivity_template_generator.hpp"
#include "ores.analytics.api/generators/shift_type_generator.hpp"
#include "ores.analytics.api/generators/stress_shift_family_generator.hpp"
#include "ores.analytics.api/generators/stress_test_library_generator.hpp"
#include "ores.analytics.api/generators/stress_test_scenario_generator.hpp"
#include "ores.analytics.api/generators/stress_test_shift_generator.hpp"
#include "ores.analytics.api/generators/todays_market_collection_generator.hpp"
#include "ores.analytics.api/generators/todays_market_collection_kind_generator.hpp"
#include "ores.analytics.api/generators/todays_market_config_generator.hpp"
#include "ores.analytics.api/generators/todays_market_configuration_binding_generator.hpp"
#include "ores.analytics.api/generators/todays_market_configuration_generator.hpp"
#include "ores.analytics.api/generators/todays_market_entry_generator.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.analytics.api/messaging/publish_operation_protocol.hpp"
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
    CHECK(json == ores::utility::codec::json::write(v));
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

TEST_CASE("every analytics domain type has reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_credit_simulation_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_credit_simulation_entity_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_credit_simulation_matrix_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_credit_simulation_matrix_row_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_credit_simulation_netting_set_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_pricing_engine_type(ctx));
    same_bytes_as_reflect_cpp(ores::analytics::generators::generate_synthetic_pricing_method(ctx));
    same_bytes_as_reflect_cpp(ores::analytics::generators::generate_synthetic_pricing_model(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_pricing_model_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_pricing_model_engine_pair(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_pricing_model_product(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_pricing_model_product_parameter(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_pricing_pair_setting(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_sensitivity_asset_class(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_sensitivity_method(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_sensitivity_template(ctx));
    same_bytes_as_reflect_cpp(ores::analytics::generators::generate_synthetic_shift_type(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_stress_shift_family(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_stress_test_library(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_stress_test_scenario(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_stress_test_shift(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_todays_market_collection(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_todays_market_collection_kind(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_todays_market_config(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_todays_market_configuration(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_todays_market_configuration_binding(ctx));
    same_bytes_as_reflect_cpp(
        ores::analytics::generators::generate_synthetic_todays_market_entry(ctx));
}

TEST_CASE("every analytics operation message's default has reflect-cpp's bytes", codec_tags) {
    every_default_has_the_same_bytes<
        ores::analytics::messaging::pricing_engines_document,
        ores::analytics::messaging::todays_market_document,
        ores::analytics::messaging::pricing_engines_document_problem,
        ores::analytics::messaging::save_pricing_engines_document_request,
        ores::analytics::messaging::save_pricing_engines_document_response,
        ores::analytics::messaging::get_pricing_engines_document_request,
        ores::analytics::messaging::get_pricing_engines_document_response,
        ores::analytics::messaging::delete_pricing_engines_document_request,
        ores::analytics::messaging::delete_pricing_engines_document_response,
        ores::analytics::messaging::save_todays_market_document_request,
        ores::analytics::messaging::save_todays_market_document_response,
        ores::analytics::messaging::get_todays_market_document_request,
        ores::analytics::messaging::get_todays_market_document_response,
        ores::analytics::messaging::delete_todays_market_document_request,
        ores::analytics::messaging::delete_todays_market_document_response,
        ores::analytics::messaging::publish_pricing_engines_from_dq_request,
        ores::analytics::messaging::publish_todays_market_from_dq_request>();
}

TEST_CASE("filled configuration documents have reflect-cpp's bytes", codec_tags) {
    namespace g = ores::analytics::generators;
    ores::utility::generation::generation_context ctx(20261105);

    ores::analytics::messaging::pricing_engines_document engines;
    engines.config = g::generate_synthetic_pricing_model_config(ctx);
    engines.products = {g::generate_synthetic_pricing_model_product(ctx),
                        g::generate_synthetic_pricing_model_product(ctx)};
    engines.parameters = {g::generate_synthetic_pricing_model_product_parameter(ctx)};
    same_bytes_as_reflect_cpp(engines);

    ores::analytics::messaging::todays_market_document market;
    market.config = g::generate_synthetic_todays_market_config(ctx);
    market.collections = {g::generate_synthetic_todays_market_collection(ctx)};
    market.entries = {g::generate_synthetic_todays_market_entry(ctx),
                      g::generate_synthetic_todays_market_entry(ctx)};
    market.configurations = {g::generate_synthetic_todays_market_configuration(ctx)};
    market.bindings = {g::generate_synthetic_todays_market_configuration_binding(ctx)};
    same_bytes_as_reflect_cpp(market);
}
