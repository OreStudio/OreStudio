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
#include "ores.dq.api/domain/artefact_type_codec.hpp"
#include "ores.dq.api/domain/badge_definition_codec.hpp"
#include "ores.dq.api/domain/badge_mapping_codec.hpp"
#include "ores.dq.api/domain/badge_severity_codec.hpp"
#include "ores.dq.api/domain/catalog_codec.hpp"
#include "ores.dq.api/domain/change_reason_category_codec.hpp"
#include "ores.dq.api/domain/change_reason_codec.hpp"
#include "ores.dq.api/domain/code_domain_codec.hpp"
#include "ores.dq.api/domain/coding_scheme_authority_type_codec.hpp"
#include "ores.dq.api/domain/coding_scheme_codec.hpp"
#include "ores.dq.api/domain/data_domain_codec.hpp"
#include "ores.dq.api/domain/dataset_bundle_codec.hpp"
#include "ores.dq.api/domain/dataset_bundle_member_codec.hpp"
#include "ores.dq.api/domain/dataset_codec.hpp"
#include "ores.dq.api/domain/dataset_dependency_codec.hpp"
#include "ores.dq.api/domain/fsm_state_codec.hpp"
#include "ores.dq.api/domain/fsm_transition_codec.hpp"
#include "ores.dq.api/domain/lei_bic_codec.hpp"
#include "ores.dq.api/domain/lei_entity_codec.hpp"
#include "ores.dq.api/domain/lei_relationship_codec.hpp"
#include "ores.dq.api/domain/methodology_codec.hpp"
#include "ores.dq.api/domain/nature_dimension_codec.hpp"
#include "ores.dq.api/domain/origin_dimension_codec.hpp"
#include "ores.dq.api/domain/publication_codec.hpp"
#include "ores.dq.api/domain/subject_area_codec.hpp"
#include "ores.dq.api/domain/treatment_dimension_codec.hpp"
#include "ores.dq.api/generators/artefact_type_generator.hpp"
#include "ores.dq.api/generators/badge_definition_generator.hpp"
#include "ores.dq.api/generators/badge_mapping_generator.hpp"
#include "ores.dq.api/generators/badge_severity_generator.hpp"
#include "ores.dq.api/generators/catalog_generator.hpp"
#include "ores.dq.api/generators/change_reason_category_generator.hpp"
#include "ores.dq.api/generators/change_reason_generator.hpp"
#include "ores.dq.api/generators/code_domain_generator.hpp"
#include "ores.dq.api/generators/coding_scheme_authority_type_generator.hpp"
#include "ores.dq.api/generators/coding_scheme_generator.hpp"
#include "ores.dq.api/generators/data_domain_generator.hpp"
#include "ores.dq.api/generators/dataset_bundle_generator.hpp"
#include "ores.dq.api/generators/dataset_bundle_member_generator.hpp"
#include "ores.dq.api/generators/dataset_generator.hpp"
#include "ores.dq.api/generators/fsm_state_generator.hpp"
#include "ores.dq.api/generators/fsm_transition_generator.hpp"
#include "ores.dq.api/generators/lei_bic_generator.hpp"
#include "ores.dq.api/generators/lei_entity_generator.hpp"
#include "ores.dq.api/generators/lei_relationship_generator.hpp"
#include "ores.dq.api/generators/methodology_generator.hpp"
#include "ores.dq.api/generators/nature_dimension_generator.hpp"
#include "ores.dq.api/generators/origin_dimension_generator.hpp"
#include "ores.dq.api/generators/publication_generator.hpp"
#include "ores.dq.api/generators/subject_area_generator.hpp"
#include "ores.dq.api/generators/treatment_dimension_generator.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>
#include <string_view>

namespace {

using ores::utility::generation::generation_context;

/**
 * @brief A string's bytes, which is what the MessagePack readers take.
 */
std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The bytes reflect-cpp writes for @p v, which is what the generated
 * codec has to reproduce.
 */
template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

/**
 * @brief Asserts that one type's codec writes what reflect-cpp writes, in both
 * formats, and reads those bytes back to what reflect-cpp reads.
 *
 * The reader is compared with reflect-cpp's reader rather than with the value
 * the encoding started from, because the wire states an instant to the
 * microsecond and a generator hands out finer ones: what survives a round trip
 * is the wire's own resolution, and both readers agree on it or one of them is
 * wrong about the format.
 *
 * The codec is named as a template argument rather than built into the helper
 * so that a type's case is one line, and the list of lines below is the list of
 * types: a type with no case here is a type whose codec nothing compares.
 */
template <typename Codec, typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    const auto json = Codec::to_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(Codec::from_json(json) == rfl::json::read<T>(json).value());

    const auto msgpack = Codec::to_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(Codec::from_msgpack(bytes_of(msgpack)) ==
          rfl::msgpack::read<T>(std::span<const char>(msgpack.data(), msgpack.size())).value());

    // The seam resolved to this type's specialisation rather than falling
    // through to reflect-cpp, which is what the bytes alone cannot say.
    CHECK(ores::utility::codec::wire_traits<T>::is_generated);
    CHECK(ores::utility::codec::wire_traits<T>::write_json(v) == Codec::to_json(v));
}

/**
 * @brief A dataset dependency, which has no generator: it is the one type in
 * the component the models do not own.
 */
ores::dq::domain::dataset_dependency a_dataset_dependency() {
    auto v = ores::dq::domain::dataset_dependency{};
    v.tenant_id = "a2c0a6c0-1f3b-4a2f-9c7d-6f2e0b1d4c33";
    v.dataset_code = "FpML";
    v.dependency_code = "ISO_4217";
    v.role = "reference_data";
    v.modified_by = "admin";
    v.performed_by = "admin";
    v.recorded_at =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    return v;
}

}

TEST_CASE("every generated codec writes what reflect-cpp writes", "[codec]") {
    generation_context ctx(20261029);

    same_bytes_as_reflect_cpp<ores::dq::domain::artefact_type_codec>(
        ores::dq::generators::generate_synthetic_artefact_type(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::badge_definition_codec>(
        ores::dq::generators::generate_synthetic_badge_definition(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::badge_mapping_codec>(
        ores::dq::generators::generate_synthetic_badge_mapping(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::badge_severity_codec>(
        ores::dq::generators::generate_synthetic_badge_severity(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::catalog_codec>(
        ores::dq::generators::generate_synthetic_catalog(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::change_reason_category_codec>(
        ores::dq::generators::generate_synthetic_change_reason_category(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::change_reason_codec>(
        ores::dq::generators::generate_synthetic_change_reason(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::code_domain_codec>(
        ores::dq::generators::generate_synthetic_code_domain(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::coding_scheme_authority_type_codec>(
        ores::dq::generators::generate_synthetic_coding_scheme_authority_type(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::coding_scheme_codec>(
        ores::dq::generators::generate_synthetic_coding_scheme(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::data_domain_codec>(
        ores::dq::generators::generate_synthetic_data_domain(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::dataset_bundle_codec>(
        ores::dq::generators::generate_synthetic_dataset_bundle(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::dataset_bundle_member_codec>(
        ores::dq::generators::generate_synthetic_dataset_bundle_member(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::dataset_codec>(
        ores::dq::generators::generate_synthetic_dataset(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::dataset_dependency_codec>(a_dataset_dependency());
    same_bytes_as_reflect_cpp<ores::dq::domain::fsm_state_codec>(
        ores::dq::generators::generate_synthetic_fsm_state(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::fsm_transition_codec>(
        ores::dq::generators::generate_synthetic_fsm_transition(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::lei_bic_codec>(
        ores::dq::generators::generate_synthetic_lei_bic(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::lei_entity_codec>(
        ores::dq::generators::generate_synthetic_lei_entity(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::lei_relationship_codec>(
        ores::dq::generators::generate_synthetic_lei_relationship(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::methodology_codec>(
        ores::dq::generators::generate_synthetic_methodology(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::nature_dimension_codec>(
        ores::dq::generators::generate_synthetic_nature_dimension(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::origin_dimension_codec>(
        ores::dq::generators::generate_synthetic_origin_dimension(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::publication_codec>(
        ores::dq::generators::generate_synthetic_publication(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::subject_area_codec>(
        ores::dq::generators::generate_synthetic_subject_area(ctx));
    same_bytes_as_reflect_cpp<ores::dq::domain::treatment_dimension_codec>(
        ores::dq::generators::generate_synthetic_treatment_dimension(ctx));
}

TEST_CASE("the wire codec sends a dq type through its generated codec", "[codec][wire_codec]") {
    generation_context ctx(20261029);
    const auto v = ores::dq::generators::generate_synthetic_catalog(ctx);

    for (const auto format : {ores::nats::wire_format::json, ores::nats::wire_format::msgpack}) {
        const ores::nats::wire_codec codec(format);
        const auto bytes = codec.encode(v);
        const std::string_view sent(reinterpret_cast<const char*>(bytes.data()), bytes.size());

        const auto generated = format == ores::nats::wire_format::json ?
                                   ores::dq::domain::catalog_codec::to_json(v) :
                                   ores::dq::domain::catalog_codec::to_msgpack(v);
        CHECK(sent == generated);

        // Decoding and encoding again reproduces the bytes: what survives a
        // round trip is the wire's own resolution, which is the microsecond
        // and not the generator's finer one.
        const auto read = codec.decode<ores::dq::domain::catalog>(bytes);
        REQUIRE(read);
        const auto again = codec.encode(*read);
        CHECK(std::string_view(reinterpret_cast<const char*>(again.data()), again.size()) == sent);
    }
}
