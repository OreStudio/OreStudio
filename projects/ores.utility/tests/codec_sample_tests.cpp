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
#include "ores.utility/codec/sample.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <catch2/catch_test_macros.hpp>
#include <span>

namespace ores::utility::codec_sample_probe {

enum class sample_probe_colour { red, green, blue };

struct sample_probe_leaf {
    std::string name;
    sample_probe_colour tint = sample_probe_colour::green;

    friend bool operator==(const sample_probe_leaf&, const sample_probe_leaf&) = default;
};

[[nodiscard]] constexpr auto wire_fields(const sample_probe_leaf&) {
    return std::tuple{
        utility::codec::field{"name", &sample_probe_leaf::name},
        utility::codec::field{"tint", &sample_probe_leaf::tint},
    };
}

struct sample_probe_every_member {
    std::string text;
    bool flag = true;
    std::int32_t small = 0;
    std::uint64_t large = 0;
    double ratio = 0.0;
    sample_probe_colour hue = sample_probe_colour::red;
    boost::uuids::uuid id{};
    ores::utility::uuid::tenant_id tenant = ores::utility::uuid::tenant_id::system();
    ores::utility::decimal::decimal amount;
    std::chrono::year_month_day on{};
    std::chrono::system_clock::time_point at{};
    std::chrono::milliseconds wait{0};
    std::filesystem::path file;
    std::optional<std::string> note;
    std::vector<sample_probe_leaf> leaves;
    std::map<std::string, std::int64_t> counts;
    std::optional<std::vector<boost::uuids::uuid>> ids;
    std::optional<std::optional<std::string>> maybe;
    sample_probe_leaf single;
    boost::asio::ip::address host;
    ores::utility::serialization::error_code code = ores::utility::serialization::error_code::none;
    std::int16_t signed_small = 0;
    std::vector<std::uint8_t> bytes;

    friend bool operator==(const sample_probe_every_member&,
                           const sample_probe_every_member&) = default;
};

[[nodiscard]] constexpr auto wire_fields(const sample_probe_every_member&) {
    return std::tuple{
        utility::codec::field{"text", &sample_probe_every_member::text},
        utility::codec::field{"flag", &sample_probe_every_member::flag},
        utility::codec::field{"small", &sample_probe_every_member::small},
        utility::codec::field{"large", &sample_probe_every_member::large},
        utility::codec::field{"ratio", &sample_probe_every_member::ratio},
        utility::codec::field{"hue", &sample_probe_every_member::hue},
        utility::codec::field{"id", &sample_probe_every_member::id},
        utility::codec::field{"tenant", &sample_probe_every_member::tenant},
        utility::codec::field{"amount", &sample_probe_every_member::amount},
        utility::codec::field{"on", &sample_probe_every_member::on},
        utility::codec::field{"at", &sample_probe_every_member::at},
        utility::codec::field{"wait", &sample_probe_every_member::wait},
        utility::codec::field{"file", &sample_probe_every_member::file},
        utility::codec::field{"note", &sample_probe_every_member::note},
        utility::codec::field{"leaves", &sample_probe_every_member::leaves},
        utility::codec::field{"counts", &sample_probe_every_member::counts},
        utility::codec::field{"ids", &sample_probe_every_member::ids},
        utility::codec::field{"maybe", &sample_probe_every_member::maybe},
        utility::codec::field{"single", &sample_probe_every_member::single},
        utility::codec::field{"host", &sample_probe_every_member::host},
        utility::codec::field{"code", &sample_probe_every_member::code},
        utility::codec::field{"signed_small", &sample_probe_every_member::signed_small},
        utility::codec::field{"bytes", &sample_probe_every_member::bytes},
    };
}

struct sample_probe_twins {
    std::string text_a;
    std::string text_b;
    std::int32_t int_a = 0;
    std::int32_t int_b = 0;
    std::uint16_t count_a = 0;
    std::uint16_t count_b = 0;
    double ratio_a = 0.0;
    double ratio_b = 0.0;
    boost::uuids::uuid id_a{};
    boost::uuids::uuid id_b{};
    ores::utility::decimal::decimal amount_a;
    ores::utility::decimal::decimal amount_b;
    std::chrono::system_clock::time_point at_a{};
    std::chrono::system_clock::time_point at_b{};
    std::optional<std::string> note_a;
    std::optional<std::string> note_b;
    std::vector<std::int64_t> values_a;
    std::vector<std::int64_t> values_b;
    sample_probe_leaf leaf_a;
    sample_probe_leaf leaf_b;

    friend bool operator==(const sample_probe_twins&, const sample_probe_twins&) = default;
};

[[nodiscard]] constexpr auto wire_fields(const sample_probe_twins&) {
    return std::tuple{
        utility::codec::field{"text_a", &sample_probe_twins::text_a},
        utility::codec::field{"text_b", &sample_probe_twins::text_b},
        utility::codec::field{"int_a", &sample_probe_twins::int_a},
        utility::codec::field{"int_b", &sample_probe_twins::int_b},
        utility::codec::field{"count_a", &sample_probe_twins::count_a},
        utility::codec::field{"count_b", &sample_probe_twins::count_b},
        utility::codec::field{"ratio_a", &sample_probe_twins::ratio_a},
        utility::codec::field{"ratio_b", &sample_probe_twins::ratio_b},
        utility::codec::field{"id_a", &sample_probe_twins::id_a},
        utility::codec::field{"id_b", &sample_probe_twins::id_b},
        utility::codec::field{"amount_a", &sample_probe_twins::amount_a},
        utility::codec::field{"amount_b", &sample_probe_twins::amount_b},
        utility::codec::field{"at_a", &sample_probe_twins::at_a},
        utility::codec::field{"at_b", &sample_probe_twins::at_b},
        utility::codec::field{"note_a", &sample_probe_twins::note_a},
        utility::codec::field{"note_b", &sample_probe_twins::note_b},
        utility::codec::field{"values_a", &sample_probe_twins::values_a},
        utility::codec::field{"values_b", &sample_probe_twins::values_b},
        utility::codec::field{"leaf_a", &sample_probe_twins::leaf_a},
        utility::codec::field{"leaf_b", &sample_probe_twins::leaf_b},
    };
}

struct sample_probe_code {
    std::string text = "unset";

    friend bool operator==(const sample_probe_code&, const sample_probe_code&) = default;
};

}

template <>
struct ores::utility::codec::text_form<ores::utility::codec_sample_probe::sample_probe_code> {
    static constexpr std::string_view wire_name = "a probe code";

    static constexpr std::string_view sample_texts[] = {"first", "second"};

    static std::string to_text(const ores::utility::codec_sample_probe::sample_probe_code& v) {
        return v.text;
    }

    static ores::utility::codec_sample_probe::sample_probe_code from_text(std::string_view text) {
        return {std::string(text)};
    }
};

namespace {

const std::string sample_tags("[codec][sample]");

using ores::utility::codec::sample;
using ores::utility::codec_sample_probe::sample_probe_code;
using ores::utility::codec_sample_probe::sample_probe_colour;
using ores::utility::codec_sample_probe::sample_probe_every_member;
using ores::utility::codec_sample_probe::sample_probe_leaf;
using ores::utility::codec_sample_probe::sample_probe_twins;

// The sampler's leaf specialisations win over its wire_fields branch. The codec
// tries wire_fields first, so the two agree only while no leaf states one.
static_assert(!ores::utility::codec::states_wire_form<std::string>);
static_assert(!ores::utility::codec::states_wire_form<std::filesystem::path>);
static_assert(!ores::utility::codec::states_wire_form<boost::uuids::uuid>);
static_assert(!ores::utility::codec::states_wire_form<ores::utility::uuid::tenant_id>);
static_assert(!ores::utility::codec::states_wire_form<ores::utility::decimal::decimal>);
static_assert(!ores::utility::codec::states_wire_form<std::chrono::year_month_day>);
static_assert(!ores::utility::codec::states_wire_form<std::chrono::system_clock::time_point>);
static_assert(!ores::utility::codec::states_wire_form<boost::asio::ip::address>);
static_assert(!ores::utility::codec::states_wire_form<ores::utility::serialization::error_code>);

std::span<const std::byte> sample_bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

}

TEST_CASE("a sampled struct sets every member away from its default", sample_tags) {
    const auto v = sample<sample_probe_every_member>(0);
    const sample_probe_every_member d{};
    CHECK(v.text != d.text);
    CHECK(v.flag != d.flag);
    CHECK(v.small != d.small);
    CHECK(v.large != d.large);
    CHECK(v.ratio != d.ratio);
    CHECK(v.hue != d.hue);
    CHECK(v.id != d.id);
    CHECK(v.tenant != d.tenant);
    CHECK(v.amount != d.amount);
    CHECK(v.on != d.on);
    CHECK(v.at != d.at);
    CHECK(v.wait != d.wait);
    CHECK(v.file != d.file);
    CHECK(v.note != d.note);
    CHECK(v.leaves != d.leaves);
    CHECK(v.counts != d.counts);
    CHECK(v.ids != d.ids);
    CHECK(v.maybe != d.maybe);
    CHECK(v.single != d.single);
    CHECK(v.host != d.host);
    CHECK(v.code != d.code);
    CHECK(v.signed_small != d.signed_small);
    CHECK(v.bytes != d.bytes);
}

TEST_CASE("a member whose default is the first sample takes the second", sample_tags) {
    CHECK(sample<sample_probe_colour>(0) == sample_probe_colour::green);
    CHECK(ores::utility::codec::sample_unlike(sample_probe_colour::green) ==
          sample_probe_colour::red);
    CHECK(sample<sample_probe_every_member>(0).flag == false);
}

TEST_CASE("two members of one type hold different values", sample_tags) {
    for (std::size_t which = 0; which < 1000; ++which) {
        CAPTURE(which);
        const auto v = sample<sample_probe_twins>(which);
        CHECK(v.text_a != v.text_b);
        CHECK(v.int_a != v.int_b);
        CHECK(v.count_a != v.count_b);
        CHECK(v.ratio_a != v.ratio_b);
        CHECK(v.id_a != v.id_b);
        CHECK(v.amount_a != v.amount_b);
        CHECK(v.at_a != v.at_b);
        CHECK(v.note_a != v.note_b);
        CHECK(v.values_a != v.values_b);
        CHECK(v.leaf_a != v.leaf_b);
    }
}

TEST_CASE("the twins sample survives JSON and MessagePack exactly", sample_tags) {
    using namespace ores::utility::codec;
    for (std::size_t which = 0; which < 1000; ++which) {
        CAPTURE(which);
        const auto v = sample<sample_probe_twins>(which);
        CHECK(object::read_json<sample_probe_twins>(object::write_json(v)) == v);
        const auto packed = object::write_msgpack(v);
        CHECK(object::read_msgpack<sample_probe_twins>(sample_bytes_of(packed)) == v);
    }
}

TEST_CASE("a vector sample holds two different elements", sample_tags) {
    const auto v = sample<std::vector<sample_probe_leaf>>(0);
    REQUIRE(v.size() == 2);
    CHECK(v[0] != v[1]);
}

TEST_CASE("the second sample differs from the first in every member", sample_tags) {
    const auto a = sample<sample_probe_every_member>(0);
    const auto b = sample<sample_probe_every_member>(1);
    CHECK(a.text != b.text);
    CHECK(a.small != b.small);
    CHECK(a.ratio != b.ratio);
    CHECK(a.amount != b.amount);
    CHECK(a.at != b.at);
    CHECK(a.leaves != b.leaves);
    CHECK(a.single != b.single);
    CHECK(a.host != b.host);
    CHECK(a.code != b.code);
    CHECK(b.small < 0);
    CHECK(b.signed_small < 0);
}

TEST_CASE("both samples survive JSON and MessagePack exactly", sample_tags) {
    using namespace ores::utility::codec;
    for (const std::size_t which : {0u, 1u}) {
        CAPTURE(which);
        const auto v = sample<sample_probe_every_member>(which);
        CHECK(object::read_json<sample_probe_every_member>(object::write_json(v)) == v);
        const auto packed = object::write_msgpack(v);
        CHECK(object::read_msgpack<sample_probe_every_member>(sample_bytes_of(packed)) == v);
    }
}

TEST_CASE("a type with its own text form samples through its sample texts", sample_tags) {
    CHECK(sample<sample_probe_code>(0).text == "first");
    CHECK(sample<sample_probe_code>(1).text == "second");
    CHECK(ores::utility::codec::sample_unlike(sample_probe_code{"first"}).text == "second");
}

TEST_CASE("an instant sample survives its text form at any sample number", sample_tags) {
    using clock_point = std::chrono::system_clock::time_point;
    using form = ores::utility::codec::text_form<clock_point>;
    for (std::size_t which = 0; which < 20000; which += 7) {
        CAPTURE(which);
        const auto v = sample<clock_point>(which);
        REQUIRE(form::from_text(form::to_text(v)) == v);
    }
}
