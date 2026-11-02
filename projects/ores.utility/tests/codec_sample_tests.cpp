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

    friend bool operator==(const sample_probe_every_member&, const sample_probe_every_member&) = default;
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
    };
}

}

namespace {

const std::string sample_tags("[codec][sample]");

using ores::utility::codec::sample;
using ores::utility::codec_sample_probe::sample_probe_colour;
using ores::utility::codec_sample_probe::sample_probe_every_member;
using ores::utility::codec_sample_probe::sample_probe_leaf;

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
}

TEST_CASE("a member whose default is the first sample takes the second", sample_tags) {
    CHECK(sample<sample_probe_colour>(0) == sample_probe_colour::green);
    CHECK(sample<sample_probe_leaf>(0).tint == sample_probe_colour::red);
    CHECK(sample<sample_probe_every_member>(0).flag == false);
}

TEST_CASE("a vector sample holds two different elements", sample_tags) {
    const auto v = sample<std::vector<sample_probe_leaf>>(0);
    REQUIRE(v.size() == 2);
    CHECK(v[0] != v[1]);
}

TEST_CASE("every sample survives JSON and MessagePack exactly", sample_tags) {
    using namespace ores::utility::codec;
    const auto v = sample<sample_probe_every_member>(0);

    CHECK(object::read_json<sample_probe_every_member>(object::write_json(v)) == v);
    const auto packed = object::write_msgpack(v);
    CHECK(object::read_msgpack<sample_probe_every_member>(sample_bytes_of(packed)) == v);
}
