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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/pq/codec.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.database/pq/text.hpp"
#include <charconv>
#include <limits>

namespace ores::database::pq {

namespace {

using namespace std::chrono;

constexpr std::int64_t micros_per_day = 86'400'000'000;

constexpr std::int64_t days_since_2000(year_month_day d) {
    return (sys_days(d) - sys_days(year{2000} / January / 1)).count();
}

/*
 * The bitemporal "never": the timestamp the database's infinity function
 * returns. A time point counts nanoseconds and stops in 2262, so it stands for
 * this instant with its maximum.
 */
constexpr std::int64_t never_micros =
    days_since_2000(year{9999} / December / 31) * micros_per_day + 86'399'000'000;

constexpr std::int64_t unix_to_2000_micros = 946'684'800'000'000;

[[noreturn]] void refuse(std::string message) {
    throw store_error(std::move(message), "22000", error_kind::refused);
}

}

void unreadable_text(std::string_view text, std::string_view type) {
    refuse("Not a " + std::string(type) + ": " + std::string(text));
}

param to_text(double v) {
    char buffer[32];
    const auto [end, ec] = std::to_chars(buffer, buffer + sizeof buffer, v);
    return text_param(std::string(buffer, end));
}

void wire::expect_size(std::span<const std::byte> in, std::size_t size, std::string_view type) {
    if (in.size() != size)
        refuse("A " + std::string(type) + " value has " + std::to_string(in.size()) +
               " bytes, not " + std::to_string(size));
}

void wire::null_element(std::uint32_t index) {
    refuse("Array element " + std::to_string(index) + " is NULL");
}

void unexpected_null(int row, int column) {
    refuse("Row " + std::to_string(row) + " column " + std::to_string(column) +
           " is NULL in a column that cannot hold one");
}

utility::uuid::tenant_id codec<utility::uuid::tenant_id>::decode(std::span<const std::byte> in) {
    auto id = utility::uuid::tenant_id::from_uuid(codec<boost::uuids::uuid>::decode(in));
    if (!id)
        refuse("Not a tenant id: " + id.error());
    return *id;
}

void codec<system_clock::time_point>::encode(system_clock::time_point v, std::string& out) {
    if (v == system_clock::time_point::max()) {
        wire::put_u64(out, static_cast<std::uint64_t>(never_micros));
        return;
    }
    if (v == system_clock::time_point::min()) {
        wire::put_u64(out, static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::min()));
        return;
    }
    const auto unix_micros = duration_cast<microseconds>(v.time_since_epoch()).count();
    wire::put_u64(out, static_cast<std::uint64_t>(unix_micros - unix_to_2000_micros));
}

system_clock::time_point codec<system_clock::time_point>::decode(std::span<const std::byte> in) {
    wire::expect_size(in, 8, "timestamptz");
    const auto micros = static_cast<std::int64_t>(wire::get_u64(in));
    constexpr auto latest =
        duration_cast<microseconds>(system_clock::time_point::max().time_since_epoch()).count() -
        unix_to_2000_micros;
    constexpr auto earliest =
        duration_cast<microseconds>(system_clock::time_point::min().time_since_epoch()).count() -
        unix_to_2000_micros;
    if (micros >= latest)
        return system_clock::time_point::max();
    if (micros <= earliest)
        return system_clock::time_point::min();
    return system_clock::time_point(
        duration_cast<system_clock::duration>(microseconds(micros + unix_to_2000_micros)));
}

void codec<year_month_day>::encode(year_month_day v, std::string& out) {
    wire::put_u32(out, static_cast<std::uint32_t>(static_cast<std::int32_t>(days_since_2000(v))));
}

year_month_day codec<year_month_day>::decode(std::span<const std::byte> in) {
    wire::expect_size(in, 4, "date");
    const auto days = static_cast<std::int32_t>(wire::get_u32(in));
    return year_month_day(sys_days(year{2000} / January / 1) + std::chrono::days(days));
}

void codec<utility::decimal::decimal>::encode(const utility::decimal::decimal& v,
                                              std::string& out) {
    std::string text = v.to_string();
    const bool negative = !text.empty() && text.front() == '-';
    if (negative || (!text.empty() && text.front() == '+'))
        text.erase(0, 1);
    const auto point = text.find('.');
    std::string whole = point == std::string::npos ? text : text.substr(0, point);
    std::string fraction = point == std::string::npos ? std::string() : text.substr(point + 1);
    whole.erase(0, whole.find_first_not_of('0'));
    const auto dscale = static_cast<std::uint16_t>(fraction.size());

    whole.insert(0, (4 - whole.size() % 4) % 4, '0');
    fraction.append((4 - fraction.size() % 4) % 4, '0');
    std::vector<std::uint16_t> digits;
    for (std::size_t i = 0; i < whole.size(); i += 4)
        digits.push_back(static_cast<std::uint16_t>(std::stoi(whole.substr(i, 4))));
    auto weight = static_cast<std::int16_t>(static_cast<int>(digits.size()) - 1);
    for (std::size_t i = 0; i < fraction.size(); i += 4)
        digits.push_back(static_cast<std::uint16_t>(std::stoi(fraction.substr(i, 4))));

    std::size_t first = 0;
    while (first < digits.size() && digits[first] == 0) {
        ++first;
        --weight;
    }
    std::size_t last = digits.size();
    while (last > first && digits[last - 1] == 0)
        --last;
    const bool zero = first == last;

    wire::put_u16(out, static_cast<std::uint16_t>(last - first));
    wire::put_u16(out, static_cast<std::uint16_t>(zero ? 0 : weight));
    wire::put_u16(out, negative && !zero ? 0x4000 : 0x0000);
    wire::put_u16(out, dscale);
    for (std::size_t i = first; i < last; ++i)
        wire::put_u16(out, digits[i]);
}

utility::decimal::decimal codec<utility::decimal::decimal>::decode(std::span<const std::byte> in) {
    if (in.size() < 8)
        refuse("A numeric value has fewer than 8 bytes");
    const auto ndigits = wire::get_u16(in, 0);
    const auto weight = static_cast<std::int16_t>(wire::get_u16(in, 2));
    const auto sign = wire::get_u16(in, 4);
    const auto dscale = wire::get_u16(in, 6);
    wire::expect_size(in, 8 + 2 * static_cast<std::size_t>(ndigits), "numeric");
    if (sign != 0x0000 && sign != 0x4000)
        refuse("A numeric value is NaN or infinite, which a decimal cannot hold");

    const auto digit = [&](int index) -> std::uint16_t {
        return index >= 0 && index < ndigits ? wire::get_u16(in, 8 + 2 * index) : 0;
    };

    std::string text = sign == 0x4000 ? "-" : "";
    if (weight < 0) {
        text += '0';
    } else {
        text += std::to_string(digit(0));
        for (int i = 1; i <= weight; ++i) {
            const auto group = std::to_string(digit(i));
            text.append(4 - group.size(), '0');
            text += group;
        }
    }
    if (dscale > 0) {
        std::string fraction;
        for (int i = weight + 1; fraction.size() < dscale; ++i) {
            const auto group = std::to_string(digit(i));
            fraction.append(4 - group.size(), '0');
            fraction += group;
        }
        fraction.resize(dscale);
        text += '.';
        text += fraction;
    }
    auto value = utility::decimal::decimal::from_string(text);
    if (!value)
        refuse("Not a decimal: " + text + ": " + value.error());
    return *value;
}

}
