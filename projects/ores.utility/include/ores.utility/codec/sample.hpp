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
#ifndef ORES_UTILITY_CODEC_SAMPLE_HPP
#define ORES_UTILITY_CODEC_SAMPLE_HPP

#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <limits>
#include <magic_enum/magic_enum.hpp>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

/**
 * @brief Non-default values for every type the wire codec writes.
 *
 * A roundtrip test fills a value with these, writes it, reads it back and
 * requires the same value. Every member must differ from its default, or a
 * codec that drops the member would still pass. The dispatch covers the
 * codec's set: a struct that states its wire_fields, a type with a text form,
 * the scalars, and the optional, vector, map and duration wrappers. A type
 * outside that set fails to compile here, which names the gap.
 *
 * Each type has two samples. In the first, every member of a struct differs
 * from its default. The second differs from the first in every member, and a
 * member whose modelled default equals the first takes it. A signed integer's
 * second sample is negative. Every value is held exactly by both formats: no
 * NaN and no computed double.
 *
 * An enumeration takes its values from magic_enum, which sees only those in
 * its range, -128 to 128 by default.
 */
namespace ores::utility::codec {

template <typename T>
T sample(std::size_t which);

template <std::equality_comparable T>
T sample_unlike(const T& avoid) {
    auto first = sample<T>(0);
    if (first == avoid)
        return sample<T>(1);
    return first;
}

namespace detail {

template <typename T>
inline constexpr bool unsampled = false;

template <typename T>
inline constexpr bool is_character =
    std::is_same_v<T, char> || std::is_same_v<T, signed char> ||
    std::is_same_v<T, unsigned char> || std::is_same_v<T, wchar_t> ||
    std::is_same_v<T, char8_t> || std::is_same_v<T, char16_t> || std::is_same_v<T, char32_t>;

template <typename T>
struct sampler {
    static T make(std::size_t which) {
        if constexpr (states_wire_form<T>) {
            const auto unlike = [](const T& avoid) {
                T out{};
                std::apply(
                    [&](const auto&... fields) {
                        ((out.*(fields.member) = sample_unlike(avoid.*(fields.member))), ...);
                    },
                    wire_fields(out));
                return out;
            };
            // The second sample differs from the first in every member, so two
            // elements of a vector of structs are never equal.
            const auto first = unlike(T{});
            return which == 0 ? first : unlike(first);
        } else if constexpr (std::is_enum_v<T>) {
            constexpr auto values = magic_enum::enum_values<T>();
            static_assert(!values.empty(), "an enumeration with no enumerators");
            constexpr std::size_t first = values.size() > 1 ? 1 : 0;
            return values[which == 0 ? first : 0];
        } else if constexpr (std::is_same_v<T, bool>) {
            return which == 0;
        } else if constexpr (std::is_integral_v<T> && !is_character<T>) {
            if (which == 0)
                return static_cast<T>(std::numeric_limits<T>::max() - 1);
            if constexpr (std::is_signed_v<T>)
                return static_cast<T>(std::numeric_limits<T>::min() + 1);
            return static_cast<T>(1);
        } else if constexpr (std::is_same_v<T, double>) {
            return which == 0 ? 1.5 : -0.25;
        } else {
            static_assert(unsampled<T>, "the wire codec writes a type with no sample");
        }
    }
};

template <>
struct sampler<std::string> {
    static std::string make(std::size_t which) {
        return which == 0 ? std::string("a \"quoted\" back\\slash\nline \xc3\xa9") : "second";
    }
};

template <>
struct sampler<std::filesystem::path> {
    static std::filesystem::path make(std::size_t which) {
        return which == 0 ? "dir/sub dir/file.txt" : "other";
    }
};

template <>
struct sampler<boost::uuids::uuid> {
    static boost::uuids::uuid make(std::size_t which) {
        boost::uuids::uuid out{};
        std::uint8_t next = which == 0 ? 0xa0 : 0x10;
        for (auto& byte : out)
            byte = next++;
        return out;
    }
};

template <>
struct sampler<ores::utility::uuid::tenant_id> {
    static ores::utility::uuid::tenant_id make(std::size_t which) {
        const auto text = which == 0 ? "a0a1a2a3-a4a5-a6a7-a8a9-aaabacadaeaf"
                                     : "10111213-1415-1617-1819-1a1b1c1d1e1f";
        auto out = ores::utility::uuid::tenant_id::from_string(text);
        if (!out)
            throw std::logic_error("the tenant id sample is not a tenant id: " + out.error());
        return *out;
    }
};

template <>
struct sampler<ores::utility::decimal::decimal> {
    static ores::utility::decimal::decimal make(std::size_t which) {
        const auto text = which == 0 ? "12345678901234567.25" : "-0.5";
        auto out = ores::utility::decimal::decimal::from_string(text);
        if (!out)
            throw std::logic_error("the decimal sample is not a decimal: " + out.error());
        return *out;
    }
};

template <>
struct sampler<std::chrono::year_month_day> {
    static std::chrono::year_month_day make(std::size_t which) {
        using namespace std::chrono;
        return which == 0 ? year_month_day{year{2026}, month{3}, day{14}}
                          : year_month_day{year{1999}, month{12}, day{31}};
    }
};

template <>
struct sampler<std::chrono::system_clock::time_point> {
    static std::chrono::system_clock::time_point make(std::size_t which) {
        using namespace std::chrono;
        const auto day = sys_days{sampler<year_month_day>::make(which)};
        return which == 0 ? day + hours{15} + minutes{9} + seconds{26} + microseconds{535897}
                          : day + hours{23} + minutes{59} + seconds{59};
    }
};

template <>
struct sampler<boost::asio::ip::address> {
    static boost::asio::ip::address make(std::size_t which) {
        return boost::asio::ip::make_address(which == 0 ? "2001:db8::1" : "192.0.2.7");
    }
};

template <>
struct sampler<ores::utility::serialization::error_code> {
    static ores::utility::serialization::error_code make(std::size_t which) {
        using ores::utility::serialization::error_code;
        return which == 0 ? error_code::version_mismatch : error_code::crc_validation_failed;
    }
};

template <typename T>
struct sampler<std::optional<T>> {
    static std::optional<T> make(std::size_t which) {
        return sample<T>(which);
    }
};

template <typename T>
struct sampler<std::vector<T>> {
    static std::vector<T> make(std::size_t which) {
        if (which == 0)
            return {sample<T>(0), sample<T>(1)};
        return {sample<T>(1)};
    }
};

template <typename T>
struct sampler<std::map<std::string, T>> {
    static std::map<std::string, T> make(std::size_t which) {
        if (which == 0)
            return {{"first", sample<T>(0)}, {"second", sample<T>(1)}};
        return {{"only", sample<T>(1)}};
    }
};

template <typename Rep, typename Period>
struct sampler<std::chrono::duration<Rep, Period>> {
    static std::chrono::duration<Rep, Period> make(std::size_t which) {
        return std::chrono::duration<Rep, Period>(which == 0 ? Rep(1234) : Rep(7));
    }
};

}

template <typename T>
T sample(std::size_t which) {
    return detail::sampler<T>::make(which);
}

}

#endif
