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
#ifndef ORES_DATABASE_PQ_CODEC_HPP
#define ORES_DATABASE_PQ_CODEC_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/connection.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <bit>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::database::pq {

/**
 * @brief One parameter of a statement, in the binary format.
 */
struct param final {
    std::string bytes;
    bool null = false;
    Oid oid = 0;
    /// A text parameter, typed by the server from where it is used.
    bool text = false;
};

namespace wire {

inline void put_u16(std::string& out, std::uint16_t v) {
    out += static_cast<char>(v >> 8);
    out += static_cast<char>(v);
}

inline void put_u32(std::string& out, std::uint32_t v) {
    put_u16(out, static_cast<std::uint16_t>(v >> 16));
    put_u16(out, static_cast<std::uint16_t>(v));
}

inline void put_u64(std::string& out, std::uint64_t v) {
    put_u32(out, static_cast<std::uint32_t>(v >> 32));
    put_u32(out, static_cast<std::uint32_t>(v));
}

inline std::uint16_t get_u16(std::span<const std::byte> in, std::size_t at = 0) {
    return static_cast<std::uint16_t>(std::to_integer<std::uint16_t>(in[at]) << 8 |
                                      std::to_integer<std::uint16_t>(in[at + 1]));
}

inline std::uint32_t get_u32(std::span<const std::byte> in, std::size_t at = 0) {
    return static_cast<std::uint32_t>(get_u16(in, at)) << 16 | get_u16(in, at + 2);
}

inline std::uint64_t get_u64(std::span<const std::byte> in, std::size_t at = 0) {
    return static_cast<std::uint64_t>(get_u32(in, at)) << 32 | get_u32(in, at + 4);
}

/**
 * @brief Refuses a value whose length is not the one its type has.
 */
ORES_DATABASE_EXPORT void
expect_size(std::span<const std::byte> in, std::size_t size, std::string_view type);

/**
 * @brief Refuses a value shorter than the header it has to read first.
 */
ORES_DATABASE_EXPORT void
expect_min_size(std::span<const std::byte> in, std::size_t size, std::string_view type);

/**
 * @brief Refuses a NULL element in an array read into a vector.
 */
[[noreturn]] ORES_DATABASE_EXPORT void null_element(std::uint32_t index);

/**
 * @brief Refuses an unsigned value the column's signed type cannot hold.
 *
 * The database has no unsigned integer, so a width it stores is read back with
 * the sign bit as the value's top bit. A value past the signed maximum would
 * travel as a negative number rather than fail, so it is refused here.
 */
[[noreturn]] ORES_DATABASE_EXPORT void refuse_unsigned(std::uint64_t v, std::string_view type);

}

/**
 * @brief The binary encoding of a type: its OID, how to write it and how to
 * read it. A type the layer does not know has no codec, so a statement that
 * names it does not compile.
 */
template <typename T>
struct codec;

template <>
struct codec<bool> {
    static constexpr Oid oid = 16;
    static constexpr Oid array_oid = 1000;
    static void encode(bool v, std::string& out) {
        out += static_cast<char>(v ? 1 : 0);
    }
    static bool decode(std::span<const std::byte> in) {
        wire::expect_size(in, 1, "bool");
        return in[0] != std::byte{0};
    }
};

template <>
struct codec<std::int16_t> {
    static constexpr Oid oid = 21;
    static constexpr Oid array_oid = 1005;
    static void encode(std::int16_t v, std::string& out) {
        wire::put_u16(out, static_cast<std::uint16_t>(v));
    }
    static std::int16_t decode(std::span<const std::byte> in) {
        wire::expect_size(in, 2, "int2");
        return static_cast<std::int16_t>(wire::get_u16(in));
    }
};

/**
 * @brief smallint held unsigned: the database has no unsigned type, so the
 * column's own width is what travels. A value past the signed maximum is
 * refused, because the bits would otherwise store a negative number.
 */
template <>
struct codec<std::uint16_t> {
    static constexpr Oid oid = 21;
    static constexpr Oid array_oid = 1005;
    static void encode(std::uint16_t v, std::string& out) {
        if (v > static_cast<std::uint16_t>(std::numeric_limits<std::int16_t>::max()))
            wire::refuse_unsigned(v, "smallint");
        wire::put_u16(out, v);
    }
    static std::uint16_t decode(std::span<const std::byte> in) {
        wire::expect_size(in, 2, "int2");
        return wire::get_u16(in);
    }
};

template <>
struct codec<std::int32_t> {
    static constexpr Oid oid = 23;
    static constexpr Oid array_oid = 1007;
    static void encode(std::int32_t v, std::string& out) {
        wire::put_u32(out, static_cast<std::uint32_t>(v));
    }
    static std::int32_t decode(std::span<const std::byte> in) {
        wire::expect_size(in, 4, "int4");
        return static_cast<std::int32_t>(wire::get_u32(in));
    }
};

template <>
struct codec<std::int64_t> {
    static constexpr Oid oid = 20;
    static constexpr Oid array_oid = 1016;
    static void encode(std::int64_t v, std::string& out) {
        wire::put_u64(out, static_cast<std::uint64_t>(v));
    }
    static std::int64_t decode(std::span<const std::byte> in) {
        wire::expect_size(in, 8, "int8");
        return static_cast<std::int64_t>(wire::get_u64(in));
    }
};

/**
 * @brief bigint held unsigned: the bits travel as the column's own eight
 * bytes. A value past the signed maximum is refused, because the bits would
 * otherwise store a negative number.
 */
template <>
struct codec<std::uint64_t> {
    static constexpr Oid oid = 20;
    static constexpr Oid array_oid = 1016;
    static void encode(std::uint64_t v, std::string& out) {
        if (v > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
            wire::refuse_unsigned(v, "bigint");
        wire::put_u64(out, v);
    }
    static std::uint64_t decode(std::span<const std::byte> in) {
        wire::expect_size(in, 8, "int8");
        return wire::get_u64(in);
    }
};

template <>
struct codec<double> {
    static constexpr Oid oid = 701;
    static constexpr Oid array_oid = 1022;
    static void encode(double v, std::string& out) {
        wire::put_u64(out, std::bit_cast<std::uint64_t>(v));
    }
    static double decode(std::span<const std::byte> in) {
        wire::expect_size(in, 8, "float8");
        return std::bit_cast<double>(wire::get_u64(in));
    }
};

template <>
struct codec<std::string> {
    static constexpr Oid oid = 25;
    static constexpr Oid array_oid = 1009;
    static void encode(std::string_view v, std::string& out) {
        out += v;
    }
    static std::string decode(std::span<const std::byte> in) {
        return {reinterpret_cast<const char*>(in.data()), in.size()};
    }
};

template <>
struct codec<std::string_view> : codec<std::string> {};

template <>
struct codec<boost::uuids::uuid> {
    static constexpr Oid oid = 2950;
    static constexpr Oid array_oid = 2951;
    static void encode(const boost::uuids::uuid& v, std::string& out) {
        out.append(reinterpret_cast<const char*>(&*v.begin()), 16);
    }
    static boost::uuids::uuid decode(std::span<const std::byte> in) {
        wire::expect_size(in, 16, "uuid");
        boost::uuids::uuid v;
        std::memcpy(&*v.begin(), in.data(), 16);
        return v;
    }
};

template <>
struct codec<utility::uuid::tenant_id> {
    static constexpr Oid oid = 2950;
    static constexpr Oid array_oid = 2951;
    static void encode(const utility::uuid::tenant_id& v, std::string& out) {
        codec<boost::uuids::uuid>::encode(v.to_uuid(), out);
    }
    ORES_DATABASE_EXPORT static utility::uuid::tenant_id decode(std::span<const std::byte> in);
};

/**
 * @brief timestamptz: microseconds since 2000-01-01 UTC. The infinities map
 * to the time point's minimum and maximum.
 *
 * A time point counts nanoseconds and stops in 2262. Its maximum is the
 * bitemporal never, and encodes as the database's 9999-12-31 23:59:59; every
 * value read at or past the clock's end decodes as that maximum, so a real
 * timestamp after 2262 cannot be told from never.
 */
template <>
struct codec<std::chrono::system_clock::time_point> {
    static constexpr Oid oid = 1184;
    static constexpr Oid array_oid = 1185;
    ORES_DATABASE_EXPORT static void encode(std::chrono::system_clock::time_point v,
                                            std::string& out);
    ORES_DATABASE_EXPORT static std::chrono::system_clock::time_point
    decode(std::span<const std::byte> in);
};

/**
 * @brief date: days since 2000-01-01.
 */
template <>
struct codec<std::chrono::year_month_day> {
    static constexpr Oid oid = 1082;
    static constexpr Oid array_oid = 1182;
    ORES_DATABASE_EXPORT static void encode(std::chrono::year_month_day v, std::string& out);
    ORES_DATABASE_EXPORT static std::chrono::year_month_day decode(std::span<const std::byte> in);
};

/**
 * @brief numeric: base-10000 digits, a weight, a sign and a display scale,
 * carried exactly through the project's decimal.
 */
template <>
struct codec<utility::decimal::decimal> {
    static constexpr Oid oid = 1700;
    static constexpr Oid array_oid = 1231;
    ORES_DATABASE_EXPORT static void encode(const utility::decimal::decimal& v, std::string& out);
    ORES_DATABASE_EXPORT static utility::decimal::decimal decode(std::span<const std::byte> in);
};

namespace detail {

template <typename T>
struct is_optional : std::false_type {};
template <typename T>
struct is_optional<std::optional<T>> : std::true_type {};

}

/**
 * @brief A one-dimensional array of a type that has a codec.
 */
template <typename T>
struct codec<std::vector<T>> {
    static constexpr Oid oid = codec<T>::array_oid;
    static void encode(const std::vector<T>& v, std::string& out) {
        wire::put_u32(out, v.empty() ? 0 : 1);
        wire::put_u32(out, 0);
        wire::put_u32(out, codec<T>::oid);
        if (v.empty())
            return;
        wire::put_u32(out, static_cast<std::uint32_t>(v.size()));
        wire::put_u32(out, 1);
        for (const auto& e : v) {
            std::string element;
            codec<T>::encode(e, element);
            wire::put_u32(out, static_cast<std::uint32_t>(element.size()));
            out += element;
        }
    }
    static std::vector<T> decode(std::span<const std::byte> in) {
        // The header is the dimension count, the NULL flag and the element
        // type, and it is all an empty array holds. A one-dimensional array
        // then states its length and its lower bound.
        wire::expect_min_size(in, 12, "array");
        const auto dims = wire::get_u32(in, 0);
        std::vector<T> out;
        if (dims == 0)
            return out;
        wire::expect_min_size(in, 20, "array");
        const auto count = wire::get_u32(in, 12);
        std::size_t at = 20;
        out.reserve(count);
        for (std::uint32_t i = 0; i < count; ++i) {
            wire::expect_min_size(in, at + 4, "array");
            const auto len = static_cast<std::int32_t>(wire::get_u32(in, at));
            at += 4;
            if (len < 0)
                wire::null_element(i);
            wire::expect_min_size(in, at + static_cast<std::size_t>(len), "array");
            out.push_back(codec<T>::decode(in.subspan(at, static_cast<std::size_t>(len))));
            at += static_cast<std::size_t>(len);
        }
        return out;
    }
};

/**
 * @brief The OID a parameter of type T is prepared with.
 */
template <typename T>
constexpr Oid oid_of() {
    if constexpr (detail::is_optional<T>::value)
        return codec<typename T::value_type>::oid;
    else
        return codec<T>::oid;
}

/**
 * @brief Encodes one parameter; an empty optional is a NULL.
 */
template <typename T>
param encode_param(const T& v) {
    param p;
    p.oid = oid_of<T>();
    if constexpr (detail::is_optional<T>::value) {
        if (!v) {
            p.null = true;
            return p;
        }
        codec<typename T::value_type>::encode(*v, p.bytes);
    } else {
        codec<T>::encode(v, p.bytes);
    }
    return p;
}

/**
 * @brief Refuses a NULL read into a column type that cannot hold one.
 */
[[noreturn]] ORES_DATABASE_EXPORT void unexpected_null(int row, int column);

/**
 * @brief Reads one column of one row; an optional takes a NULL.
 */
template <typename T>
T read(const result& r, int row, int column) {
    if constexpr (detail::is_optional<T>::value) {
        if (r.is_null(row, column))
            return std::nullopt;
        return codec<typename T::value_type>::decode(r.value(row, column));
    } else {
        if (r.is_null(row, column))
            unexpected_null(row, column);
        return codec<T>::decode(r.value(row, column));
    }
}

}

#endif
