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
#ifndef ORES_COMPUTE_API_MESSAGING_ASSIGNMENT_EXPIRY_HPP
#define ORES_COMPUTE_API_MESSAGING_ASSIGNMENT_EXPIRY_HPP

#include <array>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::compute::messaging {

namespace detail {

/**
 * @brief The bytes of a base64url string, or nothing when it is not one.
 *
 * A JWT segment is base64url without padding. Only the characters of that
 * alphabet are accepted.
 */
inline std::optional<std::string> decode_base64url(std::string_view text) {
    static constexpr std::string_view alphabet =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    std::array<int, 256> value{};
    value.fill(-1);
    for (std::size_t i = 0; i < alphabet.size(); ++i)
        value[static_cast<unsigned char>(alphabet[i])] = static_cast<int>(i);

    std::string out;
    std::uint32_t buffer = 0;
    int bits = 0;
    for (const char c : text) {
        const int v = value[static_cast<unsigned char>(c)];
        if (v < 0)
            return std::nullopt;
        buffer = (buffer << 6) | static_cast<std::uint32_t>(v);
        bits += 6;
        if (bits >= 8) {
            bits -= 8;
            out.push_back(static_cast<char>((buffer >> bits) & 0xFF));
        }
    }
    return out;
}

} // namespace detail

/**
 * @brief The instant a bearer token stops working, read from its claims.
 *
 * The signature is not checked, because the answer is only used to avoid
 * starting work that the storage service will refuse: a node that reads a wrong
 * expiry still meets the real check at the storage service. A token whose claims
 * cannot be read, or that states no expiry, gives nothing.
 */
inline std::optional<std::chrono::system_clock::time_point> token_expiry(std::string_view token) {
    const auto first = token.find('.');
    if (first == std::string_view::npos)
        return std::nullopt;
    const auto second = token.find('.', first + 1);
    if (second == std::string_view::npos)
        return std::nullopt;

    const auto claims = detail::decode_base64url(token.substr(first + 1, second - first - 1));
    if (!claims)
        return std::nullopt;

    static constexpr std::string_view key = "\"exp\":";
    const auto at = claims->find(key);
    if (at == std::string::npos)
        return std::nullopt;

    std::size_t i = at + key.size();
    while (i < claims->size() && (*claims)[i] == ' ')
        ++i;
    // A date in seconds has ten digits for the next centuries. A longer run is not
    // a date a node can read, and reading it could overflow the count.
    constexpr std::size_t max_digits = 12;
    std::int64_t seconds = 0;
    std::size_t digits = 0;
    while (i < claims->size() && (*claims)[i] >= '0' && (*claims)[i] <= '9') {
        if (++digits > max_digits)
            return std::nullopt;
        seconds = seconds * 10 + ((*claims)[i] - '0');
        ++i;
    }
    if (digits == 0)
        return std::nullopt;
    // A number in exponent form, such as 1.7e9, would read as its mantissa and make
    // a live token look ended. The storage service decides such a token. The
    // fractional digits are skipped first, so 1.7e9 is seen as an exponent form.
    std::size_t after = i;
    if (after < claims->size() && (*claims)[after] == '.') {
        ++after;
        while (after < claims->size() && (*claims)[after] >= '0' && (*claims)[after] <= '9')
            ++after;
    }
    if (after < claims->size() && ((*claims)[after] == 'e' || (*claims)[after] == 'E'))
        return std::nullopt;
    return std::chrono::system_clock::time_point{std::chrono::seconds{seconds}};
}

/**
 * @brief Whether a node can tell that an assignment's storage token has run out.
 *
 * A node takes an assignment from a queue where it may have waited for any time,
 * and the token it carries lives for a fixed time from when it was minted. A node
 * that sees the token has ended must not start the job: every storage request
 * would be refused.
 */
inline bool assignment_expired(std::string_view token,
                               std::chrono::system_clock::time_point now) {
    const auto expiry = token_expiry(token);
    return expiry && *expiry <= now;
}

} // namespace ores::compute::messaging

#endif
