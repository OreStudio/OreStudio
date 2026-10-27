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
#ifndef ORES_ANALYTICS_QUANT_MATH_RNG_STREAM_HPP
#define ORES_ANALYTICS_QUANT_MATH_RNG_STREAM_HPP

#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string_view>

namespace ores::analytics::quant::math {

/**
 * @brief One block of the Philox4x32-10 counter-based generator.
 *
 * Maps a 128-bit counter and a 64-bit key to 128 random bits in ten rounds, as
 * published by Salmon et al., "Parallel random numbers: as easy as 1, 2, 3"
 * (SC 2011), and as shipped in Random123. A pure function of its arguments, so
 * any draw of a stream can be computed without the draws before it.
 */
constexpr std::array<std::uint32_t, 4> philox4x32_10(std::array<std::uint32_t, 4> counter,
                                                     std::array<std::uint32_t, 2> key) {
    constexpr std::uint64_t m0 = 0xD2511F53u;
    constexpr std::uint64_t m1 = 0xCD9E8D57u;
    constexpr std::uint32_t w0 = 0x9E3779B9u;
    constexpr std::uint32_t w1 = 0xBB67AE85u;

    for (int round = 0; round < 10; ++round) {
        if (round > 0) {
            key[0] += w0;
            key[1] += w1;
        }
        const std::uint64_t p0 = m0 * counter[0];
        const std::uint64_t p1 = m1 * counter[2];
        counter = {static_cast<std::uint32_t>(p1 >> 32) ^ counter[1] ^ key[0],
                   static_cast<std::uint32_t>(p1),
                   static_cast<std::uint32_t>(p0 >> 32) ^ counter[3] ^ key[1],
                   static_cast<std::uint32_t>(p0)};
    }
    return counter;
}

/**
 * @brief A stable 64-bit key for a series, taken from the source it publishes.
 *
 * The source name is unique within a tenant and stable across runs, so the key
 * is too. FNV-1a, so the key needs no dependency and is the same on every
 * platform.
 */
constexpr std::uint64_t series_key(std::string_view source_name) {
    std::uint64_t hash = 14695981039346656037ull;
    for (const char c : source_name) {
        hash ^= static_cast<std::uint8_t>(c);
        hash *= 1099511628211ull;
    }
    return hash;
}

/**
 * @brief What a stream's draws are for.
 *
 * One series draws its arrival gaps and its price moves from two streams, so a
 * change to how many draws one of them takes does not shift the other.
 */
enum class draw_purpose : std::uint32_t { arrival = 1, price = 2, market = 3 };

/**
 * @brief Names one stream of a run: whose draws, and for what.
 */
struct stream_key {
    std::uint64_t run_seed;
    std::uint64_t series;
    draw_purpose purpose;
};

/**
 * @brief A reproducible stream of random bits, keyed by run, series and purpose.
 *
 * The run seed is the Philox key, so two runs never share a stream. The series
 * and the purpose hash to a 64-bit stream id in the high half of the counter,
 * and the draw index fills the low half. Two pairs of series and purpose share
 * a range only if their ids collide, with a chance of about 2^-64 a pair. A
 * stream's draws depend on its key alone: not on the thread that draws them,
 * nor on how many other streams exist or the order they were made in.
 *
 * Satisfies UniformRandomBitGenerator, so the standard distributions take it.
 * A copy replays the draws of its original, so pass a stream by reference.
 */
class rng_stream final {
public:
    using result_type = std::uint64_t;

    constexpr explicit rng_stream(const stream_key& key)
        : key_{static_cast<std::uint32_t>(key.run_seed),
               static_cast<std::uint32_t>(key.run_seed >> 32)}
        , stream_id_(mix(key.series ^ mix(static_cast<std::uint64_t>(key.purpose)))) {}

    static constexpr result_type min() {
        return 0;
    }

    static constexpr result_type max() {
        return std::numeric_limits<result_type>::max();
    }

    constexpr result_type operator()() {
        if (draws_ % 2 == 0)
            block_ = philox4x32_10({static_cast<std::uint32_t>(draws_ / 2),
                                    static_cast<std::uint32_t>(draws_ / 2 >> 32),
                                    static_cast<std::uint32_t>(stream_id_),
                                    static_cast<std::uint32_t>(stream_id_ >> 32)},
                                   key_);
        const auto lane = static_cast<std::size_t>(draws_ % 2) * 2;
        ++draws_;
        return static_cast<std::uint64_t>(block_[lane]) |
               static_cast<std::uint64_t>(block_[lane + 1]) << 32;
    }

    /// How many values the stream has handed out, so a trace can say where it is.
    [[nodiscard]] constexpr std::uint64_t draws() const {
        return draws_;
    }

private:
    /// SplitMix64's finaliser, so nearby series keys land far apart.
    static constexpr std::uint64_t mix(std::uint64_t x) {
        x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ull;
        x = (x ^ (x >> 27)) * 0x94D049BB133111EBull;
        return x ^ (x >> 31);
    }

    std::array<std::uint32_t, 2> key_;
    std::uint64_t stream_id_;
    std::uint64_t draws_ = 0;
    std::array<std::uint32_t, 4> block_{};
};

}

#endif
