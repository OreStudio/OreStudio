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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_SCHEDULER_TRACE_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_SCHEDULER_TRACE_HPP

#include "ores.analytics.quant/domain/operational_clock.hpp"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace ores::analytics::quant::service {

/// What the loop decided.
enum class trace_kind : std::uint8_t { tick, transition, pin, unpin, add, remove };

/// The name a reader sees for a kind.
constexpr std::string_view to_string(trace_kind k) {
    switch (k) {
        case trace_kind::tick:
            return "tick";
        case trace_kind::transition:
            return "transition";
        case trace_kind::pin:
            return "pin";
        case trace_kind::unpin:
            return "unpin";
        case trace_kind::add:
            return "add";
        case trace_kind::remove:
            return "remove";
    }
    return "unknown";
}

/**
 * @brief One decision of the loop, stamped with when it happened in both times.
 *
 * Trivially copyable, so the ring stores it by value and a record is one copy.
 */
struct trace_event final {
    trace_kind kind = trace_kind::tick;
    /// The series key, for a tick, an add or a remove; zero for a market event.
    std::uint64_t series = 0;
    /// Seconds since the run started.
    double at = 0.0;
    /// The market's operational time.
    double tau = 0.0;
    /// For a tick: the gap the series' law drew, in its own operational time.
    double operational_gap = 0.0;
    /// For a tick: the wall seconds since the series last ticked.
    double wall_gap = 0.0;
    /// For a tick: the law's intensity integrated over the gap, which is Exp(1) for a
    /// stream that follows its law.
    double compensated_gap = 0.0;
    /// The heat after the decision.
    double heat = 0.0;
    /// The regime's multiplier after the decision; the pace less it is the heat's share.
    double multiplier = 1.0;
    /// How fast market time runs after the decision.
    double pace = 1.0;
    /// The market state after the decision.
    domain::market_state state = domain::market_state::calm;
};

/// Whether an event belongs to the market every series shares, rather than to one series.
constexpr bool is_market_event(const trace_event& e) {
    return e.kind == trace_kind::transition || e.kind == trace_kind::pin ||
           e.kind == trace_kind::unpin;
}

/**
 * @brief The last N decisions of the loop, oldest overwritten first.
 *
 * Its storage is allocated once, so recording never allocates. The loop records
 * while it already holds its own lock, and a reader copies under that lock.
 */
class trace_ring final {
public:
    explicit trace_ring(std::size_t capacity)
        : slots_(capacity) {
        if (capacity == 0)
            throw std::invalid_argument("trace_ring: capacity must be positive");
    }

    void record(const trace_event& e) {
        slots_[written_ % slots_.size()] = e;
        ++written_;
    }

    /**
     * @brief The newest @p limit events @p keep accepts, oldest first.
     *
     * Walks back from the newest event and stops at the limit, so a capped read costs the
     * events it returns and not the whole ring.
     */
    template <class Predicate>
    [[nodiscard]] std::vector<trace_event>
    copy_if(Predicate keep, std::size_t limit = std::numeric_limits<std::size_t>::max()) const {
        std::vector<trace_event> out;
        const auto held = std::min<std::uint64_t>(written_, slots_.size());
        for (auto i = written_; i > written_ - held && out.size() < limit; --i) {
            const auto& e = slots_[(i - 1) % slots_.size()];
            if (keep(e))
                out.push_back(e);
        }
        std::ranges::reverse(out);
        return out;
    }

    /// How many events were ever recorded, including the ones overwritten.
    [[nodiscard]] std::uint64_t written() const {
        return written_;
    }

    [[nodiscard]] std::size_t capacity() const {
        return slots_.size();
    }

private:
    std::vector<trace_event> slots_;
    std::uint64_t written_ = 0;
};

}

#endif
