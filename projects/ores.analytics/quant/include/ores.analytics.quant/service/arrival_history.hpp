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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_ARRIVAL_HISTORY_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_ARRIVAL_HISTORY_HPP

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <stdexcept>
#include <vector>

namespace ores::analytics::quant::service {

/// One slot of the history: the ticks of each group in it, and the market it ran under.
struct arrival_bin final {
    /// The slot's index: its start is the index times the slot's length, in seconds of the run.
    std::int64_t slot = 0;
    /// The ticks of each group, by the group's index; a group that joined later is missing.
    std::vector<std::uint32_t> counts;
    /// The sums of the pace and of being active over the samples taken in the slot.
    double pace_sum = 0.0;
    double active_sum = 0.0;
    std::uint32_t samples = 0;
};

/**
 * @brief The ticks of the last stretch of a run, counted by slot and by group.
 *
 * A count rather than a list of ticks, so it costs the same at any tick rate: a screen folds
 * it into a temporal raster, a column a stretch of time and a cell a slot of it. A slot with
 * nothing in it is not stored, so a quiet run costs nothing.
 *
 * Not thread-safe; the scheduler fills and reads it under its own lock.
 */
class arrival_history final {
public:
    arrival_history(double slot_seconds, std::size_t capacity)
        : slot_seconds_(slot_seconds)
        , capacity_(capacity) {
        if (!(slot_seconds > 0.0) || capacity == 0)
            throw std::invalid_argument(
                "arrival_history: the slot and the capacity must be positive");
    }

    [[nodiscard]] double slot_seconds() const {
        return slot_seconds_;
    }

    /// A tick of group @p group at @p at seconds of the run.
    void tick(double at, std::size_t group) {
        auto& counts = bin_at(at).counts;
        if (counts.size() <= group)
            counts.resize(group + 1, 0);
        ++counts[group];
    }

    /// A reading of the market at @p at seconds of the run.
    void sample(double at, double pace, bool active) {
        auto& bin = bin_at(at);
        bin.pace_sum += pace;
        bin.active_sum += active ? 1.0 : 0.0;
        ++bin.samples;
    }

    /// The slots that start at @p from seconds of the run or later, oldest first.
    [[nodiscard]] std::vector<arrival_bin> since(double from) const {
        const auto first = slot_of(from);
        std::vector<arrival_bin> out;
        for (const auto& bin : bins_)
            if (bin.slot >= first)
                out.push_back(bin);
        return out;
    }

private:
    [[nodiscard]] std::int64_t slot_of(double at) const {
        return static_cast<std::int64_t>(std::floor(at / slot_seconds_));
    }

    arrival_bin& bin_at(double at) {
        const auto slot = slot_of(at);
        // The loop moves forward, so a slot is the last one or a new one; one from the past
        // is found among the few kept since.
        for (auto it = bins_.rbegin(); it != bins_.rend(); ++it) {
            if (it->slot == slot)
                return *it;
            if (it->slot < slot)
                break;
        }
        if (bins_.empty() || bins_.back().slot < slot) {
            bins_.push_back(arrival_bin{.slot = slot});
            // The capacity is a span of slots, not a count of stored ones, so a quiet stretch
            // ages out as a busy one does.
            while (bins_.front().slot <= slot - static_cast<std::int64_t>(capacity_))
                bins_.pop_front();
            return bins_.back();
        }
        auto it = bins_.begin();
        while (it != bins_.end() && it->slot < slot)
            ++it;
        return *bins_.insert(it, arrival_bin{.slot = slot});
    }

    double slot_seconds_;
    std::size_t capacity_;
    std::deque<arrival_bin> bins_;
};

}

#endif
