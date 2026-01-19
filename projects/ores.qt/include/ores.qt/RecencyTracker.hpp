/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_RECENCY_TRACKER_HPP
#define ORES_QT_RECENCY_TRACKER_HPP

#include <chrono>
#include <string>
#include <unordered_set>
#include <QDateTime>

#ifdef _MSC_VER
#define ORES_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#else
#define ORES_NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif

namespace ores::qt {

/**
 * @brief Default timestamp extractor that accesses .recorded_at member.
 */
template<typename Entity>
struct default_timestamp_extractor {
    auto operator()(const Entity& e) const {
        return e.recorded_at;
    }
};

/**
 * @brief Tracks recently-modified records for recency highlighting.
 *
 * This template class provides a reusable way to detect which records have been
 * modified since the last reload. It compares each record's recorded_at timestamp
 * against the last reload time and tracks their identifiers.
 *
 * @tparam Entity The entity type (e.g., catalog, currency)
 * @tparam KeyExtractor A callable that extracts the identifier from an entity.
 *         Signature: std::string(const Entity&)
 * @tparam TimestampExtractor A callable that extracts the recorded_at timestamp.
 *         Signature: std::chrono::system_clock::time_point(const Entity&)
 *         Defaults to accessing .recorded_at member.
 *
 * Usage:
 * @code
 * // Define extractor
 * auto keyFn = [](const auto& e) { return e.name; };
 *
 * // Create tracker (uses default recorded_at extractor)
 * RecencyTracker<Catalog, decltype(keyFn)> tracker(keyFn);
 *
 * // After loading data
 * tracker.update(catalogs);
 *
 * // In data() method
 * if (tracker.is_recent(catalog.name) && pulseManager_->is_pulse_on()) {
 *     return color_constants::stale_indicator;
 * }
 * @endcode
 */
template<typename Entity,
         typename KeyExtractor,
         typename TimestampExtractor = default_timestamp_extractor<Entity>>
class RecencyTracker {
public:
    /**
     * @brief Construct a RecencyTracker with custom key extractor.
     *
     * Uses the default timestamp extractor (entity.recorded_at).
     *
     * @param key_extractor Callable to extract identifier from entity
     */
    explicit RecencyTracker(KeyExtractor key_extractor)
        : key_extractor_(std::move(key_extractor)),
          timestamp_extractor_() {}

    /**
     * @brief Construct a RecencyTracker with custom key and timestamp extractors.
     *
     * @param key_extractor Callable to extract identifier from entity
     * @param timestamp_extractor Callable to extract recorded_at from entity
     */
    RecencyTracker(KeyExtractor key_extractor, TimestampExtractor timestamp_extractor)
        : key_extractor_(std::move(key_extractor)),
          timestamp_extractor_(std::move(timestamp_extractor)) {}

    /**
     * @brief Update the set of recent records by comparing timestamps.
     *
     * Compares each entity's recorded_at against the last reload time.
     * On first call, sets the baseline timestamp without marking anything recent.
     *
     * @param entities The collection of entities to check
     * @return true if any recent records were found, false otherwise
     */
    template<typename Container>
    bool update(const Container& entities) {
        recent_keys_.clear();

        const QDateTime now = QDateTime::currentDateTime();

        // First load: set baseline timestamp, no highlighting
        if (!last_reload_time_.isValid()) {
            last_reload_time_ = now;
            return false;
        }

        // Find entities with recorded_at newer than last reload
        for (const auto& entity : entities) {
            const auto recorded_at = timestamp_extractor_(entity);
            if (recorded_at == std::chrono::system_clock::time_point{}) {
                continue;
            }

            const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
                recorded_at.time_since_epoch()).count();
            QDateTime recorded_dt = QDateTime::fromMSecsSinceEpoch(msecs);

            if (recorded_dt.isValid() && recorded_dt > last_reload_time_) {
                recent_keys_.insert(key_extractor_(entity));
            }
        }

        last_reload_time_ = now;
        return !recent_keys_.empty();
    }

    /**
     * @brief Check if a record with the given key is recent.
     *
     * @param key The identifier to check
     * @return true if the record is in the recent set
     */
    [[nodiscard]] bool is_recent(const std::string& key) const {
        return recent_keys_.find(key) != recent_keys_.end();
    }

    /**
     * @brief Get the number of recent records.
     */
    [[nodiscard]] std::size_t recent_count() const {
        return recent_keys_.size();
    }

    /**
     * @brief Check if there are any recent records.
     */
    [[nodiscard]] bool has_recent() const {
        return !recent_keys_.empty();
    }

    /**
     * @brief Clear the recent records set.
     *
     * Call this when pulsing completes.
     */
    void clear() {
        recent_keys_.clear();
    }

    /**
     * @brief Reset the tracker completely.
     *
     * Clears recent keys and resets the last reload time.
     */
    void reset() {
        recent_keys_.clear();
        last_reload_time_ = QDateTime();
    }

private:
    KeyExtractor key_extractor_;
    ORES_NO_UNIQUE_ADDRESS TimestampExtractor timestamp_extractor_;
    std::unordered_set<std::string> recent_keys_;
    QDateTime last_reload_time_;
};

/**
 * @brief Helper function to create a RecencyTracker with type deduction.
 *
 * @param key_extractor Callable to extract identifier from entity
 * @return RecencyTracker with deduced types
 */
template<typename Entity, typename KeyExtractor>
auto make_recency_tracker(KeyExtractor key_extractor) {
    return RecencyTracker<Entity, KeyExtractor>(std::move(key_extractor));
}

/**
 * @brief Helper function to create a RecencyTracker with custom timestamp extractor.
 *
 * @param key_extractor Callable to extract identifier from entity
 * @param timestamp_extractor Callable to extract recorded_at from entity
 * @return RecencyTracker with deduced types
 */
template<typename Entity, typename KeyExtractor, typename TimestampExtractor>
auto make_recency_tracker(KeyExtractor key_extractor,
                          TimestampExtractor timestamp_extractor) {
    return RecencyTracker<Entity, KeyExtractor, TimestampExtractor>(
        std::move(key_extractor), std::move(timestamp_extractor));
}

}

#endif
