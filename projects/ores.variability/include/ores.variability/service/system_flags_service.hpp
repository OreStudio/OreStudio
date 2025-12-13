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
#ifndef ORES_VARIABILITY_SERVICE_SYSTEM_FLAGS_SERVICE_HPP
#define ORES_VARIABILITY_SERVICE_SYSTEM_FLAGS_SERVICE_HPP

#include <string_view>
#include "ores.utility/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.variability/domain/system_flags.hpp"
#include "ores.variability/domain/system_flags_cache.hpp"
#include "ores.variability/service/feature_flags_service.hpp"

namespace ores::variability::service {

/**
 * @brief Service for accessing well-known system flags with type-safe methods.
 *
 * This service provides a typed interface for querying and modifying system
 * flags. Unlike feature_flags_service which works with arbitrary flag names,
 * this service only handles the predefined system flags enumerated in
 * domain::system_flag.
 *
 * The service maintains an internal cache of flag values for fast access.
 * The cache is populated on first access or via explicit refresh(). Write
 * operations automatically update the cache after persisting to database.
 *
 * The service wraps feature_flags_service internally and translates between
 * the system_flag enum and the underlying string-based storage.
 */
class system_flags_service {
private:
    inline static std::string_view logger_name =
        "ores.variability.service.system_flags_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a system_flags_service.
     *
     * @param ctx The database context to be used by the underlying repository.
     */
    explicit system_flags_service(database::context ctx);

    /**
     * @brief Refreshes the cache by reading all system flags from the database.
     *
     * Call this at startup after flag_initializer has run, or when you need
     * to ensure the cache reflects the latest database state.
     */
    void refresh();

    /**
     * @brief Gets a const reference to the cached system flags.
     *
     * This provides fast, read-only access to the cached flag values without
     * database queries. The cache must be populated via refresh() first.
     *
     * @return Const reference to the cached system flags.
     */
    [[nodiscard]] const domain::system_flags_cache& cache() const;

    /**
     * @brief Checks if a system flag is enabled (from cache).
     *
     * Returns the cached value. Call refresh() first to populate the cache.
     *
     * @param flag The system flag to check.
     * @return true if the flag is enabled, false otherwise.
     */
    [[nodiscard]] bool is_enabled(domain::system_flag flag) const;

    /**
     * @brief Sets a system flag's enabled state.
     *
     * Persists to database and updates the cache.
     *
     * @param flag The system flag to modify.
     * @param enabled The new enabled state.
     * @param modified_by Username of the user making the change.
     */
    void set_enabled(domain::system_flag flag, bool enabled,
        std::string_view modified_by);

    // Convenience methods for specific flags

    /**
     * @brief Checks if the system is in bootstrap mode (from cache).
     *
     * @return true if bootstrap mode is enabled, false otherwise.
     */
    [[nodiscard]] bool is_bootstrap_mode_enabled() const;

    /**
     * @brief Sets the bootstrap mode state.
     *
     * Persists to database and updates the cache.
     *
     * @param enabled The new bootstrap mode state.
     * @param modified_by Username of the user making the change.
     */
    void set_bootstrap_mode(bool enabled, std::string_view modified_by);

    /**
     * @brief Checks if user self-registration (signups) is enabled (from cache).
     *
     * @return true if user signups are enabled, false otherwise.
     */
    [[nodiscard]] bool is_user_signups_enabled() const;

    /**
     * @brief Sets the user signups state.
     *
     * Persists to database and updates the cache.
     *
     * @param enabled The new user signups state.
     * @param modified_by Username of the user making the change.
     */
    void set_user_signups(bool enabled, std::string_view modified_by);

private:
    /**
     * @brief Updates a specific flag in the cache.
     */
    void update_cache(domain::system_flag flag, bool enabled);

    feature_flags_service feature_flags_service_;
    domain::system_flags_cache cache_;
};

}

#endif
