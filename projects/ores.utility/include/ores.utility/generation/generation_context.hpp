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
#ifndef ORES_UTILITY_GENERATION_GENERATION_CONTEXT_HPP
#define ORES_UTILITY_GENERATION_GENERATION_CONTEXT_HPP

#include <memory>
#include "ores.utility/generation/generation_engine.hpp"
#include "ores.utility/generation/generation_environment.hpp"

namespace ores::utility::generation {

/**
 * @brief Combines a generation engine with a scoped environment.
 *
 * The engine is shared across parent and child contexts so random sequences
 * remain consistent. The environment supports parent-child scoping for
 * passing contextual data (tenant_id, modified_by, FK references) through
 * entity generation graphs.
 */
class generation_context final {
public:
    using entries = generation_environment::entries;

    /**
     * @brief Constructs a context with a specific seed and optional entries.
     */
    explicit generation_context(std::uint64_t seed, entries initial = {});

    /**
     * @brief Constructs a context with a random seed and optional entries.
     */
    explicit generation_context(entries initial = {});

    /**
     * @brief Creates a child context with the same engine but overridden env.
     */
    generation_context child(entries overrides) const;

    /**
     * @brief Returns the generation engine.
     */
    generation_engine& engine() { return *engine_; }

    /**
     * @brief Returns the generation environment.
     */
    const generation_environment& env() const { return *env_; }

    // Convenience delegations to engine
    std::uint64_t seed() const { return engine_->seed(); }
    boost::uuids::uuid generate_uuid() { return engine_->generate_uuid(); }
    int random_int(int min, int max) { return engine_->random_int(min, max); }
    bool random_bool(double probability = 0.5) {
        return engine_->random_bool(probability);
    }
    std::chrono::system_clock::time_point past_timepoint(int years_back = 3) {
        return engine_->past_timepoint(years_back);
    }
    std::string alphanumeric(std::size_t length) {
        return engine_->alphanumeric(length);
    }

    template<typename T>
    const T& pick(const std::vector<T>& items) {
        return engine_->pick(items);
    }

    template<typename T, std::size_t N>
    const T& pick(const std::array<T, N>& items) {
        return engine_->pick(items);
    }

private:
    generation_context(std::shared_ptr<generation_engine> engine,
        std::shared_ptr<const generation_environment> env);

    std::shared_ptr<generation_engine> engine_;
    std::shared_ptr<const generation_environment> env_;
};

}

#endif
