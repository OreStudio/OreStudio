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
#ifndef ORES_SYNTHETIC_SERVICE_RUNNING_SET_HPP
#define ORES_SYNTHETIC_SERVICE_RUNNING_SET_HPP

#include <cstddef>
#include <mutex>
#include <set>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The source names of the feeds the service is running, and the claim on each.
 *
 * A runner claims a feed's source name before it launches the generator, so two runs never
 * publish one feed, and it releases the name only once that generator has stopped, so the
 * set never names a generator that is gone. A reader takes the whole set under one lock, so
 * a screen reads a run either started or stopped and never half way.
 *
 * The set holds the name and not the run, so it answers what the service publishes without
 * knowing which of its runners publishes it.
 */
class running_set final {
public:
    /** @brief Claims one feed's source name. False when another run holds it. */
    bool claim(const std::string& source_name) {
        std::lock_guard lock(mu_);
        return names_.insert(source_name).second;
    }

    /** @brief Gives one feed's source name back, once its generator has stopped. */
    void release(const std::string& source_name) {
        std::lock_guard lock(mu_);
        names_.erase(source_name);
    }

    /** @brief Whether a generator of this feed runs now. */
    [[nodiscard]] bool contains(const std::string& source_name) const {
        std::lock_guard lock(mu_);
        return names_.contains(source_name);
    }

    /** @brief Every source name that runs now, in name order. */
    [[nodiscard]] std::vector<std::string> names() const {
        std::lock_guard lock(mu_);
        return {names_.begin(), names_.end()};
    }

    /** @brief How many feeds run now. */
    [[nodiscard]] std::size_t size() const {
        std::lock_guard lock(mu_);
        return names_.size();
    }

private:
    mutable std::mutex mu_;
    std::set<std::string> names_;
};

}

#endif
