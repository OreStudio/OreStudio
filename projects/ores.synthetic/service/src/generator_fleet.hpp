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
#ifndef ORES_SYNTHETIC_SERVICE_GENERATOR_FLEET_HPP
#define ORES_SYNTHETIC_SERVICE_GENERATOR_FLEET_HPP

#include "generator_runner.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "running_set.hpp"
#include <cstddef>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Every generator the service runs, demanded or manual, and the names they publish.
 *
 * The runner the operator drives belongs to the fleet, and each adapter takes its own runner
 * into it, so one place answers what runs now and one stop reaches a feed whichever run holds
 * it. The running set is the union of the demanded runs and the manual ones: a feed reads as
 * running for the screens whatever started it.
 */
class generator_fleet final {
public:
    generator_fleet() = default;
    ~generator_fleet() = default;

    generator_fleet(const generator_fleet&) = delete;
    generator_fleet& operator=(const generator_fleet&) = delete;

    /** The set every runner of the service claims the feeds it runs in. */
    [[nodiscard]] running_set& running_names() {
        return names_;
    }

    /** The source names of the feeds the service runs now. */
    [[nodiscard]] std::vector<std::string> running_source_names() const {
        return names_.names();
    }

    /** How many feeds the service runs now. */
    [[nodiscard]] std::size_t running_count() const {
        return names_.size();
    }

    /**
     * @brief Starts the feed an operator asked for, keyed by the id of its config.
     *
     * Returns false, and starts nothing, when a run of the feed's source name already
     * publishes it.
     */
    bool start_manual(std::string feed_config_id,
                      std::shared_ptr<ores::marketdata::domain::i_feed> feed) {
        return manual_.start_manual(std::move(feed_config_id), std::move(feed));
    }

    /** Stops the feed with this source name, whichever run of the fleet holds it. */
    std::size_t stop_source(const std::string& source_name) {
        std::size_t stopped = manual_.stop_source(source_name);
        for (auto* runner : attached())
            stopped += runner->stop_source(source_name);
        return stopped;
    }

    /** Stops every run of the fleet, demanded and manual alike, and answers how many. */
    std::size_t stop_all() {
        std::size_t stopped = manual_.stop_all();
        for (auto* runner : attached())
            stopped += runner->stop_all();
        return stopped;
    }

    /** Takes an adapter's runner into the fleet, so its runs are read and stopped here. */
    void attach(generator_runner& runner) {
        std::lock_guard lock(mu_);
        attached_.push_back(&runner);
    }

    /** Gives an adapter's runner back, before the runner is destroyed. */
    void detach(generator_runner& runner) {
        std::lock_guard lock(mu_);
        std::erase(attached_, &runner);
    }

private:
    std::vector<generator_runner*> attached() const {
        std::lock_guard lock(mu_);
        return attached_;
    }

    mutable std::mutex mu_;
    std::vector<generator_runner*> attached_;
    running_set names_;
    // The runner the operator drives. No generator is offered to it, because a manual run is
    // built from a feed's config by the verb that asks for it.
    generator_runner manual_{[] { return std::vector<generator_offer>{}; }, &names_};
};

}

#endif
