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
#ifndef ORES_SYNTHETIC_SERVICE_GENERATOR_RUNNER_HPP
#define ORES_SYNTHETIC_SERVICE_GENERATOR_RUNNER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include <algorithm>
#include <exception>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <stdexcept>
#include <string>
#include <thread>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief A generator the simulator can start for one vendor instrument.
 *
 * @c make builds the generator when the demand asks for it. It may throw, and the runner turns the
 * throw into a rejection of that id.
 */
struct generator_offer final {
    ores::marketdata::messaging::vendor_id id;
    std::function<std::shared_ptr<ores::marketdata::domain::IFeed>()> make;
};

/** What the runner did with one demand set. */
struct demand_outcome final {
    std::vector<ores::marketdata::messaging::vendor_id> accepted;
    std::vector<ores::marketdata::messaging::rejected_id> rejected;
    std::size_t started = 0;
    std::size_t stopped = 0;
};

/**
 * @brief Runs one generator for each vendor instrument in the demand set.
 *
 * The demand set is the whole set and it replaces the last one: the runner starts the generators
 * that the set adds, keeps the ones it still names and stops the ones it drops. A vendor
 * instrument has at most one generator. When two offers name the same instrument the first offer
 * wins and the second is not built, so two generators never publish one instrument. An id with no
 * offer is rejected as @c unknown_id, and an id whose generator cannot be built is rejected as
 * @c generator_failed.
 *
 * The runner knows nothing of NATS or of the database. The offers come from a function the caller
 * gives, and the runner reads it on each demand, so a changed config takes effect at the next set.
 */
class generator_runner final {
private:
    static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.synthetic.service.generator_runner");
        return instance;
    }

public:
    using offers_fn = std::function<std::vector<generator_offer>()>;

    explicit generator_runner(offers_fn offers)
        : offers_(std::move(offers)) {}

    ~generator_runner() {
        stop_all();
    }

    generator_runner(const generator_runner&) = delete;
    generator_runner& operator=(const generator_runner&) = delete;

    /** The ids the runner can serve now, each once. */
    [[nodiscard]] std::vector<ores::marketdata::messaging::vendor_id> universe() const {
        std::vector<ores::marketdata::messaging::vendor_id> ids;
        for (const auto& [_, offer] : first_offers())
            ids.push_back(offer.id);
        return ids;
    }

    /** Makes the running generators agree with @p demand. */
    demand_outcome apply(const std::vector<ores::marketdata::messaging::vendor_id>& demand) {
        using ores::marketdata::messaging::rejected_id;
        using ores::marketdata::messaging::vendor_id;

        const auto offers = first_offers();
        demand_outcome out;
        std::vector<running_generator> dropped;
        std::unique_lock lock(mu_);
        std::set<key> wanted;
        for (const auto& id : demand) {
            const auto k = key_of(id);
            if (!wanted.insert(k).second)
                continue;
            if (running_.contains(k)) {
                out.accepted.push_back(id);
                continue;
            }
            const auto offer = offers.find(k);
            if (offer == offers.end()) {
                out.rejected.push_back(rejected_id{id, "unknown_id"});
                continue;
            }
            try {
                auto feed = offer->second.make();
                if (!feed)
                    throw std::runtime_error("The generator is not built.");
                running_.emplace(k, launch(std::move(feed)));
                out.accepted.push_back(id);
                ++out.started;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), ores::logging::warn)
                    << "The generator of " << id.external_id << " cannot start: " << e.what();
                out.rejected.push_back(rejected_id{id, "generator_failed"});
            }
        }
        for (auto it = running_.begin(); it != running_.end();) {
            if (wanted.contains(it->first)) {
                ++it;
                continue;
            }
            dropped.push_back(std::move(it->second));
            it = running_.erase(it);
            ++out.stopped;
        }
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "Demand of " << demand.size() << " id(s): " << out.accepted.size() << " accepted, "
            << out.rejected.size() << " rejected, " << out.started << " started, " << out.stopped
            << " stopped; " << running_.size() << " generator(s) running";
        // A generator takes up to a tick period to stop. The lock is free while it does.
        lock.unlock();
        for (auto& g : dropped)
            halt(g);
        return out;
    }

    /** Stops every generator. */
    void stop_all() {
        std::lock_guard lock(mu_);
        for (auto& [_, g] : running_)
            halt(g);
        running_.clear();
    }

    [[nodiscard]] std::size_t running_count() const {
        std::lock_guard lock(mu_);
        return running_.size();
    }

private:
    using key = std::tuple<std::string, std::string, std::string>;

    struct running_generator {
        std::shared_ptr<ores::marketdata::domain::IFeed> feed;
        std::thread thread;
    };

    static key key_of(const ores::marketdata::messaging::vendor_id& id) {
        return {id.id_scheme, id.external_id, id.field};
    }

    // One offer for each instrument. The first one in the order the function gave wins.
    std::map<key, generator_offer> first_offers() const {
        std::map<key, generator_offer> out;
        for (auto& offer : offers_()) {
            const auto k = key_of(offer.id);
            if (!out.emplace(k, offer).second)
                BOOST_LOG_SEV(lg(), ores::logging::warn)
                    << "A second generator is offered for " << offer.id.external_id
                    << ". It is not used.";
        }
        return out;
    }

    static running_generator launch(std::shared_ptr<ores::marketdata::domain::IFeed> feed) {
        running_generator g;
        auto* raw = feed.get();
        g.feed = std::move(feed);
        g.thread = std::thread([raw] {
            try {
                raw->start();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), ores::logging::error)
                    << "The generator of " << raw->source_name() << " stopped: " << e.what();
            }
        });
        return g;
    }

    static void halt(running_generator& g) {
        if (g.feed)
            g.feed->stop();
        if (g.thread.joinable())
            g.thread.join();
    }

    offers_fn offers_;
    mutable std::mutex mu_;
    std::map<key, running_generator> running_;
};

}

#endif
