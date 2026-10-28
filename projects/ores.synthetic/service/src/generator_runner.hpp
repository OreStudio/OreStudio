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
#include "running_set.hpp"
#include <algorithm>
#include <exception>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
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
 *
 * @c group names the feed the id belongs to. A feed that quotes one instrument leaves it empty and
 * the id stands alone. A curve quotes one id for each of its pillars and every one of them names
 * the curve, so the curve runs once however many of its pillars the demand names. Every offer of
 * one group builds the same generator, whichever of its ids started it.
 */
struct generator_offer final {
    ores::marketdata::messaging::vendor_id id;
    std::string group;
    std::function<std::shared_ptr<ores::marketdata::domain::i_feed>()> make;
};

/** What the runner did with one demand set. */
struct demand_outcome final {
    std::vector<ores::marketdata::messaging::vendor_id> accepted;
    std::vector<ores::marketdata::messaging::rejected_id> rejected;
    std::size_t started = 0;
    std::size_t stopped = 0;
};

/** One feed the runner runs now, and where its run started. */
struct running_feed_start final {
    std::string source_name;
    /** Absent for a kind that starts from no published value. */
    std::optional<ores::marketdata::domain::feed_start> start;
};

/**
 * @brief Runs one generator for each feed in the demand set, and the feeds an operator starts.
 *
 * The demand set is the whole set and it replaces the last one: the runner starts the generators
 * that the set adds, keeps the ones it still names and stops the ones it drops. A feed has at most
 * one generator, and a feed whose group holds several ids runs while any of them is demanded. A
 * vendor instrument has at most one generator. When two offers name the same instrument the first
 * offer wins and the second is not built, so two generators never publish one instrument. An id
 * with no offer is rejected as @c unknown_id, and an id whose generator cannot be built is
 * rejected as @c generator_failed, as is every other id of the same feed.
 *
 * A manual run is the operator's test control: @c start_manual runs one feed, keyed by the id of
 * its config, and no demand names it, so the next demand set leaves it running. A demand that
 * names its group takes it over, and reconciles it like every other demanded group from then on.
 *
 * The runner knows nothing of NATS or of the database. The offers come from a function the caller
 * gives, and the runner reads it on each demand, so a changed config takes effect at the next set.
 * The runner claims each feed's source name in the @c running_set it is given before it launches
 * its generator, so a feed that another run already publishes starts no second generator.
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

    explicit generator_runner(offers_fn offers, running_set* names = nullptr)
        : offers_(std::move(offers))
        , names_(names) {}

    ~generator_runner() {
        stop_all();
    }

    generator_runner(const generator_runner&) = delete;
    generator_runner& operator=(const generator_runner&) = delete;

    /** The ids the runner can serve now, each once. */
    [[nodiscard]] std::vector<ores::marketdata::messaging::vendor_id> universe() const {
        std::vector<ores::marketdata::messaging::vendor_id> ids;
        for (const auto& offer : offers_()) {
            const auto k = key_of(offer.id);
            if (std::ranges::find(ids, k, key_of) == ids.end())
                ids.push_back(offer.id);
        }
        return ids;
    }

    /** Makes the running generators agree with @p demand. */
    demand_outcome apply(const std::vector<ores::marketdata::messaging::vendor_id>& demand) {
        using ores::marketdata::messaging::rejected_id;
        using ores::marketdata::messaging::vendor_id;

        const auto offers = index_offers();
        demand_outcome out;
        std::vector<running_generator> dropped;
        std::unique_lock lock(mu_);
        std::set<key> wanted;
        std::set<std::string> wanted_feeds;
        // A feed that cannot be built is not built again for its other ids in this set.
        std::set<std::string> failed;
        // A feed a manual run already publishes starts no second generator in this set.
        std::set<std::string> served_elsewhere;
        for (const auto& id : demand) {
            const auto k = key_of(id);
            if (!wanted.insert(k).second)
                continue;
            const auto offer = offers.a_feed_of.find(k);
            if (offer == offers.a_feed_of.end()) {
                out.rejected.push_back(rejected_id{id, "unknown_id"});
                continue;
            }
            const auto& feed = offer->second;
            wanted_feeds.insert(feed);
            if (running_.contains(feed)) {
                // A demanded feed the operator started runs on until a demand stops naming it.
                running_.at(feed).manual = false;
                out.accepted.push_back(id);
                continue;
            }
            if (served_elsewhere.contains(feed)) {
                out.accepted.push_back(id);
                continue;
            }
            if (failed.contains(feed)) {
                out.rejected.push_back(rejected_id{id, "generator_failed"});
                continue;
            }
            try {
                const auto& make = offers.feeds.at(feed).offer.make;
                auto generator = make();
                if (!generator)
                    throw std::runtime_error("The generator is not built.");
                const auto source_name = generator->source_name();
                if (!claim(source_name)) {
                    BOOST_LOG_SEV(lg(), ores::logging::info)
                        << "The demand of " << id.external_id << " names the feed " << source_name
                        << ", which a manual run already publishes. It runs once.";
                    served_elsewhere.insert(feed);
                    out.accepted.push_back(id);
                    continue;
                }
                running_generator started;
                started.feed = std::move(generator);
                started.source_name = source_name;
                try {
                    started.thread = launch(started.feed.get());
                } catch (...) {
                    release(source_name);
                    throw;
                }
                running_.emplace(feed, std::move(started));
                out.accepted.push_back(id);
                ++out.started;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), ores::logging::warn)
                    << "The generator of " << id.external_id << " cannot start: " << e.what();
                failed.insert(feed);
                out.rejected.push_back(rejected_id{id, "generator_failed"});
            }
        }
        for (auto it = running_.begin(); it != running_.end();) {
            if (wanted_feeds.contains(it->first) || it->second.manual) {
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

    /**
     * @brief Starts the feed an operator asked for, keyed by the id of its config.
     *
     * A manual run builds the feed as a producer, with no adapter target, and no demand names its
     * group, so the next demand set leaves it running. Returns false, and starts nothing, when a
     * run of the feed's source name is already publishing it, whichever run started it.
     */
    bool start_manual(std::string group, std::shared_ptr<ores::marketdata::domain::i_feed> feed) {
        if (!feed)
            throw std::invalid_argument("A manual run needs a feed to run.");
        const auto source_name = feed->source_name();
        running_generator started;
        started.feed = std::move(feed);
        started.source_name = source_name;
        started.manual = true;
        {
            std::lock_guard lock(mu_);
            if (running_.contains(group))
                return false;
            if (!claim(source_name))
                return false;
            try {
                started.thread = launch(started.feed.get());
            } catch (...) {
                release(source_name);
                throw;
            }
            running_.emplace(std::move(group), std::move(started));
        }
        BOOST_LOG_SEV(lg(), ores::logging::info) << "Manual start of " << source_name << "; "
                                                 << running_count() << " generator(s) running";
        return true;
    }

    /** Stops the run of the feed with this source name. Returns 1 when it found one. */
    std::size_t stop_source(const std::string& source_name) {
        running_generator stopped;
        {
            std::lock_guard lock(mu_);
            const auto found = std::ranges::find_if(running_, [&](const auto& entry) {
                return entry.second.source_name == source_name;
            });
            if (found == running_.end())
                return 0;
            stopped = std::move(found->second);
            running_.erase(found);
        }
        halt(stopped);
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "Stopped " << source_name << "; " << running_count() << " generator(s) running";
        return 1;
    }

    /** Stops every generator, and answers how many it stopped. */
    std::size_t stop_all() {
        std::vector<running_generator> stopped;
        {
            std::lock_guard lock(mu_);
            stopped.reserve(running_.size());
            for (auto& [_, g] : running_)
                stopped.push_back(std::move(g));
            running_.clear();
        }
        for (auto& g : stopped)
            halt(g);
        return stopped.size();
    }

    [[nodiscard]] std::size_t running_count() const {
        std::lock_guard lock(mu_);
        return running_.size();
    }

    /**
     * @brief The feeds this runner runs now, each with where its run started.
     *
     * The start is the one the generator recorded when it was built, so it names the
     * observation this run took and not the one a later run would take.
     */
    [[nodiscard]] std::vector<running_feed_start> running_feeds() const {
        std::lock_guard lock(mu_);
        std::vector<running_feed_start> out;
        out.reserve(running_.size());
        for (const auto& [_, g] : running_) {
            if (!g.feed)
                continue;
            out.push_back({.source_name = g.source_name, .start = g.feed->start_source()});
        }
        return out;
    }

private:
    using key = std::tuple<std::string, std::string, std::string>;

    struct running_generator {
        std::shared_ptr<ores::marketdata::domain::i_feed> feed;
        std::string source_name;
        /** An operator started it, and no demand has named its group since. */
        bool manual = false;
        std::thread thread;
    };

    static key key_of(const ores::marketdata::messaging::vendor_id& id) {
        return {id.id_scheme, id.external_id, id.field};
    }

    /** One feed's offers: the generator it builds and the ids it publishes. */
    struct feed_offers {
        generator_offer offer;
        std::vector<key> ids;
    };

    /** The offers read one way: which feed serves an id, and what each feed serves. */
    struct offer_index {
        std::map<key, std::string> a_feed_of;
        std::map<std::string, feed_offers> feeds;
    };

    // The feed an offer belongs to. An offer that names none stands alone.
    static std::string feed_of(const generator_offer& offer) {
        if (!offer.group.empty())
            return offer.group;
        return "id:" + std::get<0>(key_of(offer.id)) + "/" + std::get<1>(key_of(offer.id)) + "/" +
               std::get<2>(key_of(offer.id));
    }

    // One offer for each instrument. The first one in the order the function gave wins, and it
    // decides the feed the instrument belongs to.
    offer_index index_offers() const {
        offer_index out;
        for (const auto& offer : offers_()) {
            const auto k = key_of(offer.id);
            const auto feed = feed_of(offer);
            if (!out.a_feed_of.emplace(k, feed).second) {
                BOOST_LOG_SEV(lg(), ores::logging::warn)
                    << "A second generator is offered for " << offer.id.external_id
                    << ". It is not used.";
                continue;
            }
            auto& group = out.feeds[feed];
            if (group.ids.empty())
                group.offer = offer;
            group.ids.push_back(k);
        }
        return out;
    }

    static std::thread launch(ores::marketdata::domain::i_feed* raw) {
        return std::thread([raw] {
            try {
                raw->start();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), ores::logging::error)
                    << "The generator of " << raw->source_name() << " stopped: " << e.what();
            }
        });
    }

    // The name is released only after the generator has stopped, so the set never names a
    // generator that is already gone.
    void halt(running_generator& g) {
        if (g.feed)
            g.feed->stop();
        if (g.thread.joinable())
            g.thread.join();
        release(g.source_name);
    }

    bool claim(const std::string& source_name) {
        return names_ == nullptr || names_->claim(source_name);
    }

    void release(const std::string& source_name) {
        if (names_ != nullptr)
            names_->release(source_name);
    }

    offers_fn offers_;
    running_set* names_;
    mutable std::mutex mu_;
    /// The running generator of each feed, by the feed's own name.
    std::map<std::string, running_generator> running_;
};

}

#endif
