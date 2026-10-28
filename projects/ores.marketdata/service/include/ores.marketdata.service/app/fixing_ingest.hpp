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
#ifndef ORES_MARKETDATA_SERVICE_APP_FIXING_INGEST_HPP
#define ORES_MARKETDATA_SERVICE_APP_FIXING_INGEST_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <span>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief One fixing an adapter published, mapped to the index it fixes.
 */
struct fixing final {
    /// The oresmd URI of the index, which is the identity of its series.
    std::string index_uri;
    std::chrono::year_month_day date;
    /// The value as decimal text, as the publisher wrote it.
    std::string value;
    /// The name of the source that published it.
    std::string source;
};

/**
 * @brief The values a series already holds, by date.
 */
using stored_fixings = std::map<std::chrono::year_month_day, std::string>;

/**
 * @brief Whether @p f adds anything to @p stored: a date the series lacks, or a new value
 * for a date it has. A backfill sent again after a restart adds nothing.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT bool adds_to(const stored_fixings& stored,
                                                          const fixing& f);

/**
 * @brief Writes fixings into the series of each consumer of their source.
 *
 * A fixing is a durable fact, kept for good, so it goes to the fixings table rather than
 * the tick table. The series of an index is found or created for each consumer, as the
 * ingest does for a quote. One thread uses a store, so it holds its caches without a lock.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fixing_store {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.fixing_store");
        return instance;
    }

public:
    explicit fixing_store(ores::database::context ctx);

    /**
     * @brief Stores each fixing for each consumer, and returns the rows written.
     */
    std::size_t store(std::span<const consumer> consumers, std::span<const fixing> fixings);

private:
    using series_key = std::tuple<std::string, boost::uuids::uuid, std::string, std::string>;

    boost::uuids::uuid series_for(const ores::database::context& tenant_ctx,
                                  const consumer& owner,
                                  const std::string& index_uri);
    stored_fixings& stored_in(const ores::database::context& tenant_ctx,
                              const boost::uuids::uuid& series_id);

    ores::database::context ctx_;
    boost::uuids::random_generator uuid_gen_;
    std::map<series_key, boost::uuids::uuid> series_;
    std::map<boost::uuids::uuid, stored_fixings> stored_;
    std::map<std::string, std::shared_ptr<const core::series_classifier>> classifiers_;
};

/**
 * @brief The path of the fixings through the ingest: a queue that never sheds, and one
 * thread that stores what it holds.
 *
 * The quotes' queue drops its oldest tick when it is full and its worker drops a tick
 * that has aged, both right for a perishable quote. A fixing is neither, so it takes this
 * path instead. A daily volume is small, and a backfill waits in the queue rather than
 * being lost.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fixing_ingest {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.fixing_ingest");
        return instance;
    }

public:
    /// The consumers of a source, read when its fixings are stored.
    using consumers_of = std::function<std::vector<consumer>(const std::string& source)>;

    fixing_ingest(ores::database::context ctx, consumers_of consumers);
    ~fixing_ingest();

    fixing_ingest(const fixing_ingest&) = delete;
    fixing_ingest& operator=(const fixing_ingest&) = delete;

    void push(fixing f);

    /// The rows written since the start.
    [[nodiscard]] std::uint64_t stored() const;
    /// The fixings that failed to store since the start.
    [[nodiscard]] std::uint64_t failed() const;
    /// The fixings that wait to be stored.
    [[nodiscard]] std::size_t pending() const;

private:
    void run();

    fixing_store store_;
    consumers_of consumers_;
    mutable std::mutex mu_;
    std::condition_variable ready_;
    std::deque<fixing> pending_;
    bool stopping_ = false;
    std::atomic<std::uint64_t> stored_{0};
    std::atomic<std::uint64_t> failed_{0};
    std::thread thread_;
};

}

#endif
