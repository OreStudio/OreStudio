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
#include "ores.marketdata.service/app/fixing_ingest.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.marketdata.service/app/feed_series.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <utility>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

/// How many fixings the thread takes from the queue at once.
constexpr std::size_t batch_limit = 2048;

/// The first wait before a batch that failed is tried again, and the longest.
constexpr auto retry_after = std::chrono::seconds(2);
constexpr auto retry_at_most = std::chrono::seconds(60);

}

bool adds_to(const stored_fixings& stored, const fixing& f) {
    const auto found = stored.find(f.date);
    return found == stored.end() || found->second != f.value;
}

fixing_store::fixing_store(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

boost::uuids::uuid fixing_store::series_for(const ores::database::context& tenant_ctx,
                                            const consumer& owner,
                                            const std::string& index_uri) {
    const auto party = boost::uuids::to_string(owner.party_id);
    const series_key key{
        owner.tenant_id.to_string(), owner.party_id, owner.provider_code, index_uri};
    if (const auto found = series_.find(key); found != series_.end())
        return found->second;

    repository::market_series_repository series_repo;
    auto existing =
        series_repo.read_latest_by_key(tenant_ctx, index_uri, owner.provider_code, party);
    if (existing.empty()) {
        const auto index = datum::oresmd_uri_codec::read_index(index_uri);
        if (!index)
            throw std::runtime_error("'" + index_uri + "' names no index: " + index.error());
        auto& classifier = classifiers_[owner.tenant_id.to_string()];
        if (!classifier)
            classifier = std::make_shared<const core::series_classifier>(
                repository::series_classification_rule_repository{}.read_latest(tenant_ctx));
        const auto cl = classifier->classify(core::classification_key_of(*index));
        BOOST_LOG_SEV(lg(), info) << "Creating fixing series " << index_uri << " for provider "
                                  << owner.provider_code;
        existing.push_back(create_feed_series(
            tenant_ctx, owner, uuid_gen_(), index_uri, cl, ctx_.service_account()));
    }
    const auto id = existing.front().id;
    series_.emplace(key, id);
    return id;
}

stored_fixings& fixing_store::stored_in(const ores::database::context& tenant_ctx,
                                        const boost::uuids::uuid& series_id) {
    auto [it, fresh] = stored_.try_emplace(series_id);
    if (fresh)
        for (auto& row :
             repository::market_fixing_repository{}.read_latest_for_series(tenant_ctx, series_id))
            it->second.insert_or_assign(row.fixing_date, std::move(row.value));
    return it->second;
}

std::size_t fixing_store::store(std::span<const consumer> consumers,
                                std::span<const fixing> fixings) {
    // The last value of each index and date, in the order the dates were first sent.
    std::vector<fixing> latest;
    std::map<std::pair<std::string, std::chrono::year_month_day>, std::size_t> at;
    for (const auto& f : fixings) {
        if (!datum::oresmd_uri_codec::read_index(f.index_uri)) {
            ++rejected_;
            BOOST_LOG_SEV(lg(), warn) << "Rejecting a fixing of '" << f.index_uri
                                      << "' from source '" << f.source << "': it names no index";
            continue;
        }
        const auto [it, fresh] = at.try_emplace({f.index_uri, f.date}, latest.size());
        if (fresh)
            latest.push_back(f);
        else
            latest[it->second] = f;
    }

    std::size_t written = 0;
    for (const auto& owner : consumers) {
        const auto tenant_ctx = ctx_.with_tenant(owner.tenant_id, "ores.marketdata.service");
        std::vector<domain::market_fixing> rows;
        for (const auto& f : latest) {
            const auto series_id = series_for(tenant_ctx, owner, f.index_uri);
            if (!adds_to(stored_in(tenant_ctx, series_id), f))
                continue;
            domain::market_fixing row;
            row.id = uuid_gen_();
            row.tenant_id = owner.tenant_id;
            row.party_id = owner.party_id;
            row.series_id = series_id;
            row.fixing_date = f.date;
            row.value = f.value;
            row.source = f.source;
            rows.push_back(std::move(row));
        }
        if (rows.empty())
            continue;
        repository::market_fixing_repository{}.insert(tenant_ctx, rows);
        for (const auto& row : rows)
            stored_[row.series_id].insert_or_assign(row.fixing_date, row.value);
        written += rows.size();
    }
    return written;
}

fixing_ingest::fixing_ingest(ores::database::context ctx, consumers_of consumers)
    : store_(std::move(ctx))
    , consumers_(std::move(consumers))
    , thread_([this] { run(); }) {}

fixing_ingest::~fixing_ingest() {
    {
        std::lock_guard lock(mu_);
        stopping_ = true;
    }
    ready_.notify_all();
    if (thread_.joinable())
        thread_.join();
}

void fixing_ingest::push(fixing f) {
    {
        std::lock_guard lock(mu_);
        pending_.push_back({std::move(f), 0});
    }
    ready_.notify_one();
}

std::uint64_t fixing_ingest::stored() const {
    return stored_.load(std::memory_order_relaxed);
}

std::uint64_t fixing_ingest::failed() const {
    return failed_.load(std::memory_order_relaxed);
}

std::uint64_t fixing_ingest::rejected() const {
    return rejected_.load(std::memory_order_relaxed);
}

std::size_t fixing_ingest::pending() const {
    std::lock_guard lock(mu_);
    return pending_.size();
}

void fixing_ingest::run() {
    auto backoff = retry_after;
    for (;;) {
        std::vector<queued> batch;
        {
            std::unique_lock lock(mu_);
            ready_.wait(lock, [this] { return stopping_ || !pending_.empty(); });
            if (pending_.empty())
                return;
            const auto n = std::min(pending_.size(), batch_limit);
            batch.assign(std::make_move_iterator(pending_.begin()),
                         std::make_move_iterator(pending_.begin() + static_cast<long>(n)));
            pending_.erase(pending_.begin(), pending_.begin() + static_cast<long>(n));
        }

        std::map<std::string, std::vector<queued>> by_source;
        for (auto& q : batch)
            by_source[q.value.source].push_back(std::move(q));
        std::vector<queued> retry;
        for (auto& [source, queue] : by_source) {
            std::vector<fixing> fixings;
            fixings.reserve(queue.size());
            for (const auto& q : queue)
                fixings.push_back(q.value);
            try {
                stored_.fetch_add(store_.store(consumers_(source), fixings),
                                  std::memory_order_relaxed);
                rejected_.store(store_.rejected(), std::memory_order_relaxed);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to store " << fixings.size() << " fixing(s) of source '" << source
                    << "': " << e.what();
                for (auto& q : queue) {
                    if (++q.attempts < max_attempts)
                        retry.push_back(std::move(q));
                    else
                        failed_.fetch_add(1, std::memory_order_relaxed);
                }
            }
        }
        if (retry.empty()) {
            backoff = retry_after;
            continue;
        }

        // The fixings that failed go back to the front, and the thread waits before it tries
        // again, so a store that is down is not hammered. A stop ends the wait.
        std::unique_lock lock(mu_);
        pending_.insert(pending_.begin(),
                        std::make_move_iterator(retry.begin()),
                        std::make_move_iterator(retry.end()));
        if (ready_.wait_for(lock, backoff, [this] { return stopping_; })) {
            failed_.fetch_add(pending_.size(), std::memory_order_relaxed);
            BOOST_LOG_SEV(lg(), warn)
                << "Stopping with " << pending_.size() << " fixing(s) that the store refused";
            return;
        }
        backoff = std::min(backoff * 2, retry_at_most);
    }
}

}
